use crate::Span;
use crate::diagnostics::Diagnostics;
use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    KwExport,
    KwFunction,
    KwImport,
    KwFrom,
    KwReturn,
    KwLet,
    KwConst,
    KwIf,
    KwElse,
    KwWhile,
    KwFor,
    KwBreak,
    KwContinue,
    Ident,
    Number,
    BigInt,
    String,
    Colon,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    AndAnd,
    OrOr,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    Semicolon,
    Comma,
    Dot,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Plus,
    Minus,
    Star,
    Slash,
    Error,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn lex(source: &str, path: &Path, diags: &mut Diagnostics) -> Vec<Token> {
    let mut lexer = Lexer::new(source, path, diags);
    let mut tokens = Vec::new();
    loop {
        let t = lexer.next_token();
        let is_eof = t.kind == TokenKind::Eof;
        tokens.push(t);
        if is_eof {
            break;
        }
    }
    tokens
}

struct Lexer<'a, 'd> {
    src: &'a [u8],
    pos: usize,
    diags: &'d mut Diagnostics,
    path: std::path::PathBuf,
}

impl<'a, 'd> Lexer<'a, 'd> {
    fn new(source: &'a str, path: &Path, diags: &'d mut Diagnostics) -> Self {
        Self {
            src: source.as_bytes(),
            pos: 0,
            diags,
            path: path.to_path_buf(),
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_ws_and_comments();
        let start = self.pos;
        let Some(b) = self.peek_byte() else {
            return self.eof_token();
        };
        let kind = self.lex_token_kind(start, b);

        Token {
            kind,
            span: Span {
                start,
                end: self.pos,
            },
        }
    }

    fn eof_token(&self) -> Token {
        Token {
            kind: TokenKind::Eof,
            span: Span {
                start: self.pos,
                end: self.pos,
            },
        }
    }

    fn lex_token_kind(&mut self, start: usize, b: u8) -> TokenKind {
        // 2-char operators must be handled before `lex_single_char`.
        match b {
            b'+' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::PlusEqual;
                }
                self.pos += 1;
                return TokenKind::Plus;
            }
            b'-' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::MinusEqual;
                }
                self.pos += 1;
                return TokenKind::Minus;
            }
            b'*' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::StarEqual;
                }
                self.pos += 1;
                return TokenKind::Star;
            }
            b'/' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::SlashEqual;
                }
                self.pos += 1;
                return TokenKind::Slash;
            }
            b'=' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::EqualEqual;
                }
                self.pos += 1;
                return TokenKind::Equal;
            }
            b'!' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::BangEqual;
                }
                self.pos += 1;
                return TokenKind::Bang;
            }
            b'<' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::LessEqual;
                }
                self.pos += 1;
                return TokenKind::Less;
            }
            b'>' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return TokenKind::GreaterEqual;
                }
                self.pos += 1;
                return TokenKind::Greater;
            }
            b'&' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'&') {
                    self.pos += 2;
                    return TokenKind::AndAnd;
                }
                self.error_at(start, start + 1, "Unsupported character: '&'");
                self.pos += 1;
                return TokenKind::Error;
            }
            b'|' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'|') {
                    self.pos += 2;
                    return TokenKind::OrOr;
                }
                self.error_at(start, start + 1, "Unsupported character: '|'");
                self.pos += 1;
                return TokenKind::Error;
            }
            _ => {}
        }

        if let Some(kind) = lex_single_char(b) {
            self.pos += 1;
            return kind;
        }

        match b {
            b'"' | b'\'' => {
                if self.lex_string() {
                    TokenKind::String
                } else {
                    TokenKind::Error
                }
            }
            b'0'..=b'9' => self.lex_number_or_bigint(),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => self.lex_ident_or_keyword(),
            _ => {
                self.error_at(
                    start,
                    start + 1,
                    format!("Unsupported character: 0x{b:02X}"),
                );
                self.pos += 1;
                TokenKind::Error
            }
        }
    }

    fn lex_ident_or_keyword(&mut self) -> TokenKind {
        let start = self.pos;
        self.pos += 1;
        while let Some(b) = self.peek_byte() {
            if b.is_ascii_alphanumeric() || b == b'_' || b == b'$' {
                self.pos += 1;
            } else {
                break;
            }
        }
        let s = match std::str::from_utf8(&self.src[start..self.pos]) {
            Ok(s) => s,
            Err(_) => {
                self.error_at(start, self.pos, "Identifier is not valid UTF-8");
                return TokenKind::Error;
            }
        };

        match s {
            "export" => TokenKind::KwExport,
            "function" => TokenKind::KwFunction,
            "import" => TokenKind::KwImport,
            "from" => TokenKind::KwFrom,
            "return" => TokenKind::KwReturn,
            "let" => TokenKind::KwLet,
            "const" => TokenKind::KwConst,
            "if" => TokenKind::KwIf,
            "else" => TokenKind::KwElse,
            "while" => TokenKind::KwWhile,
            "for" => TokenKind::KwFor,
            "break" => TokenKind::KwBreak,
            "continue" => TokenKind::KwContinue,
            _ => TokenKind::Ident,
        }
    }

    fn lex_number_or_bigint(&mut self) -> TokenKind {
        let start = self.pos;
        self.pos += 1;

        // Integer part
        while let Some(b'0'..=b'9') = self.peek_byte() {
            self.pos += 1;
        }

        // Fractional part (bigint cannot contain '.')
        let mut is_float = false;
        if let Some(b'.') = self.peek_byte() {
            is_float = true;
            self.pos += 1;
            while let Some(b'0'..=b'9') = self.peek_byte() {
                self.pos += 1;
            }
        }

        // Exponent part (scientific notation): `1e3` / `1E-3`.
        if let Some(b'e' | b'E') = self.peek_byte() {
            is_float = true;
            self.pos += 1; // consume `e` / `E`

            // Optional sign: `+` / `-`
            if let Some(b'+' | b'-') = self.peek_byte() {
                self.pos += 1;
            }

            // Require at least one digit.
            let exp_start = self.pos;
            while let Some(b'0'..=b'9') = self.peek_byte() {
                self.pos += 1;
            }
            if self.pos == exp_start {
                self.error_at(
                    start,
                    self.pos,
                    "Invalid scientific notation: missing exponent digits",
                );
                return TokenKind::Error;
            }
        }

        // BigInt suffix 'n' (integers only).
        if let Some(b'n') = self.peek_byte() {
            if is_float {
                self.error_at(
                    start,
                    self.pos + 1,
                    "BigInt suffix 'n' is not allowed on floating-point literals",
                );
                self.pos += 1;
                return TokenKind::Error;
            }
            self.pos += 1;
            return TokenKind::BigInt;
        }

        TokenKind::Number
    }

    fn lex_string(&mut self) -> bool {
        let quote = self.peek_byte().expect("string start checked");
        let start = self.pos;
        self.pos += 1;
        while let Some(b) = self.peek_byte() {
            self.pos += 1;
            match b {
                b'\\' => {
                    // Skip one byte after an escape (simplified: we do not implement full escape semantics).
                    if self.peek_byte().is_some() {
                        self.pos += 1;
                    }
                }
                _ if b == quote => return true,
                _ => {}
            }
        }
        self.error_at(start, self.pos, "Unterminated string literal");
        false
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            while let Some(b) = self.peek_byte() {
                if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' {
                    self.pos += 1;
                } else {
                    break;
                }
            }

            let Some(b'/') = self.peek_byte() else { break };
            if self.peek_byte_at(self.pos + 1) == Some(b'/') {
                // Line comment
                self.pos += 2;
                while let Some(b) = self.peek_byte() {
                    self.pos += 1;
                    if b == b'\n' {
                        break;
                    }
                }
                continue;
            }
            if self.peek_byte_at(self.pos + 1) == Some(b'*') {
                // Block comment
                let start = self.pos;
                self.pos += 2;
                loop {
                    match (self.peek_byte(), self.peek_byte_at(self.pos + 1)) {
                        (Some(b'*'), Some(b'/')) => {
                            self.pos += 2;
                            break;
                        }
                        (Some(_), _) => self.pos += 1,
                        (None, _) => {
                            self.error_at(
                                start,
                                self.pos,
                                "Unterminated block comment (missing */)",
                            );
                            self.pos = self.src.len();
                            return;
                        }
                    }
                }
                continue;
            }

            break;
        }
    }

    #[inline]
    fn peek_byte(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    #[inline]
    fn peek_byte_at(&self, pos: usize) -> Option<u8> {
        self.src.get(pos).copied()
    }

    fn error_at(&mut self, start: usize, end: usize, message: impl Into<String>) {
        self.diags
            .error_at(&self.path, Span { start, end }, message);
    }
}

#[inline]
fn lex_single_char(b: u8) -> Option<TokenKind> {
    Some(match b {
        b':' => TokenKind::Colon,
        b';' => TokenKind::Semicolon,
        b',' => TokenKind::Comma,
        b'.' => TokenKind::Dot,
        b'(' => TokenKind::LParen,
        b')' => TokenKind::RParen,
        b'{' => TokenKind::LBrace,
        b'}' => TokenKind::RBrace,
        _ => return None,
    })
}
