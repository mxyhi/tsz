use crate::{Span, TszError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    KwExport,
    KwFunction,
    KwImport,
    KwFrom,
    KwReturn,
    KwLet,
    KwConst,
    Ident,
    Number,
    BigInt,
    String,
    Colon,
    Equal,
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
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn lex(source: &str) -> Result<Vec<Token>, TszError> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let t = lexer.next_token()?;
        let is_eof = t.kind == TokenKind::Eof;
        tokens.push(t);
        if is_eof {
            break;
        }
    }
    Ok(tokens)
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            src: source.as_bytes(),
            pos: 0,
        }
    }

    fn next_token(&mut self) -> Result<Token, TszError> {
        self.skip_ws_and_comments()?;
        let start = self.pos;
        let Some(b) = self.peek_byte() else { return Ok(self.eof_token()) };
        let kind = self.lex_token_kind(start, b)?;

        Ok(Token {
            kind,
            span: Span {
                start,
                end: self.pos,
            },
        })
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

    fn lex_token_kind(&mut self, start: usize, b: u8) -> Result<TokenKind, TszError> {
        // 2-char operators must be handled before `lex_single_char`.
        match b {
            b'+' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return Ok(TokenKind::PlusEqual);
                }
                self.pos += 1;
                return Ok(TokenKind::Plus);
            }
            b'-' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return Ok(TokenKind::MinusEqual);
                }
                self.pos += 1;
                return Ok(TokenKind::Minus);
            }
            b'*' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return Ok(TokenKind::StarEqual);
                }
                self.pos += 1;
                return Ok(TokenKind::Star);
            }
            b'/' => {
                if self.peek_byte_at(self.pos + 1) == Some(b'=') {
                    self.pos += 2;
                    return Ok(TokenKind::SlashEqual);
                }
                self.pos += 1;
                return Ok(TokenKind::Slash);
            }
            b'=' => {
                self.pos += 1;
                return Ok(TokenKind::Equal);
            }
            _ => {}
        }

        if let Some(kind) = lex_single_char(b) {
            self.pos += 1;
            return Ok(kind);
        }

        match b {
            b'"' | b'\'' => {
                self.lex_string()?;
                Ok(TokenKind::String)
            }
            b'0'..=b'9' => self.lex_number_or_bigint(),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => self.lex_ident_or_keyword(),
            _ => Err(TszError::Lex {
                message: format!("Unsupported character: 0x{b:02X}"),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
        }
    }

    fn lex_ident_or_keyword(&mut self) -> Result<TokenKind, TszError> {
        let start = self.pos;
        self.pos += 1;
        while let Some(b) = self.peek_byte() {
            if b.is_ascii_alphanumeric() || b == b'_' || b == b'$' {
                self.pos += 1;
            } else {
                break;
            }
        }
        let s = std::str::from_utf8(&self.src[start..self.pos]).map_err(|_| TszError::Lex {
            message: "Identifier is not valid UTF-8".to_string(),
            span: Span {
                start,
                end: self.pos,
            },
        })?;

        Ok(match s {
            "export" => TokenKind::KwExport,
            "function" => TokenKind::KwFunction,
            "import" => TokenKind::KwImport,
            "from" => TokenKind::KwFrom,
            "return" => TokenKind::KwReturn,
            "let" => TokenKind::KwLet,
            "const" => TokenKind::KwConst,
            _ => TokenKind::Ident,
        })
    }

    fn lex_number_or_bigint(&mut self) -> Result<TokenKind, TszError> {
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

        // BigInt suffix 'n' (integers only)
        if !is_float {
            if let Some(b'n') = self.peek_byte() {
                self.pos += 1;
                return Ok(TokenKind::BigInt);
            }
        }

        // Scientific notation is not supported yet (keep the subset small/optimizable).
        if let Some(b'e' | b'E') = self.peek_byte() {
            return Err(TszError::Lex {
                message: "Scientific notation is not supported yet".to_string(),
                span: Span {
                    start,
                    end: self.pos + 1,
                },
            });
        }

        Ok(TokenKind::Number)
    }

    fn lex_string(&mut self) -> Result<(), TszError> {
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
                _ if b == quote => return Ok(()),
                _ => {}
            }
        }
        Err(TszError::Lex {
            message: "Unterminated string literal".to_string(),
            span: Span {
                start,
                end: self.pos,
            },
        })
    }

    fn skip_ws_and_comments(&mut self) -> Result<(), TszError> {
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
                            return Err(TszError::Lex {
                                message: "Unterminated block comment (missing */)".to_string(),
                                span: Span {
                                    start,
                                    end: self.pos,
                                },
                            });
                        }
                    }
                }
                continue;
            }

            break;
        }
        Ok(())
    }

    #[inline]
    fn peek_byte(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    #[inline]
    fn peek_byte_at(&self, pos: usize) -> Option<u8> {
        self.src.get(pos).copied()
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
