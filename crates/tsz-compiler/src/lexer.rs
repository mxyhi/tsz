use crate::{Span, TszError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    KwExport,
    KwFunction,
    KwReturn,
    Ident,
    Number,
    BigInt,
    String,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Minus,
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
        let Some(b) = self.peek_byte() else {
            return Ok(Token {
                kind: TokenKind::Eof,
                span: Span {
                    start: self.pos,
                    end: self.pos,
                },
            });
        };

        let kind = match b {
            b':' => {
                self.pos += 1;
                TokenKind::Colon
            }
            b';' => {
                self.pos += 1;
                TokenKind::Semicolon
            }
            b'(' => {
                self.pos += 1;
                TokenKind::LParen
            }
            b')' => {
                self.pos += 1;
                TokenKind::RParen
            }
            b'{' => {
                self.pos += 1;
                TokenKind::LBrace
            }
            b'}' => {
                self.pos += 1;
                TokenKind::RBrace
            }
            b'-' => {
                self.pos += 1;
                TokenKind::Minus
            }
            b'"' | b'\'' => {
                self.lex_string()?;
                TokenKind::String
            }
            b'0'..=b'9' => self.lex_number_or_bigint()?,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => self.lex_ident_or_keyword()?,
            _ => {
                return Err(TszError::Lex {
                    message: format!("不支持的字符: 0x{b:02X}"),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                });
            }
        };

        Ok(Token {
            kind,
            span: Span {
                start,
                end: self.pos,
            },
        })
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
            message: "标识符不是有效 UTF-8".to_string(),
            span: Span {
                start,
                end: self.pos,
            },
        })?;

        Ok(match s {
            "export" => TokenKind::KwExport,
            "function" => TokenKind::KwFunction,
            "return" => TokenKind::KwReturn,
            _ => TokenKind::Ident,
        })
    }

    fn lex_number_or_bigint(&mut self) -> Result<TokenKind, TszError> {
        let start = self.pos;
        self.pos += 1;

        // 整数部分
        while let Some(b'0'..=b'9') = self.peek_byte() {
            self.pos += 1;
        }

        // 小数部分（bigint 不允许小数点）
        let mut is_float = false;
        if let Some(b'.') = self.peek_byte() {
            is_float = true;
            self.pos += 1;
            while let Some(b'0'..=b'9') = self.peek_byte() {
                self.pos += 1;
            }
        }

        // bigint 后缀 n（只允许纯整数）
        if !is_float {
            if let Some(b'n') = self.peek_byte() {
                self.pos += 1;
                return Ok(TokenKind::BigInt);
            }
        }

        // 科学计数法暂不支持（保持子集可控、可优化）
        if let Some(b'e' | b'E') = self.peek_byte() {
            return Err(TszError::Lex {
                message: "暂不支持科学计数法".to_string(),
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
                    // 跳过转义后的一个字节（简化：不做完整转义语义，只保证能跳过）
                    if self.peek_byte().is_some() {
                        self.pos += 1;
                    }
                }
                _ if b == quote => return Ok(()),
                _ => {}
            }
        }
        Err(TszError::Lex {
            message: "字符串缺少闭合引号".to_string(),
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
                // 行注释
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
                // 块注释
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
                                message: "块注释缺少闭合 */".to_string(),
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

