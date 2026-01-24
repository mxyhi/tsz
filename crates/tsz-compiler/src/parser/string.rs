use super::Parser;
use crate::Span;
use crate::lexer::Token;

impl<'a, 'd> Parser<'a, 'd> {
    pub(super) fn parse_string_value(&mut self, tok: Token) -> Option<String> {
        let raw = self.slice(tok.span);
        let bytes = raw.as_bytes();
        self.validate_string_literal(tok, bytes)?;

        // 解码转义序列（TSZ 子集）：
        // - \n \t \\ \" \' \uXXXX
        // 注意：这里产出“字面量的值”，便于后续 typecheck / const folding / codegen 复用。
        let out = self.decode_string_escapes(tok, bytes)?;
        match String::from_utf8(out) {
            Ok(s) => Some(s),
            Err(_) => {
                self.error_at(tok.span, "String literal is not valid UTF-8");
                None
            }
        }
    }

    fn validate_string_literal(&mut self, tok: Token, bytes: &[u8]) -> Option<()> {
        if bytes.len() < 2 {
            self.error_at(tok.span, "Invalid string literal");
            return None;
        }
        let quote = bytes[0];
        if quote != b'"' && quote != b'\'' {
            self.error_at(tok.span, "Invalid string literal");
            return None;
        }
        if bytes[bytes.len() - 1] != quote {
            self.error_at(tok.span, "Invalid string literal");
            return None;
        }
        Some(())
    }

    fn decode_string_escapes(&mut self, tok: Token, bytes: &[u8]) -> Option<Vec<u8>> {
        let mut out = Vec::with_capacity(bytes.len().saturating_sub(2));
        let mut i = 1;
        let end = bytes.len() - 1; // exclude closing quote
        while i < end {
            if bytes[i] != b'\\' {
                out.push(bytes[i]);
                i += 1;
                continue;
            }

            self.decode_escape(tok, bytes, end, &mut i, &mut out)?;
        }

        Some(out)
    }

    fn decode_escape(
        &mut self,
        tok: Token,
        bytes: &[u8],
        end: usize,
        i: &mut usize,
        out: &mut Vec<u8>,
    ) -> Option<()> {
        // 入口：bytes[*i] == '\\'
        let backslash_pos = *i;
        *i += 1; // consume '\'
        if *i >= end {
            self.error_from_offset_to_end(
                tok,
                backslash_pos,
                "Invalid string literal: incomplete escape sequence",
            );
            return None;
        }

        let b = match bytes[*i] {
            b'n' => b'\n',
            b't' => b'\t',
            b'\\' => b'\\',
            b'"' => b'"',
            b'\'' => b'\'',
            b'u' => {
                let u_pos = *i;
                let ch = self.decode_unicode_escape(tok, bytes, backslash_pos, u_pos, end)?;
                let mut buf = [0u8; 4];
                let s = ch.encode_utf8(&mut buf);
                out.extend_from_slice(s.as_bytes());
                *i = u_pos + 5; // 'u' + 4 digits
                return Some(());
            }
            other => {
                let esc = other as char;
                self.error_at_offsets(
                    tok,
                    backslash_pos,
                    *i + 1,
                    format!("Unsupported escape sequence: \\{esc}"),
                );
                return None;
            }
        };

        out.push(b);
        *i += 1;

        Some(())
    }

    fn decode_unicode_escape(
        &mut self,
        tok: Token,
        bytes: &[u8],
        backslash_pos: usize,
        u_pos: usize,
        end: usize,
    ) -> Option<char> {
        let code = self.decode_unicode_hex4(tok, bytes, backslash_pos, u_pos, end)?;

        // Reject surrogate halves.
        if (0xD800..=0xDFFF).contains(&code) {
            self.error_at_offsets(
                tok,
                backslash_pos,
                u_pos + 5,
                "Invalid unicode escape: surrogate code point is not allowed",
            );
            return None;
        }

        let Some(ch) = char::from_u32(code) else {
            self.error_at_offsets(
                tok,
                backslash_pos,
                u_pos + 5,
                "Invalid unicode escape: code point is not a valid Unicode scalar value",
            );
            return None;
        };

        Some(ch)
    }

    fn decode_unicode_hex4(
        &mut self,
        tok: Token,
        bytes: &[u8],
        backslash_pos: usize,
        u_pos: usize,
        end: usize,
    ) -> Option<u32> {
        // `\uXXXX` (4 hex digits)
        if u_pos + 4 >= end {
            self.error_from_offset_to_end(
                tok,
                backslash_pos,
                "Invalid unicode escape: expected 4 hex digits (\\uXXXX)",
            );
            return None;
        }

        let mut code: u32 = 0;
        for off in 1..=4 {
            let b = bytes[u_pos + off];
            let Some(v) = hex_digit_value(b) else {
                self.error_at_offsets(
                    tok,
                    backslash_pos,
                    u_pos + off + 1,
                    "Invalid unicode escape: expected hex digits (\\uXXXX)",
                );
                return None;
            };
            code = (code << 4) | v;
        }

        Some(code)
    }

    fn error_at_offsets(
        &mut self,
        tok: Token,
        start_offset: usize,
        end_offset: usize,
        message: impl Into<String>,
    ) {
        self.error_at(
            Span {
                start: tok.span.start + start_offset,
                end: tok.span.start + end_offset,
            },
            message,
        );
    }

    fn error_from_offset_to_end(
        &mut self,
        tok: Token,
        start_offset: usize,
        message: impl Into<String>,
    ) {
        self.error_at(
            Span {
                start: tok.span.start + start_offset,
                end: tok.span.end,
            },
            message,
        );
    }
}

fn hex_digit_value(b: u8) -> Option<u32> {
    match b {
        b'0'..=b'9' => Some(u32::from(b - b'0')),
        b'a'..=b'f' => Some(u32::from(b - b'a' + 10)),
        b'A'..=b'F' => Some(u32::from(b - b'A' + 10)),
        _ => None,
    }
}
