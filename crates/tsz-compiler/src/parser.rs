use crate::diagnostics::Diagnostics;
use crate::lexer::{Token, TokenKind};
use crate::{ast::*, lexer, Span};
use std::path::{Path, PathBuf};

mod expr;
mod stmt;

pub fn parse_module(path: &Path, source: &str, diags: &mut Diagnostics) -> Module {
    let tokens = lexer::lex(source, path, diags);
    let mut p = Parser::new(path, source, tokens, diags);
    let mut imports = Vec::new();
    let mut functions = Vec::new();

    while !p.is_eof() {
        let start_idx = p.idx;
        match p.peek().kind {
            TokenKind::KwImport => {
                if let Some(import) = p.parse_import() {
                    imports.push(import);
                } else {
                    p.sync_toplevel();
                }
            }
            TokenKind::KwExport | TokenKind::KwFunction => {
                if let Some(func) = p.parse_function() {
                    functions.push(func);
                } else {
                    p.sync_toplevel();
                }
            }
            TokenKind::Eof => break,
            _ => {
                let span = p.peek().span;
                p.error_at(span, "Expected import or function declaration");
                p.bump();
            }
        }

        if p.idx == start_idx {
            // Ensure forward progress even if a handler forgot to consume.
            p.bump();
        }
    }

    Module {
        path: path.to_path_buf(),
        imports,
        functions,
    }
}

struct Parser<'a, 'd> {
    path: PathBuf,
    source: &'a str,
    tokens: Vec<Token>,
    idx: usize,
    diags: &'d mut Diagnostics,
}

impl<'a, 'd> Parser<'a, 'd> {
    fn new(path: &Path, source: &'a str, tokens: Vec<Token>, diags: &'d mut Diagnostics) -> Self {
        Self {
            path: path.to_path_buf(),
            source,
            tokens,
            idx: 0,
            diags,
        }
    }

    fn is_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn parse_function(&mut self) -> Option<FunctionDecl> {
        let start_span = self.peek().span;
        let is_export = self.eat(TokenKind::KwExport);
        let (_kw_fn, ok_fn) = self.expect_kind(TokenKind::KwFunction);
        if !ok_fn {
            return None;
        }

        let (name_tok, name_ok) = self.expect_ident();
        let name = if name_ok {
            self.slice(name_tok.span).to_string()
        } else {
            "<error>".to_string()
        };

        self.expect(TokenKind::LParen);
        let params = self.parse_param_list();
        self.expect(TokenKind::RParen);

        self.expect(TokenKind::Colon);
        let return_type = self.parse_type();

        self.expect(TokenKind::LBrace);
        let mut body = Vec::new();
        while self.peek().kind != TokenKind::RBrace && !self.is_eof() {
            body.push(self.parse_stmt());
        }
        self.expect(TokenKind::RBrace);

        let end_span = self.prev_span();
        Some(FunctionDecl {
            is_export,
            name,
            params,
            return_type,
            body,
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    fn parse_param_list(&mut self) -> Vec<ParamDecl> {
        let mut params = Vec::new();
        if self.peek().kind == TokenKind::RParen {
            return params;
        }

        loop {
            let start = self.peek().span;
            let (name_tok, name_ok) = self.expect_ident();
            let name_span = name_tok.span;
            let name = if name_ok {
                self.slice(name_tok.span).to_string()
            } else {
                "<error>".to_string()
            };

            self.expect(TokenKind::Colon);
            let ty = self.parse_type();
            let end = self.prev_span();

            params.push(ParamDecl {
                name,
                name_span,
                ty,
                span: Span {
                    start: start.start,
                    end: end.end,
                },
            });

            if self.eat(TokenKind::Comma) {
                if self.peek().kind == TokenKind::RParen {
                    self.error_at(self.peek().span, "Trailing comma in parameter list is not supported");
                    return params;
                }
                continue;
            }
            break;
        }

        params
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.peek().kind {
            TokenKind::KwReturn => self.parse_return_stmt(),
            TokenKind::KwLet => self.parse_let_stmt(),
            TokenKind::KwConst => self.parse_const_stmt(),
            TokenKind::LBrace => self.parse_block_stmt(),
            TokenKind::KwIf => self.parse_if_stmt(),
            TokenKind::KwWhile => self.parse_while_stmt(),
            TokenKind::KwFor => self.parse_for_stmt(),
            TokenKind::KwBreak => self.parse_break_stmt(),
            TokenKind::KwContinue => self.parse_continue_stmt(),
            TokenKind::Ident => {
                let is_dot = self.tokens.get(self.idx + 1).map(|t| t.kind) == Some(TokenKind::Dot);
                if is_dot {
                    self.parse_console_log_stmt()
                } else {
                    self.parse_assign_stmt()
                }
            }
            TokenKind::Error => {
                let span = self.bump().span;
                Stmt::Error { span }
            }
            _ => {
                let start = self.peek().span;
                self.error_at(start, "Only block/if/while/for/break/continue/let/const/<name> = <expr>/console.log(...)/return statements are supported");
                self.sync_stmt();
                let end = self.prev_span();
                Stmt::Error {
                    span: Span {
                        start: start.start,
                        end: end.end,
                    },
                }
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Stmt {
        let start = self.expect(TokenKind::KwLet).span;
        let (name_tok, name_ok) = self.expect_ident();
        let name_span = name_tok.span;
        let name = if name_ok {
            self.slice(name_tok.span).to_string()
        } else {
            "<error>".to_string()
        };

        let annotated_type = if self.eat(TokenKind::Colon) {
            Some(self.parse_type())
        } else {
            None
        };

        self.expect(TokenKind::Equal);
        let expr = self.parse_expr();
        let semi = self.expect(TokenKind::Semicolon).span;

        Stmt::Let {
            name,
            name_span,
            annotated_type,
            expr,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        }
    }

    fn parse_const_stmt(&mut self) -> Stmt {
        let start = self.expect(TokenKind::KwConst).span;
        let (name_tok, name_ok) = self.expect_ident();
        let name_span = name_tok.span;
        let name = if name_ok {
            self.slice(name_tok.span).to_string()
        } else {
            "<error>".to_string()
        };

        let annotated_type = if self.eat(TokenKind::Colon) {
            Some(self.parse_type())
        } else {
            None
        };

        self.expect(TokenKind::Equal);
        let expr = self.parse_expr();
        let semi = self.expect(TokenKind::Semicolon).span;

        Stmt::Const {
            name,
            name_span,
            annotated_type,
            expr,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        }
    }

    fn parse_import(&mut self) -> Option<ImportDecl> {
        let start = self.expect(TokenKind::KwImport).span;
        self.expect(TokenKind::LBrace);

        let mut names = Vec::new();
        if self.peek().kind == TokenKind::RBrace {
            self.error_at(self.peek().span, "Import list cannot be empty");
            self.sync_stmt();
            return None;
        }
        loop {
            let (ident, ok) = self.expect_ident();
            let name = if ok {
                self.slice(ident.span).to_string()
            } else {
                "<error>".to_string()
            };
            names.push(ImportName { name, span: ident.span });
            if self.eat(TokenKind::Comma) {
                continue;
            }
            break;
        }

        self.expect(TokenKind::RBrace);
        self.expect(TokenKind::KwFrom);
        let (from_tok, from_ok) = self.expect_string();
        let from = if from_ok {
            self.parse_string_value(from_tok)
        } else {
            String::new()
        };
        let semi = self.expect(TokenKind::Semicolon).span;

        if from.is_empty() {
            self.error_at(from_tok.span, "Import source must be a string literal");
            return None;
        }

        Some(ImportDecl {
            names,
            from,
            resolved_path: None,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    fn parse_type(&mut self) -> Type {
        let (tok, ok) = self.expect_ident();
        if !ok {
            return Type::Error;
        }
        let s = self.slice(tok.span);
        match s {
            "number" => Type::Number,
            "bigint" => Type::BigInt,
            "void" => Type::Void,
            "boolean" => Type::Bool,
            "string" => Type::String,
            _ => {
                self.error_at(tok.span, format!("Unsupported type: {s}"));
                Type::Error
            }
        }
    }

    fn parse_return_stmt(&mut self) -> Stmt {
        let start = self.expect(TokenKind::KwReturn).span;
        if self.peek().kind == TokenKind::Semicolon {
            let semi = self.bump().span;
            return Stmt::Return {
                expr: None,
                span: Span {
                    start: start.start,
                    end: semi.end,
                },
            };
        }

        let expr = self.parse_expr();
        let semi = self.expect(TokenKind::Semicolon).span;
        Stmt::Return {
            expr: Some(expr),
            span: Span {
                start: start.start,
                end: semi.end,
            },
        }
    }

    fn parse_string_value(&self, tok: Token) -> String {
        let raw = self.slice(tok.span);
        let bytes = raw.as_bytes();
        if bytes.len() < 2 {
            return String::new();
        }
        let quote = bytes[0];
        if quote != b'"' && quote != b'\'' {
            return String::new();
        }
        if bytes[bytes.len() - 1] != quote {
            return String::new();
        }
        String::from_utf8(bytes[1..bytes.len() - 1].to_vec()).unwrap_or_default()
    }

    fn peek(&self) -> Token {
        self.tokens
            .get(self.idx)
            .copied()
            .unwrap_or(Token {
                kind: TokenKind::Eof,
                span: Span { start: 0, end: 0 },
            })
    }

    fn bump(&mut self) -> Token {
        let t = self.peek();
        if t.kind != TokenKind::Eof {
            self.idx += 1;
        }
        t
    }

    fn expect(&mut self, kind: TokenKind) -> Token {
        let (tok, _ok) = self.expect_kind(kind);
        tok
    }

    fn expect_ident(&mut self) -> (Token, bool) {
        self.expect_kind(TokenKind::Ident)
    }

    fn expect_string(&mut self) -> (Token, bool) {
        self.expect_kind(TokenKind::String)
    }

    fn expect_kind(&mut self, kind: TokenKind) -> (Token, bool) {
        let t = self.peek();
        if t.kind == kind {
            (self.bump(), true)
        } else {
            if t.kind != TokenKind::Error {
                self.error_at(t.span, format!("Expected {kind:?}, got {got:?}", got = t.kind));
            }
            (self.bump(), false)
        }
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.peek().kind == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn slice(&self, span: Span) -> &'a str {
        &self.source[span.start..span.end]
    }

    fn prev_span(&self) -> Span {
        self.tokens
            .get(self.idx.saturating_sub(1))
            .map(|t| t.span)
            .unwrap_or(Span { start: 0, end: 0 })
    }

    fn error_at(&mut self, span: Span, message: impl Into<String>) {
        self.diags.error_at(&self.path, span, message);
    }

    fn sync_stmt(&mut self) {
        while !self.is_eof() {
            match self.peek().kind {
                TokenKind::Semicolon => {
                    self.bump();
                    break;
                }
                TokenKind::RBrace => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn sync_toplevel(&mut self) {
        while !self.is_eof() {
            match self.peek().kind {
                TokenKind::KwImport | TokenKind::KwFunction | TokenKind::KwExport => break,
                _ => {
                    self.bump();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
