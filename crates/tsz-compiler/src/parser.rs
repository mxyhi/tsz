use crate::lexer::{Token, TokenKind};
use crate::{ast::*, lexer, Span, TszError};
use std::path::Path;

mod stmt;

pub fn parse_module(path: &Path, source: &str) -> Result<Module, TszError> {
    let tokens = lexer::lex(source)?;
    let mut p = Parser::new(source, tokens);
    let mut imports = Vec::new();
    let mut functions = Vec::new();
    while !p.is_eof() {
        match p.peek().kind {
            TokenKind::KwImport => imports.push(p.parse_import()?),
            _ => functions.push(p.parse_function()?),
        }
    }
    Ok(Module {
        path: path.to_path_buf(),
        imports,
        functions,
    })
}

struct Parser<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    idx: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, tokens: Vec<Token>) -> Self {
        Self { source, tokens, idx: 0 }
    }

    fn is_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn parse_function(&mut self) -> Result<FunctionDecl, TszError> {
        let start_span = self.peek().span;
        let is_export = self.eat(TokenKind::KwExport);
        self.expect(TokenKind::KwFunction)?;

        let name_tok = self.expect(TokenKind::Ident)?;
        let name = self.slice(name_tok.span).to_string();

        self.expect(TokenKind::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while self.peek().kind != TokenKind::RBrace && !self.is_eof() {
            body.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;

        let end_span = self.prev_span();
        Ok(FunctionDecl {
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

    fn parse_param_list(&mut self) -> Result<Vec<ParamDecl>, TszError> {
        let mut params = Vec::new();
        if self.peek().kind == TokenKind::RParen {
            return Ok(params);
        }

        loop {
            let start = self.peek().span;
            let name_tok = self.expect(TokenKind::Ident)?;
            let name_span = name_tok.span;
            let name = self.slice(name_tok.span).to_string();

            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
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
                    return Err(TszError::Parse {
                        message: "Trailing comma in parameter list is not supported".to_string(),
                        span: self.peek().span,
                    });
                }
                continue;
            }
            break;
        }

        Ok(params)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, TszError> {
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
            _ => Err(TszError::Parse {
                message:
                    "Only block/if/while/for/break/continue/let/const/<name> = <expr>/console.log(...)/return statements are supported"
                        .to_string(),
                span: self.peek().span,
            }),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwLet)?.span;
        let name_tok = self.expect(TokenKind::Ident)?;
        let name_span = name_tok.span;
        let name = self.slice(name_tok.span).to_string();

        let annotated_type = if self.eat(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr()?;
        let semi = self.expect(TokenKind::Semicolon)?.span;

        Ok(Stmt::Let {
            name,
            name_span,
            annotated_type,
            expr,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    fn parse_const_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwConst)?.span;
        let name_tok = self.expect(TokenKind::Ident)?;
        let name_span = name_tok.span;
        let name = self.slice(name_tok.span).to_string();

        let annotated_type = if self.eat(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Equal)?;
        let expr = self.parse_expr()?;
        let semi = self.expect(TokenKind::Semicolon)?.span;

        Ok(Stmt::Const {
            name,
            name_span,
            annotated_type,
            expr,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    fn parse_import(&mut self) -> Result<ImportDecl, TszError> {
        let start = self.expect(TokenKind::KwImport)?.span;
        self.expect(TokenKind::LBrace)?;

        let mut names = Vec::new();
        if self.peek().kind == TokenKind::RBrace {
            return Err(TszError::Parse {
                message: "Import list cannot be empty".to_string(),
                span: self.peek().span,
            });
        }
        loop {
            let ident = self.expect(TokenKind::Ident)?;
            names.push(ImportName {
                name: self.slice(ident.span).to_string(),
                span: ident.span,
            });
            if self.eat(TokenKind::Comma) {
                continue;
            }
            break;
        }

        self.expect(TokenKind::RBrace)?;
        self.expect(TokenKind::KwFrom)?;
        let from_tok = self.expect(TokenKind::String)?;
        let from = self.parse_string_value(from_tok)?;
        let semi = self.expect(TokenKind::Semicolon)?.span;

        Ok(ImportDecl {
            names,
            from,
            resolved_path: None,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    fn parse_type(&mut self) -> Result<Type, TszError> {
        let tok = self.expect(TokenKind::Ident)?;
        let s = self.slice(tok.span);
        match s {
            "number" => Ok(Type::Number),
            "bigint" => Ok(Type::BigInt),
            "void" => Ok(Type::Void),
            "boolean" => Ok(Type::Bool),
            "string" => Ok(Type::String),
            _ => Err(TszError::Parse {
                message: format!("Unsupported type: {s}"),
                span: tok.span,
            }),
        }
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwReturn)?.span;
        if self.peek().kind == TokenKind::Semicolon {
            let semi = self.bump().span;
            return Ok(Stmt::Return {
                expr: None,
                span: Span {
                    start: start.start,
                    end: semi.end,
                },
            });
        }

        let expr = self.parse_expr()?;
        let semi = self.expect(TokenKind::Semicolon)?.span;
        Ok(Stmt::Return {
            expr: Some(expr),
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, TszError> {
        self.parse_additive_expr()
    }

    // Expression parsing (small TS subset):
    // - primary: literals / identifiers / calls / parenthesized expressions
    // - unary: -<expr>
    // - multiplicative: * /
    // - additive: + -
    fn parse_additive_expr(&mut self) -> Result<Expr, TszError> {
        let mut expr = self.parse_multiplicative_expr()?;
        loop {
            let op = match self.peek().kind {
                TokenKind::Plus => Some(BinaryOp::Add),
                TokenKind::Minus => Some(BinaryOp::Sub),
                _ => None,
            };
            let Some(op) = op else { break };
            self.bump(); // operator
            let rhs = self.parse_multiplicative_expr()?;
            let span = Span {
                start: expr_span(&expr).start,
                end: expr_span(&rhs).end,
            };
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expr, TszError> {
        let mut expr = self.parse_unary_expr()?;
        loop {
            let op = match self.peek().kind {
                TokenKind::Star => Some(BinaryOp::Mul),
                TokenKind::Slash => Some(BinaryOp::Div),
                _ => None,
            };
            let Some(op) = op else { break };
            self.bump(); // operator
            let rhs = self.parse_unary_expr()?;
            let span = Span {
                start: expr_span(&expr).start,
                end: expr_span(&rhs).end,
            };
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, TszError> {
        if self.eat(TokenKind::Minus) {
            let minus_span = self.prev_span();
            let expr = self.parse_unary_expr()?;
            let span = Span {
                start: minus_span.start,
                end: expr_span(&expr).end,
            };
            return Ok(Expr::UnaryMinus {
                expr: Box::new(expr),
                span,
            });
        }
        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, TszError> {
        let tok = self.peek();
        match tok.kind {
            TokenKind::Number => {
                let tok = self.bump();
                let s = self.slice(tok.span);
                let v: f64 = s.parse().map_err(|_| TszError::Parse {
                    message: format!("Invalid number literal: {s}"),
                    span: tok.span,
                })?;
                Ok(Expr::Number { value: v, span: tok.span })
            }
            TokenKind::BigInt => {
                let tok = self.bump();
                let raw = self.slice(tok.span);
                let s = raw.strip_suffix('n').unwrap_or(raw);
                let v: i64 = s.parse().map_err(|_| TszError::Parse {
                    message: format!("Invalid bigint literal: {raw}"),
                    span: tok.span,
                })?;
                Ok(Expr::BigInt { value: v, span: tok.span })
            }
            TokenKind::String => {
                let tok = self.bump();
                let v = self.parse_string_value(tok)?;
                Ok(Expr::String { value: v, span: tok.span })
            }
            TokenKind::Ident => {
                let ident = self.bump();
                let name = self.slice(ident.span).to_string();
                if name == "true" || name == "false" {
                    return Ok(Expr::Bool {
                        value: name == "true",
                        span: ident.span,
                    });
                }
                if self.eat(TokenKind::LParen) {
                    let mut args = Vec::new();
                    if self.peek().kind != TokenKind::RParen {
                        loop {
                            args.push(self.parse_expr()?);
                            if self.eat(TokenKind::Comma) {
                                if self.peek().kind == TokenKind::RParen {
                                    return Err(TszError::Parse {
                                        message: "Trailing comma in call arguments is not supported".to_string(),
                                        span: self.peek().span,
                                    });
                                }
                                continue;
                            }
                            break;
                        }
                    }
                    let rparen = self.expect(TokenKind::RParen)?.span;
                    Ok(Expr::Call {
                        callee: name,
                        args,
                        span: Span {
                            start: ident.span.start,
                            end: rparen.end,
                        },
                    })
                } else {
                    Ok(Expr::Ident { name, span: ident.span })
                }
            }
            TokenKind::LParen => {
                self.bump();
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            _ => Err(TszError::Parse {
                message: "Expected expression".to_string(),
                span: tok.span,
            }),
        }
    }

    fn parse_string_value(&self, tok: Token) -> Result<String, TszError> {
        let raw = self.slice(tok.span);
        let bytes = raw.as_bytes();
        if bytes.len() < 2 {
            return Err(TszError::Parse {
                message: "Invalid string literal".to_string(),
                span: tok.span,
            });
        }
        let quote = bytes[0];
        if quote != b'"' && quote != b'\'' {
            return Err(TszError::Parse {
                message: "Invalid string literal (missing quotes)".to_string(),
                span: tok.span,
            });
        }
        if bytes[bytes.len() - 1] != quote {
            return Err(TszError::Parse {
                message: "Invalid string literal (missing closing quote)".to_string(),
                span: tok.span,
            });
        }
        String::from_utf8(bytes[1..bytes.len() - 1].to_vec()).map_err(|_| TszError::Parse {
            message: "String literal is not valid UTF-8".to_string(),
            span: tok.span,
        })
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

    fn expect(&mut self, kind: TokenKind) -> Result<Token, TszError> {
        let t = self.peek();
        if t.kind == kind {
            Ok(self.bump())
        } else {
            Err(TszError::Parse {
                message: format!("Expected {kind:?}, got {got:?}", got = t.kind),
                span: t.span,
            })
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
}

fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Number { span, .. }
        | Expr::BigInt { span, .. }
        | Expr::Bool { span, .. }
        | Expr::String { span, .. } => *span,
        Expr::Ident { span, .. } => *span,
        Expr::UnaryMinus { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::Binary { span, .. } => *span,
    }
}

#[cfg(test)]
mod tests;
