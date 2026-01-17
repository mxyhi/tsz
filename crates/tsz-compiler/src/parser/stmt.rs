use crate::lexer::TokenKind;
use crate::{ast::*, Span, TszError};

use super::{expr_span, Parser};

impl<'a> Parser<'a> {
    pub(super) fn parse_block_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::LBrace)?.span;
        let mut stmts = Vec::new();
        while self.peek().kind != TokenKind::RBrace && !self.is_eof() {
            stmts.push(self.parse_stmt()?);
        }
        let end = self.expect(TokenKind::RBrace)?.span;
        Ok(Stmt::Block {
            stmts,
            span: Span {
                start: start.start,
                end: end.end,
            },
        })
    }

    pub(super) fn parse_if_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwIf)?.span;
        self.expect(TokenKind::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;

        let then_branch = Box::new(self.parse_stmt()?);

        let else_branch = if self.eat(TokenKind::KwElse) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        let end = self.prev_span();
        Ok(Stmt::If {
            cond,
            then_branch,
            else_branch,
            span: Span {
                start: start.start,
                end: end.end,
            },
        })
    }

    pub(super) fn parse_while_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwWhile)?.span;
        self.expect(TokenKind::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;

        let body = Box::new(self.parse_stmt()?);

        let end = self.prev_span();
        Ok(Stmt::While {
            cond,
            body,
            span: Span {
                start: start.start,
                end: end.end,
            },
        })
    }

    pub(super) fn parse_break_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwBreak)?.span;
        let semi = self.expect(TokenKind::Semicolon)?.span;
        Ok(Stmt::Break {
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    pub(super) fn parse_continue_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwContinue)?.span;
        let semi = self.expect(TokenKind::Semicolon)?.span;
        Ok(Stmt::Continue {
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    pub(super) fn parse_console_log_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.peek().span;
        let console = self.expect(TokenKind::Ident)?;
        if self.slice(console.span) != "console" {
            return Err(TszError::Parse {
                message: "Only console.log(...) is supported here".to_string(),
                span: console.span,
            });
        }

        self.expect(TokenKind::Dot)?;

        let log = self.expect(TokenKind::Ident)?;
        if self.slice(log.span) != "log" {
            return Err(TszError::Parse {
                message: "Only console.log(...) is supported here".to_string(),
                span: log.span,
            });
        }

        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();
        if self.peek().kind != TokenKind::RParen {
            loop {
                args.push(self.parse_expr()?);
                if self.eat(TokenKind::Comma) {
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::RParen)?;
        let semi = self.expect(TokenKind::Semicolon)?.span;

        Ok(Stmt::ConsoleLog {
            args,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    pub(super) fn parse_assign_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.peek().span;
        let name_tok = self.expect(TokenKind::Ident)?;
        let name_span = name_tok.span;
        let name = self.slice(name_tok.span).to_string();

        // `+=/-=/*=//=` are syntax sugar, desugared into a normal assignment with a binary RHS.
        let expr = match self.peek().kind {
            TokenKind::Equal => {
                self.bump(); // '='
                self.parse_expr()?
            }
            TokenKind::PlusEqual | TokenKind::MinusEqual | TokenKind::StarEqual | TokenKind::SlashEqual => {
                let op = match self.bump().kind {
                    TokenKind::PlusEqual => BinaryOp::Add,
                    TokenKind::MinusEqual => BinaryOp::Sub,
                    TokenKind::StarEqual => BinaryOp::Mul,
                    TokenKind::SlashEqual => BinaryOp::Div,
                    _ => unreachable!("matched compound assignment token"),
                };

                let rhs = self.parse_expr()?;
                let rhs_span = expr_span(&rhs);
                let span = Span {
                    start: name_span.start,
                    end: rhs_span.end,
                };
                Expr::Binary {
                    op,
                    left: Box::new(Expr::Ident {
                        name: name.clone(),
                        span: name_span,
                    }),
                    right: Box::new(rhs),
                    span,
                }
            }
            _ => {
                let got = self.peek();
                return Err(TszError::Parse {
                    message: "Expected assignment operator: =, +=, -=, *=, /=".to_string(),
                    span: got.span,
                });
            }
        };

        let semi = self.expect(TokenKind::Semicolon)?.span;
        Ok(Stmt::Assign {
            name,
            name_span,
            expr,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }
}
