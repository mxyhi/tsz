use crate::lexer::TokenKind;
use crate::{Expr, Span};

use super::Parser;

impl<'a> Parser<'a, '_> {
    pub(super) fn parse_expr(&mut self) -> Expr {
        self.parse_logical_or_expr()
    }

    // Expression parsing (small TS subset):
    // - primary: literals / identifiers / calls / parenthesized expressions
    // - unary: -<expr> / !<expr>
    // - multiplicative: * /
    // - additive: + -
    // - relational: < <= > >=
    // - equality: == !=
    // - logical and: &&
    // - logical or: ||
    pub(super) fn parse_logical_or_expr(&mut self) -> Expr {
        let mut expr = self.parse_logical_and_expr();
        loop {
            if self.peek().kind != TokenKind::OrOr {
                break;
            }
            self.bump();
            let rhs = self.parse_logical_and_expr();
            let span = Span {
                start: expr_span(&expr).start,
                end: expr_span(&rhs).end,
            };
            expr = Expr::Binary {
                op: crate::BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(rhs),
                span,
            };
        }
        expr
    }

    pub(super) fn parse_logical_and_expr(&mut self) -> Expr {
        let mut expr = self.parse_equality_expr();
        loop {
            if self.peek().kind != TokenKind::AndAnd {
                break;
            }
            self.bump();
            let rhs = self.parse_equality_expr();
            let span = Span {
                start: expr_span(&expr).start,
                end: expr_span(&rhs).end,
            };
            expr = Expr::Binary {
                op: crate::BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(rhs),
                span,
            };
        }
        expr
    }

    pub(super) fn parse_equality_expr(&mut self) -> Expr {
        let mut expr = self.parse_relational_expr();
        loop {
            let op = match self.peek().kind {
                TokenKind::EqualEqual => Some(crate::BinaryOp::Eq),
                TokenKind::BangEqual => Some(crate::BinaryOp::Ne),
                _ => None,
            };
            let Some(op) = op else { break };
            self.bump();
            let rhs = self.parse_relational_expr();
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
        expr
    }

    pub(super) fn parse_relational_expr(&mut self) -> Expr {
        let mut expr = self.parse_additive_expr();
        loop {
            let op = match self.peek().kind {
                TokenKind::Less => Some(crate::BinaryOp::Lt),
                TokenKind::LessEqual => Some(crate::BinaryOp::Le),
                TokenKind::Greater => Some(crate::BinaryOp::Gt),
                TokenKind::GreaterEqual => Some(crate::BinaryOp::Ge),
                _ => None,
            };
            let Some(op) = op else { break };
            self.bump();
            let rhs = self.parse_additive_expr();
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
        expr
    }

    pub(super) fn parse_additive_expr(&mut self) -> Expr {
        let mut expr = self.parse_multiplicative_expr();
        loop {
            let op = match self.peek().kind {
                TokenKind::Plus => Some(crate::BinaryOp::Add),
                TokenKind::Minus => Some(crate::BinaryOp::Sub),
                _ => None,
            };
            let Some(op) = op else { break };
            self.bump();
            let rhs = self.parse_multiplicative_expr();
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
        expr
    }

    pub(super) fn parse_multiplicative_expr(&mut self) -> Expr {
        let mut expr = self.parse_unary_expr();
        loop {
            let op = match self.peek().kind {
                TokenKind::Star => Some(crate::BinaryOp::Mul),
                TokenKind::Slash => Some(crate::BinaryOp::Div),
                _ => None,
            };
            let Some(op) = op else { break };
            self.bump();
            let rhs = self.parse_unary_expr();
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
        expr
    }

    pub(super) fn parse_unary_expr(&mut self) -> Expr {
        if self.eat(TokenKind::Minus) {
            let minus_span = self.prev_span();
            let expr = self.parse_unary_expr();
            let span = Span {
                start: minus_span.start,
                end: expr_span(&expr).end,
            };
            return Expr::UnaryMinus {
                expr: Box::new(expr),
                span,
            };
        }
        if self.eat(TokenKind::Bang) {
            let bang_span = self.prev_span();
            let expr = self.parse_unary_expr();
            let span = Span {
                start: bang_span.start,
                end: expr_span(&expr).end,
            };
            return Expr::UnaryNot {
                expr: Box::new(expr),
                span,
            };
        }
        self.parse_primary_expr()
    }

    pub(super) fn parse_primary_expr(&mut self) -> Expr {
        let tok = self.peek();
        match tok.kind {
            TokenKind::Number => {
                let tok = self.bump();
                let s = self.slice(tok.span);
                let v: f64 = match s.parse() {
                    Ok(v) => v,
                    Err(_) => {
                        self.error_at(tok.span, format!("Invalid number literal: {s}"));
                        return Expr::Error { span: tok.span };
                    }
                };
                Expr::Number {
                    value: v,
                    span: tok.span,
                }
            }
            TokenKind::BigInt => {
                let tok = self.bump();
                let raw = self.slice(tok.span);
                let s = raw.strip_suffix('n').unwrap_or(raw);
                let v: i64 = match s.parse() {
                    Ok(v) => v,
                    Err(_) => {
                        self.error_at(tok.span, format!("Invalid bigint literal: {raw}"));
                        return Expr::Error { span: tok.span };
                    }
                };
                Expr::BigInt {
                    value: v,
                    span: tok.span,
                }
            }
            TokenKind::String => {
                let tok = self.bump();
                let Some(v) = self.parse_string_value(tok) else {
                    return Expr::Error { span: tok.span };
                };
                Expr::String {
                    value: v,
                    span: tok.span,
                }
            }
            TokenKind::Ident => {
                let ident = self.bump();
                let name = self.slice(ident.span).to_string();
                if name == "true" || name == "false" {
                    return Expr::Bool {
                        value: name == "true",
                        span: ident.span,
                    };
                }
                if self.eat(TokenKind::LParen) {
                    let mut args = Vec::new();
                    if self.peek().kind != TokenKind::RParen {
                        loop {
                            args.push(self.parse_expr());
                            if self.eat(TokenKind::Comma) {
                                if self.peek().kind == TokenKind::RParen {
                                    break;
                                }
                                continue;
                            }
                            break;
                        }
                    }
                    let rparen = self.expect(TokenKind::RParen).span;
                    Expr::Call {
                        callee: name,
                        args,
                        span: Span {
                            start: ident.span.start,
                            end: rparen.end,
                        },
                    }
                } else {
                    Expr::Ident {
                        name,
                        span: ident.span,
                    }
                }
            }
            TokenKind::LParen => {
                self.bump();
                let expr = self.parse_expr();
                self.expect(TokenKind::RParen);
                expr
            }
            TokenKind::Error => {
                let span = self.bump().span;
                Expr::Error { span }
            }
            _ => {
                self.error_at(tok.span, "Expected expression");
                let span = self.bump().span;
                Expr::Error { span }
            }
        }
    }
}

pub(super) fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Number { span, .. }
        | Expr::BigInt { span, .. }
        | Expr::Bool { span, .. }
        | Expr::String { span, .. } => *span,
        Expr::Ident { span, .. } => *span,
        Expr::UnaryMinus { span, .. } => *span,
        Expr::UnaryNot { span, .. } => *span,
        Expr::Call { span, .. } => *span,
        Expr::Binary { span, .. } => *span,
        Expr::Error { span } => *span,
    }
}
