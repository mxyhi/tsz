use crate::lexer::{Token, TokenKind};
use crate::{ast::*, lexer, Span, TszError};
use std::path::Path;

pub fn parse_module(path: &Path, source: &str) -> Result<Module, TszError> {
    let tokens = lexer::lex(source)?;
    let mut p = Parser::new(source, tokens);
    let mut functions = Vec::new();
    while !p.is_eof() {
        functions.push(p.parse_function()?);
    }
    Ok(Module {
        path: path.to_path_buf(),
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
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        self.expect(TokenKind::LBrace)?;
        let body = vec![self.parse_return_stmt()?];
        self.expect(TokenKind::RBrace)?;

        let end_span = self.prev_span();
        Ok(FunctionDecl {
            is_export,
            name,
            return_type,
            body,
            span: Span {
                start: start_span.start,
                end: end_span.end,
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
                message: format!("不支持的类型: {s}"),
                span: tok.span,
            }),
        }
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, TszError> {
        let start = self.expect(TokenKind::KwReturn)?.span;
        let expr = self.parse_expr()?;
        let semi = self.expect(TokenKind::Semicolon)?.span;
        Ok(Stmt::Return {
            expr,
            span: Span {
                start: start.start,
                end: semi.end,
            },
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, TszError> {
        if self.eat(TokenKind::Minus) {
            let minus_span = self.prev_span();
            let expr = self.parse_expr()?;
            let span = Span {
                start: minus_span.start,
                end: expr_span(&expr).end,
            };
            return Ok(Expr::UnaryMinus {
                expr: Box::new(expr),
                span,
            });
        }

        let tok = self.peek();
        match tok.kind {
            TokenKind::Number => {
                let tok = self.bump();
                let s = self.slice(tok.span);
                let v: f64 = s.parse().map_err(|_| TszError::Parse {
                    message: format!("无效的 number 字面量: {s}"),
                    span: tok.span,
                })?;
                Ok(Expr::Number { value: v, span: tok.span })
            }
            TokenKind::BigInt => {
                let tok = self.bump();
                let raw = self.slice(tok.span);
                let s = raw.strip_suffix('n').unwrap_or(raw);
                let v: i64 = s.parse().map_err(|_| TszError::Parse {
                    message: format!("无效的 bigint 字面量: {raw}"),
                    span: tok.span,
                })?;
                Ok(Expr::BigInt { value: v, span: tok.span })
            }
            _ => Err(TszError::Parse {
                message: "暂只支持 number/bigint 字面量表达式".to_string(),
                span: tok.span,
            }),
        }
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
                message: format!("期望 {kind:?}，但得到 {got:?}", got = t.kind),
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
        Expr::Number { span, .. } | Expr::BigInt { span, .. } => *span,
        Expr::UnaryMinus { span, .. } => *span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_minimal_main() {
        let src = "export function main(): bigint { return 42n; }";
        let m = parse_module(Path::new("main.tsz"), src).expect("parse ok");
        assert_eq!(m.functions.len(), 1);
        assert_eq!(m.functions[0].name, "main");
        assert_eq!(m.functions[0].return_type, Type::BigInt);
    }
}
