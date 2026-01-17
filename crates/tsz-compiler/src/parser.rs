use crate::lexer::{Token, TokenKind};
use crate::{ast::*, lexer, Span, TszError};
use std::path::Path;

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
        self.expect(TokenKind::RParen)?;

        self.expect(TokenKind::Colon)?;
        let return_type = self.parse_type()?;

        self.expect(TokenKind::LBrace)?;
        let mut body = Vec::new();
        while self.peek().kind != TokenKind::RBrace && !self.is_eof() {
            body.push(self.parse_stmt()?);
        }
        if body.is_empty() {
            return Err(TszError::Parse {
                message: "Empty function body (the current minimal subset requires a return)".to_string(),
                span: self.peek().span,
            });
        }
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

    fn parse_stmt(&mut self) -> Result<Stmt, TszError> {
        match self.peek().kind {
            TokenKind::KwReturn => self.parse_return_stmt(),
            TokenKind::KwLet => self.parse_let_stmt(),
            TokenKind::Ident => self.parse_console_log_stmt(),
            _ => Err(TszError::Parse {
                message: "Only let/console.log(...)/return statements are supported".to_string(),
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

    fn parse_console_log_stmt(&mut self) -> Result<Stmt, TszError> {
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
                if self.eat(TokenKind::LParen) {
                    let rparen = self.expect(TokenKind::RParen)?.span;
                    Ok(Expr::Call {
                        callee: name,
                        span: Span {
                            start: ident.span.start,
                            end: rparen.end,
                        },
                    })
                } else {
                    Ok(Expr::Ident { name, span: ident.span })
                }
            }
            _ => Err(TszError::Parse {
                message: "Currently only literals/identifiers/0-arg calls/unary minus are supported".to_string(),
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
        Expr::Number { span, .. } | Expr::BigInt { span, .. } | Expr::String { span, .. } => *span,
        Expr::Ident { span, .. } => *span,
        Expr::UnaryMinus { span, .. } => *span,
        Expr::Call { span, .. } => *span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_minimal_main() {
        let src = "export function main(): bigint { return 42n; }";
        let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
        assert_eq!(m.imports.len(), 0);
        assert_eq!(m.functions.len(), 1);
        assert_eq!(m.functions[0].name, "main");
        assert_eq!(m.functions[0].return_type, Type::BigInt);
    }

    #[test]
    fn parse_import_and_call() {
        let src = r#"
import { foo, bar } from "./lib.ts";
export function main(): bigint { return foo(); }
"#;
        let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
        assert_eq!(m.imports.len(), 1);
        assert_eq!(m.imports[0].names.len(), 2);
        assert_eq!(m.imports[0].names[0].name, "foo");
        assert_eq!(m.imports[0].names[1].name, "bar");
        assert_eq!(m.imports[0].from, "./lib.ts");
        assert_eq!(m.functions.len(), 1);
    }

    #[test]
    fn parse_void_return_stmt() {
        let src = "export function main(): void { return; }";
        let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
        let Some(Stmt::Return { expr, .. }) = m.functions[0].body.first() else {
            panic!("expected return stmt");
        };
        assert!(expr.is_none());
    }

    #[test]
    fn parse_console_log_stmt() {
        let src = r#"
export function main(): void {
  console.log("hi", 1, 2n);
  return;
}
"#;
        let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
        assert_eq!(m.functions.len(), 1);
        assert_eq!(m.functions[0].body.len(), 2);

        let Some(Stmt::ConsoleLog { args, .. }) = m.functions[0].body.first() else {
            panic!("expected console.log stmt");
        };
        assert_eq!(args.len(), 3);
    }

    #[test]
    fn parse_let_stmt() {
        let src = r#"
export function main(): bigint {
  let x: bigint = 1n;
  let y = -x;
  return y;
}
"#;
        let m = parse_module(Path::new("main.ts"), src).expect("parse ok");
        assert_eq!(m.functions.len(), 1);
        assert_eq!(m.functions[0].body.len(), 3);
    }
}
