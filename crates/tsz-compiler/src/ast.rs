use crate::Span;

#[derive(Debug, Clone)]
pub struct Module {
    pub path: std::path::PathBuf,
    pub functions: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub is_export: bool,
    pub name: String,
    pub return_type: Type,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return { expr: Expr, span: Span },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number { value: f64, span: Span },
    BigInt { value: i64, span: Span },
    UnaryMinus { expr: Box<Expr>, span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    BigInt,
    Void,
    Bool,
    String,
}
