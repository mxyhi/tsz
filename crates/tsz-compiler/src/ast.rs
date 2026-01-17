use crate::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub entry: std::path::PathBuf,
    pub modules: Vec<Module>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub path: std::path::PathBuf,
    pub imports: Vec<ImportDecl>,
    pub functions: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub names: Vec<ImportName>,
    pub from: String,
    pub resolved_path: Option<std::path::PathBuf>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ImportName {
    pub name: String,
    pub span: Span,
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
    Return { expr: Option<Expr>, span: Span },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number { value: f64, span: Span },
    BigInt { value: i64, span: Span },
    UnaryMinus { expr: Box<Expr>, span: Span },
    Call { callee: String, span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    BigInt,
    Void,
    Bool,
    String,
}
