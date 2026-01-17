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
    pub params: Vec<ParamDecl>,
    pub return_type: Type,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParamDecl {
    pub name: String,
    pub name_span: Span,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return { expr: Option<Expr>, span: Span },
    /// Stdout: `console.log(a, b, c);`
    ConsoleLog { args: Vec<Expr>, span: Span },
    /// Local variable declaration: `let <name>: <type>? = <expr>;`
    Let {
        name: String,
        name_span: Span,
        annotated_type: Option<Type>,
        expr: Expr,
        span: Span,
    },
    /// Local constant declaration: `const <name>: <type>? = <expr>;`
    Const {
        name: String,
        name_span: Span,
        annotated_type: Option<Type>,
        expr: Expr,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number { value: f64, span: Span },
    BigInt { value: i64, span: Span },
    String { value: String, span: Span },
    Ident { name: String, span: Span },
    UnaryMinus { expr: Box<Expr>, span: Span },
    Call { callee: String, args: Vec<Expr>, span: Span },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    BigInt,
    Void,
    Bool,
    String,
}
