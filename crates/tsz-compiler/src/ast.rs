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
    /// Block statement: `{ ... }` (introduces a new lexical scope).
    Block { stmts: Vec<Stmt>, span: Span },
    /// Conditional: `if (<cond>) <then> else <else>`
    If {
        cond: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
    /// Loop: `while (<cond>) <body>`
    While { cond: Expr, body: Box<Stmt>, span: Span },
    /// Loop: `for (<init>; <cond>; <update>) <body>`
    ///
    /// Notes (TSZ subset):
    /// - `<init>` supports `let/const/<name> = <expr>` or empty.
    /// - `<cond>` is a boolean expression, or empty (treated as `true`).
    /// - `<update>` supports `<name> = <expr>` (incl. `+=` etc) or empty.
    For {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        update: Option<Box<Stmt>>,
        body: Box<Stmt>,
        span: Span,
    },
    /// `break;` (only valid inside loops: `while` / `for`)
    Break { span: Span },
    /// `continue;` (only valid inside loops: `while` / `for`)
    Continue { span: Span },
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
    /// Assignment: `<name> = <expr>;` (only `let` locals are assignable).
    ///
    /// Compound assignments (`+=` / `-=` / `*=` / `/=`) are parser-level sugar
    /// and are desugared into a normal assignment with a binary expression RHS.
    Assign {
        name: String,
        name_span: Span,
        expr: Expr,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number { value: f64, span: Span },
    BigInt { value: i64, span: Span },
    Bool { value: bool, span: Span },
    String { value: String, span: Span },
    Ident { name: String, span: Span },
    UnaryMinus { expr: Box<Expr>, span: Span },
    UnaryNot { expr: Box<Expr>, span: Span },
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
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    BigInt,
    Void,
    Bool,
    String,
}
