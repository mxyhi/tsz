use crate::{Span, Type};
use std::path::PathBuf;

pub type HirFuncId = usize;
pub type HirParamId = usize;
pub type HirLocalId = usize;

#[derive(Debug, Clone)]
pub struct HirProgram {
    pub entry: HirFuncId,
    pub functions: Vec<HirFunction>,
}

#[derive(Debug, Clone)]
pub struct HirFunction {
    /// Unique symbol name for codegen (with internal mangling applied).
    pub symbol: String,
    pub return_type: Type,

    /// Function parameters (in declaration order).
    pub params: Vec<HirParam>,
    /// Function-local variables (in declaration order).
    pub locals: Vec<HirLocal>,
    /// Function body statements (structured control flow is allowed).
    pub body: Vec<HirStmt>,

    /// Only used for diagnostics and debugging.
    pub source: HirSourceInfo,
}

#[derive(Debug, Clone)]
pub struct HirLocal {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirParam {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirSourceInfo {
    pub module_path: PathBuf,
    pub name: String,
    pub is_export: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirStmt {
    /// Local variable declaration: `let <name> = <expr>;`
    Let {
        local: HirLocalId,
        init: HirExpr,
        span: Span,
    },
    /// Local variable assignment: `<name> = <expr>;`
    Assign {
        local: HirLocalId,
        value: HirExpr,
        span: Span,
    },
    /// Conditional: `if (cond) { ... } else { ... }`
    If {
        cond: HirExpr,
        then_body: Vec<HirStmt>,
        else_body: Option<Vec<HirStmt>>,
        span: Span,
    },
    /// Loop: `while (cond) { ... }`
    While {
        cond: HirExpr,
        body: Vec<HirStmt>,
        span: Span,
    },
    /// `break;` (only valid inside `while`)
    Break { span: Span },
    /// `continue;` (only valid inside `while`)
    Continue { span: Span },
    /// Stdout: `console.log(a, b, c);`
    ConsoleLog { args: Vec<HirExpr>, span: Span },
    Return { expr: Option<HirExpr>, span: Span },
}

#[derive(Debug, Clone)]
pub enum HirExpr {
    Number { value: f64, span: Span },
    BigInt { value: i64, span: Span },
    Bool { value: bool, span: Span },
    String { value: String, span: Span },
    Param { param: HirParamId, span: Span },
    Local { local: HirLocalId, span: Span },
    UnaryMinus { expr: Box<HirExpr>, span: Span },
    Call {
        callee: HirFuncId,
        args: Vec<HirExpr>,
        span: Span,
    },
    Binary {
        op: HirBinaryOp,
        left: Box<HirExpr>,
        right: Box<HirExpr>,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}
