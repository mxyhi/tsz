use crate::{Span, Type};
use std::path::PathBuf;

pub type HirFuncId = usize;
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

    /// Function-local variables (in declaration order).
    pub locals: Vec<HirLocal>,
    /// Sequential statements (current constraint: the last statement must be `return`, no control flow).
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
    /// Stdout: `console.log(a, b, c);`
    ConsoleLog { args: Vec<HirExpr>, span: Span },
    Return { expr: Option<HirExpr>, span: Span },
}

#[derive(Debug, Clone)]
pub enum HirExpr {
    Number { value: f64, span: Span },
    BigInt { value: i64, span: Span },
    String { value: String, span: Span },
    Local { local: HirLocalId, span: Span },
    UnaryMinus { expr: Box<HirExpr>, span: Span },
    Call { callee: HirFuncId, span: Span },
}
