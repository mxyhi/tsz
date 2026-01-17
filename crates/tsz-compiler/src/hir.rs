use crate::{Span, Type};
use std::path::PathBuf;

pub type HirFuncId = usize;

#[derive(Debug, Clone)]
pub struct HirProgram {
    pub entry: HirFuncId,
    pub functions: Vec<HirFunction>,
}

#[derive(Debug, Clone)]
pub struct HirFunction {
    /// 代码生成使用的唯一符号名（已做内部 mangling）。
    pub symbol: String,
    pub return_type: Type,
    pub body: HirBody,

    /// 仅用于诊断与调试。
    pub source: HirSourceInfo,
}

#[derive(Debug, Clone)]
pub struct HirSourceInfo {
    pub module_path: PathBuf,
    pub name: String,
    pub is_export: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirBody {
    pub logs: Vec<HirConsoleLog>,
    pub ret: HirReturn,
}

#[derive(Debug, Clone)]
pub struct HirConsoleLog {
    pub args: Vec<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirReturn {
    pub expr: Option<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirExpr {
    Number { value: f64, span: Span },
    BigInt { value: i64, span: Span },
    String { value: String, span: Span },
    UnaryMinus { expr: Box<HirExpr>, span: Span },
    Call { callee: HirFuncId, span: Span },
}
