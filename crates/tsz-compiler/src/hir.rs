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
    /// 代码生成使用的唯一符号名（已做内部 mangling）。
    pub symbol: String,
    pub return_type: Type,

    /// 函数内局部变量（按声明顺序）。
    pub locals: Vec<HirLocal>,
    /// 顺序语句（当前约束：最后一条必须是 `return`，无控制流）。
    pub body: Vec<HirStmt>,

    /// 仅用于诊断与调试。
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
    /// 局部变量声明：`let <name> = <expr>;`
    Let {
        local: HirLocalId,
        init: HirExpr,
        span: Span,
    },
    /// 标准输出：`console.log(a, b, c);`
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

