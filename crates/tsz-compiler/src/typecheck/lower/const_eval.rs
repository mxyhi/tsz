use crate::{HirBinaryOp, HirExpr};

#[derive(Debug, Clone)]
pub(super) enum ConstValue {
    Number(f64),
    BigInt(i64),
    Bool(bool),
    String(String),
}

pub(super) fn try_eval_const_value(expr: &HirExpr) -> Option<ConstValue> {
    match expr {
        HirExpr::Number { value, .. } => Some(ConstValue::Number(*value)),
        HirExpr::BigInt { value, .. } => Some(ConstValue::BigInt(*value)),
        HirExpr::Bool { value, .. } => Some(ConstValue::Bool(*value)),
        HirExpr::String { value, .. } => Some(ConstValue::String(value.clone())),
        HirExpr::UnaryMinus { expr, .. } => match try_eval_const_value(expr)? {
            ConstValue::Number(v) => Some(ConstValue::Number(-v)),
            ConstValue::BigInt(v) => Some(ConstValue::BigInt(v.wrapping_neg())),
            ConstValue::Bool(_) | ConstValue::String(_) => None,
        },
        HirExpr::Binary { op, left, right, .. } => {
            let l = try_eval_const_value(left)?;
            let r = try_eval_const_value(right)?;
            match (l, r) {
                (ConstValue::Number(a), ConstValue::Number(b)) => Some(ConstValue::Number(match op {
                    HirBinaryOp::Add => a + b,
                    HirBinaryOp::Sub => a - b,
                    HirBinaryOp::Mul => a * b,
                    HirBinaryOp::Div => a / b,
                })),
                (ConstValue::BigInt(a), ConstValue::BigInt(b)) => Some(ConstValue::BigInt(match op {
                    HirBinaryOp::Add => a.wrapping_add(b),
                    HirBinaryOp::Sub => a.wrapping_sub(b),
                    HirBinaryOp::Mul => a.wrapping_mul(b),
                    HirBinaryOp::Div => a.checked_div(b)?,
                })),
                _ => None,
            }
        }
        HirExpr::Param { .. } | HirExpr::Local { .. } | HirExpr::Call { .. } => None,
    }
}

