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
        HirExpr::UnaryNot { expr, .. } => match try_eval_const_value(expr)? {
            ConstValue::Bool(v) => Some(ConstValue::Bool(!v)),
            ConstValue::Number(_) | ConstValue::BigInt(_) | ConstValue::String(_) => None,
        },
        HirExpr::Binary { op, left, right, .. } => eval_binary(*op, left, right),
        HirExpr::Param { .. } | HirExpr::Local { .. } | HirExpr::Call { .. } | HirExpr::Error { .. } => None,
    }
}

fn eval_binary(op: HirBinaryOp, left: &HirExpr, right: &HirExpr) -> Option<ConstValue> {
    let l = try_eval_const_value(left)?;
    let r = try_eval_const_value(right)?;
    match op {
        HirBinaryOp::Add | HirBinaryOp::Sub | HirBinaryOp::Mul | HirBinaryOp::Div => eval_arithmetic(op, l, r),
        HirBinaryOp::Eq | HirBinaryOp::Ne => eval_equality(op, l, r),
        HirBinaryOp::Lt | HirBinaryOp::Le | HirBinaryOp::Gt | HirBinaryOp::Ge => eval_relational(op, l, r),
        HirBinaryOp::And | HirBinaryOp::Or => eval_logical(op, l, r),
    }
}

fn eval_arithmetic(op: HirBinaryOp, left: ConstValue, right: ConstValue) -> Option<ConstValue> {
    match (left, right) {
        (ConstValue::Number(a), ConstValue::Number(b)) => Some(ConstValue::Number(match op {
            HirBinaryOp::Add => a + b,
            HirBinaryOp::Sub => a - b,
            HirBinaryOp::Mul => a * b,
            HirBinaryOp::Div => a / b,
            _ => unreachable!("checked arithmetic op"),
        })),
        (ConstValue::BigInt(a), ConstValue::BigInt(b)) => Some(ConstValue::BigInt(match op {
            HirBinaryOp::Add => a.wrapping_add(b),
            HirBinaryOp::Sub => a.wrapping_sub(b),
            HirBinaryOp::Mul => a.wrapping_mul(b),
            HirBinaryOp::Div => a.checked_div(b)?,
            _ => unreachable!("checked arithmetic op"),
        })),
        _ => None,
    }
}

fn eval_equality(op: HirBinaryOp, left: ConstValue, right: ConstValue) -> Option<ConstValue> {
    match (left, right) {
        (ConstValue::Number(a), ConstValue::Number(b)) => Some(ConstValue::Bool(match op {
            HirBinaryOp::Eq => a == b,
            HirBinaryOp::Ne => a != b,
            _ => unreachable!("checked eq op"),
        })),
        (ConstValue::BigInt(a), ConstValue::BigInt(b)) => Some(ConstValue::Bool(match op {
            HirBinaryOp::Eq => a == b,
            HirBinaryOp::Ne => a != b,
            _ => unreachable!("checked eq op"),
        })),
        (ConstValue::Bool(a), ConstValue::Bool(b)) => Some(ConstValue::Bool(match op {
            HirBinaryOp::Eq => a == b,
            HirBinaryOp::Ne => a != b,
            _ => unreachable!("checked eq op"),
        })),
        _ => None,
    }
}

fn eval_relational(op: HirBinaryOp, left: ConstValue, right: ConstValue) -> Option<ConstValue> {
    match (left, right) {
        (ConstValue::Number(a), ConstValue::Number(b)) => Some(ConstValue::Bool(match op {
            HirBinaryOp::Lt => a < b,
            HirBinaryOp::Le => a <= b,
            HirBinaryOp::Gt => a > b,
            HirBinaryOp::Ge => a >= b,
            _ => unreachable!("checked relational op"),
        })),
        (ConstValue::BigInt(a), ConstValue::BigInt(b)) => Some(ConstValue::Bool(match op {
            HirBinaryOp::Lt => a < b,
            HirBinaryOp::Le => a <= b,
            HirBinaryOp::Gt => a > b,
            HirBinaryOp::Ge => a >= b,
            _ => unreachable!("checked relational op"),
        })),
        _ => None,
    }
}

fn eval_logical(op: HirBinaryOp, left: ConstValue, right: ConstValue) -> Option<ConstValue> {
    match (left, right) {
        (ConstValue::Bool(a), ConstValue::Bool(b)) => Some(ConstValue::Bool(match op {
            HirBinaryOp::And => a && b,
            HirBinaryOp::Or => a || b,
            _ => unreachable!("checked logical op"),
        })),
        _ => None,
    }
}
