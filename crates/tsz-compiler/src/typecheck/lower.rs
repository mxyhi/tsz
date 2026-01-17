use super::FuncInfo;
use crate::{
    BinaryOp, Expr, HirBinaryOp, HirExpr, HirLocal, HirLocalId, HirParam, HirParamId, HirStmt, Span, TszError, Type,
};
use std::collections::HashMap;

mod assign;
mod const_eval;
mod flow;
mod stmt;

use const_eval::ConstValue;

#[derive(Debug, Clone)]
enum LocalValue {
    Param(HirParamId),
    Local(HirLocalId),
    Const(ConstValue),
}

#[derive(Debug, Clone)]
struct LocalInfo {
    value: LocalValue,
    ty: Type,
}

#[derive(Debug)]
struct LocalScopes {
    stack: Vec<HashMap<String, LocalInfo>>,
}

impl LocalScopes {
    fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn pop(&mut self) {
        // The function-level scope is never popped.
        if self.stack.len() <= 1 {
            return;
        }
        self.stack.pop();
    }

    fn lookup(&self, name: &str) -> Option<LocalInfo> {
        self.stack.iter().rev().find_map(|scope| scope.get(name).cloned())
    }

    fn insert_current(&mut self, name: String, info: LocalInfo, name_span: Span) -> Result<(), TszError> {
        let current = self.stack.last_mut().expect("at least one scope");
        if current.contains_key(&name) {
            return Err(TszError::Type {
                message: format!("Duplicate local binding declaration: {name}"),
                span: name_span,
            });
        }
        current.insert(name, info);
        Ok(())
    }
}

struct LowerCtx<'a> {
    module_idx: usize,
    module_scope: &'a HashMap<String, usize>,
    scopes: &'a [HashMap<String, usize>],
    func_infos: &'a [FuncInfo],
    return_type: Type,
}

struct LowerFnState {
    params: Vec<HirParam>,
    locals: Vec<HirLocal>,
    local_scopes: LocalScopes,
    loop_depth: usize,
}

impl LowerFnState {
    fn new(param_count: usize) -> Self {
        Self {
            params: Vec::with_capacity(param_count),
            locals: Vec::new(),
            local_scopes: LocalScopes::new(),
            loop_depth: 0,
        }
    }
}

pub(super) fn lower_function_body(
    module_idx: usize,
    func: &crate::FunctionDecl,
    module_scope: &HashMap<String, usize>,
    scopes: &[HashMap<String, usize>],
    func_infos: &[FuncInfo],
) -> Result<(Vec<HirParam>, Vec<HirLocal>, Vec<HirStmt>), TszError> {
    let ctx = LowerCtx {
        module_idx,
        module_scope,
        scopes,
        func_infos,
        return_type: func.return_type,
    };
    let mut state = LowerFnState::new(func.params.len());

    lower_params(&ctx, &mut state, &func.params)?;

    let mut body = Vec::with_capacity(func.body.len());
    stmt::lower_stmt_list(&ctx, &mut state, &mut body, &func.body)?;

    if flow::stmts_may_fallthrough(&body) {
        match func.return_type {
            Type::Void => body.push(HirStmt::Return { expr: None, span: func.span }),
            Type::Number | Type::BigInt | Type::Bool | Type::String => {
                return Err(TszError::Type {
                    message: "Not all control paths return a value".to_string(),
                    span: func.span,
                });
            }
        }
    }

    Ok((state.params, state.locals, body))
}

fn lower_params(ctx: &LowerCtx<'_>, state: &mut LowerFnState, params: &[crate::ParamDecl]) -> Result<(), TszError> {
    for p in params {
        // Keep `foo` vs `foo()` unambiguous: parameters cannot shadow module-level symbols.
        if ctx.module_scope.contains_key(&p.name) {
            return Err(TszError::Type {
                message: format!("Parameter name conflicts with a function/import: {}", p.name),
                span: p.name_span,
            });
        }

        let param_id = state.params.len();
        state.params.push(HirParam {
            name: p.name.clone(),
            ty: p.ty,
            span: p.span,
        });

        state.local_scopes.insert_current(
            p.name.clone(),
            LocalInfo {
                value: LocalValue::Param(param_id),
                ty: p.ty,
            },
            p.name_span,
        )?;
    }
    Ok(())
}

fn validate_local_decl_type(ty: Type, span: Span) -> Result<(), TszError> {
    match ty {
        Type::Number | Type::BigInt | Type::Bool | Type::String => Ok(()),
        Type::Void => Err(TszError::Type {
            message: "Local variable type cannot be void".to_string(),
            span,
        }),
    }
}

fn validate_local_value_type(ty: Type, span: Span) -> Result<(), TszError> {
    match ty {
        Type::Number | Type::BigInt | Type::Bool | Type::String => Ok(()),
        Type::Void => Err(TszError::Type {
            message: "Local initializer expression cannot be void".to_string(),
            span,
        }),
    }
}

fn lower_expr_in_function(
    module_idx: usize,
    expr: &Expr,
    scopes: &[HashMap<String, usize>],
    func_infos: &[FuncInfo],
    locals: &LocalScopes,
) -> Result<(HirExpr, Type), TszError> {
    match expr {
        Expr::Number { value, span } => Ok((HirExpr::Number { value: *value, span: *span }, Type::Number)),
        Expr::BigInt { value, span } => Ok((HirExpr::BigInt { value: *value, span: *span }, Type::BigInt)),
        Expr::Bool { value, span } => Ok((HirExpr::Bool { value: *value, span: *span }, Type::Bool)),
        Expr::String { value, span } => Ok((HirExpr::String { value: value.clone(), span: *span }, Type::String)),
        Expr::Ident { name, span } => {
            let Some(info) = locals.lookup(name) else {
                return Err(TszError::Type {
                    message: format!("Undefined variable: {name}"),
                    span: *span,
                });
            };
            match info.value {
                LocalValue::Param(id) => Ok((HirExpr::Param { param: id, span: *span }, info.ty)),
                LocalValue::Local(id) => Ok((HirExpr::Local { local: id, span: *span }, info.ty)),
                LocalValue::Const(v) => match v {
                    ConstValue::Number(value) => Ok((HirExpr::Number { value, span: *span }, Type::Number)),
                    ConstValue::BigInt(value) => Ok((HirExpr::BigInt { value, span: *span }, Type::BigInt)),
                    ConstValue::Bool(value) => Ok((HirExpr::Bool { value, span: *span }, Type::Bool)),
                    ConstValue::String(value) => Ok((HirExpr::String { value, span: *span }, Type::String)),
                },
            }
        }
        Expr::UnaryMinus { expr, span } => {
            let (inner, ty) = lower_expr_in_function(module_idx, expr, scopes, func_infos, locals)?;
            match ty {
                Type::Number | Type::BigInt => Ok((
                    HirExpr::UnaryMinus {
                        expr: Box::new(inner),
                        span: *span,
                    },
                    ty,
                )),
                Type::Void | Type::Bool | Type::String => Err(TszError::Type {
                    message: "Unary minus is not supported for this type".to_string(),
                    span: *span,
                }),
            }
        }
        Expr::UnaryNot { expr, span } => {
            let (inner, ty) = lower_expr_in_function(module_idx, expr, scopes, func_infos, locals)?;
            if ty != Type::Bool {
                return Err(TszError::Type {
                    message: "Logical not only supports boolean".to_string(),
                    span: *span,
                });
            }
            Ok((
                HirExpr::UnaryNot {
                    expr: Box::new(inner),
                    span: *span,
                },
                Type::Bool,
            ))
        }
        Expr::Call { callee, args, span } => {
            let scope = scopes.get(module_idx).ok_or_else(|| TszError::Type {
                message: "Module scope index out of bounds (internal error)".to_string(),
                span: *span,
            })?;
            let callee_id = scope.get(callee).copied().ok_or_else(|| TszError::Type {
                message: format!("Undefined function: {callee}"),
                span: *span,
            })?;
            let callee_info = func_infos
                .get(callee_id)
                .ok_or_else(|| TszError::Type {
                    message: "Function index out of bounds (internal error)".to_string(),
                    span: *span,
                })?;

            if args.len() != callee_info.params.len() {
                return Err(TszError::Type {
                    message: format!(
                        "Argument count mismatch for {callee}: expected {}, got {}",
                        callee_info.params.len(),
                        args.len()
                    ),
                    span: *span,
                });
            }

            let mut hir_args = Vec::with_capacity(args.len());
            for (idx, arg) in args.iter().enumerate() {
                let (hir, ty) = lower_expr_in_function(module_idx, arg, scopes, func_infos, locals)?;
                let expected = callee_info.params[idx];
                if ty != expected {
                    return Err(TszError::Type {
                        message: format!(
                            "Argument type mismatch for {callee} (arg {idx}): expected {:?}, got {:?}",
                            expected, ty
                        ),
                        span: ast_expr_span(arg),
                    });
                }
                hir_args.push(hir);
            }

            Ok((
                HirExpr::Call {
                    callee: callee_id,
                    args: hir_args,
                    span: *span,
                },
                callee_info.return_type,
            ))
        }
        Expr::Binary { op, left, right, span } => lower_binary_expr(module_idx, op, left, right, *span, scopes, func_infos, locals),
    }
}

fn lower_console_log_args(
    module_idx: usize,
    args: &[Expr],
    scopes: &[HashMap<String, usize>],
    func_infos: &[FuncInfo],
    locals: &LocalScopes,
    span: Span,
) -> Result<Vec<HirExpr>, TszError> {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
        let (hir, ty) = lower_expr_in_function(module_idx, arg, scopes, func_infos, locals)?;
        match ty {
            Type::Number | Type::BigInt | Type::Bool | Type::String => {}
            Type::Void => {
                return Err(TszError::Type {
                    message: "console.log arguments cannot be void".to_string(),
                    span,
                });
            }
        }
        out.push(hir);
    }
    Ok(out)
}

fn ast_expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Number { span, .. }
        | Expr::BigInt { span, .. }
        | Expr::Bool { span, .. }
        | Expr::String { span, .. }
        | Expr::Ident { span, .. }
        | Expr::UnaryMinus { span, .. }
        | Expr::UnaryNot { span, .. }
        | Expr::Call { span, .. }
        | Expr::Binary { span, .. } => *span,
    }
}

fn lower_binary_expr(
    module_idx: usize,
    op: &BinaryOp,
    left: &Expr,
    right: &Expr,
    span: Span,
    scopes: &[HashMap<String, usize>],
    func_infos: &[FuncInfo],
    locals: &LocalScopes,
) -> Result<(HirExpr, Type), TszError> {
    let (hir_left, left_ty) = lower_expr_in_function(module_idx, left, scopes, func_infos, locals)?;
    let (hir_right, right_ty) = lower_expr_in_function(module_idx, right, scopes, func_infos, locals)?;
    if left_ty != right_ty {
        return Err(TszError::Type {
            message: format!(
                "Binary operator type mismatch: left is {:?}, right is {:?}",
                left_ty, right_ty
            ),
            span,
        });
    }

    match *op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            lower_arithmetic_binary(*op, hir_left, hir_right, left_ty, span)
        }
        BinaryOp::Eq | BinaryOp::Ne => lower_equality_binary(*op, hir_left, hir_right, left_ty, span),
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            lower_relational_binary(*op, hir_left, hir_right, left_ty, span)
        }
        BinaryOp::And | BinaryOp::Or => lower_logical_binary(*op, hir_left, hir_right, left_ty, span),
    }
}

fn lower_arithmetic_binary(
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    match left_ty {
        Type::Number | Type::BigInt => {}
        Type::Void | Type::Bool | Type::String => {
            return Err(TszError::Type {
                message: "Binary operators only support number/bigint".to_string(),
                span,
            });
        }
    }
    let hir_op = match op {
        BinaryOp::Add => HirBinaryOp::Add,
        BinaryOp::Sub => HirBinaryOp::Sub,
        BinaryOp::Mul => HirBinaryOp::Mul,
        BinaryOp::Div => HirBinaryOp::Div,
        _ => unreachable!("checked arithmetic op"),
    };
    Ok((
        HirExpr::Binary {
            op: hir_op,
            left: Box::new(left),
            right: Box::new(right),
            span,
        },
        left_ty,
    ))
}

fn lower_equality_binary(
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    match left_ty {
        Type::Number | Type::BigInt | Type::Bool => {}
        Type::String | Type::Void => {
            return Err(TszError::Type {
                message: "Equality only supports number/bigint/boolean".to_string(),
                span,
            });
        }
    }
    let hir_op = match op {
        BinaryOp::Eq => HirBinaryOp::Eq,
        BinaryOp::Ne => HirBinaryOp::Ne,
        _ => unreachable!("checked eq op"),
    };
    Ok((
        HirExpr::Binary {
            op: hir_op,
            left: Box::new(left),
            right: Box::new(right),
            span,
        },
        Type::Bool,
    ))
}

fn lower_relational_binary(
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    match left_ty {
        Type::Number | Type::BigInt => {}
        Type::Bool | Type::String | Type::Void => {
            return Err(TszError::Type {
                message: "Relational operators only support number/bigint".to_string(),
                span,
            });
        }
    }
    let hir_op = match op {
        BinaryOp::Lt => HirBinaryOp::Lt,
        BinaryOp::Le => HirBinaryOp::Le,
        BinaryOp::Gt => HirBinaryOp::Gt,
        BinaryOp::Ge => HirBinaryOp::Ge,
        _ => unreachable!("checked relational op"),
    };
    Ok((
        HirExpr::Binary {
            op: hir_op,
            left: Box::new(left),
            right: Box::new(right),
            span,
        },
        Type::Bool,
    ))
}

fn lower_logical_binary(
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    if left_ty != Type::Bool {
        return Err(TszError::Type {
            message: "Logical operators only support boolean".to_string(),
            span,
        });
    }
    let hir_op = match op {
        BinaryOp::And => HirBinaryOp::And,
        BinaryOp::Or => HirBinaryOp::Or,
        _ => unreachable!("checked logical op"),
    };
    Ok((
        HirExpr::Binary {
            op: hir_op,
            left: Box::new(left),
            right: Box::new(right),
            span,
        },
        Type::Bool,
    ))
}
