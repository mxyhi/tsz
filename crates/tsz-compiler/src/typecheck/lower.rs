use super::FuncInfo;
use crate::diagnostics::Diagnostics;
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

    fn insert_current(
        &mut self,
        name: String,
        info: LocalInfo,
        name_span: Span,
        diags: &mut Diagnostics,
        module_path: &std::path::Path,
    ) -> bool {
        let current = self.stack.last_mut().expect("at least one scope");
        if current.contains_key(&name) {
            diags.error_at(module_path, name_span, format!("Duplicate local binding declaration: {name}"));
            return false;
        }
        current.insert(name, info);
        true
    }
}

struct LowerCtx<'a> {
    module_idx: usize,
    module_path: &'a std::path::Path,
    module_scope: &'a HashMap<String, usize>,
    scopes: &'a [HashMap<String, usize>],
    func_infos: &'a [FuncInfo],
    return_type: Type,
    diags: &'a mut Diagnostics,
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
    module_path: &std::path::Path,
    module_scope: &HashMap<String, usize>,
    scopes: &[HashMap<String, usize>],
    func_infos: &[FuncInfo],
    diags: &mut Diagnostics,
) -> Result<(Vec<HirParam>, Vec<HirLocal>, Vec<HirStmt>), TszError> {
    let mut ctx = LowerCtx {
        module_idx,
        module_path,
        module_scope,
        scopes,
        func_infos,
        return_type: func.return_type,
        diags,
    };
    let mut state = LowerFnState::new(func.params.len());

    lower_params(&mut ctx, &mut state, &func.params)?;

    let mut body = Vec::with_capacity(func.body.len());
    stmt::lower_stmt_list(&mut ctx, &mut state, &mut body, &func.body)?;

    if flow::stmts_may_fallthrough(&body) {
        match func.return_type {
            Type::Void => body.push(HirStmt::Return { expr: None, span: func.span }),
            Type::Number | Type::BigInt | Type::Bool | Type::String | Type::Error => {
                ctx.diags.error_at(
                    ctx.module_path,
                    func.span,
                    "Not all control paths return a value",
                );
            }
        }
    }

    Ok((state.params, state.locals, body))
}

fn lower_params(ctx: &mut LowerCtx<'_>, state: &mut LowerFnState, params: &[crate::ParamDecl]) -> Result<(), TszError> {
    for p in params {
        // Keep `foo` vs `foo()` unambiguous: parameters cannot shadow module-level symbols.
        if ctx.module_scope.contains_key(&p.name) {
            ctx.diags
                .error_at(ctx.module_path, p.name_span, format!("Parameter name conflicts with a function/import: {}", p.name));
            continue;
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
            ctx.diags,
            ctx.module_path,
        );
    }
    Ok(())
}

fn validate_local_decl_type(ctx: &mut LowerCtx<'_>, ty: Type, span: Span) -> bool {
    match ty {
        Type::Number | Type::BigInt | Type::Bool | Type::String => true,
        Type::Void => {
            ctx.diags
                .error_at(ctx.module_path, span, "Local variable type cannot be void");
            false
        }
        Type::Error => false,
    }
}

fn validate_local_value_type(ctx: &mut LowerCtx<'_>, ty: Type, span: Span) -> bool {
    match ty {
        Type::Number | Type::BigInt | Type::Bool | Type::String => true,
        Type::Void => {
            ctx.diags
                .error_at(ctx.module_path, span, "Local initializer expression cannot be void");
            false
        }
        Type::Error => false,
    }
}

fn lower_expr_in_function(
    ctx: &mut LowerCtx<'_>,
    expr: &Expr,
    locals: &LocalScopes,
) -> Result<(HirExpr, Type), TszError> {
    match expr {
        Expr::Number { value, span } => Ok((HirExpr::Number { value: *value, span: *span }, Type::Number)),
        Expr::BigInt { value, span } => Ok((HirExpr::BigInt { value: *value, span: *span }, Type::BigInt)),
        Expr::Bool { value, span } => Ok((HirExpr::Bool { value: *value, span: *span }, Type::Bool)),
        Expr::String { value, span } => Ok((HirExpr::String { value: value.clone(), span: *span }, Type::String)),
        Expr::Ident { name, span } => {
            let Some(info) = locals.lookup(name) else {
                ctx.diags
                    .error_at(ctx.module_path, *span, format!("Undefined variable: {name}"));
                return Ok((HirExpr::Error { span: *span }, Type::Error));
            };
            match info.value {
                LocalValue::Param(id) => Ok((HirExpr::Param { param: id, span: *span }, info.ty)),
                LocalValue::Local(id) => Ok((HirExpr::Local { local: id, span: *span }, info.ty)),
                LocalValue::Const(v) => {
                    if info.ty == Type::Error {
                        return Ok((HirExpr::Error { span: *span }, Type::Error));
                    }
                    match v {
                        ConstValue::Number(value) => Ok((HirExpr::Number { value, span: *span }, Type::Number)),
                        ConstValue::BigInt(value) => Ok((HirExpr::BigInt { value, span: *span }, Type::BigInt)),
                        ConstValue::Bool(value) => Ok((HirExpr::Bool { value, span: *span }, Type::Bool)),
                        ConstValue::String(value) => Ok((HirExpr::String { value, span: *span }, Type::String)),
                    }
                }
            }
        }
        Expr::UnaryMinus { expr, span } => {
            let (inner, ty) = lower_expr_in_function(ctx, expr, locals)?;
            if ty == Type::Error {
                return Ok((HirExpr::Error { span: *span }, Type::Error));
            }
            match ty {
                Type::Number | Type::BigInt => Ok((
                    HirExpr::UnaryMinus {
                        expr: Box::new(inner),
                        span: *span,
                    },
                    ty,
                )),
                Type::Void | Type::Bool | Type::String => {
                    ctx.diags
                        .error_at(ctx.module_path, *span, "Unary minus is not supported for this type");
                    Ok((HirExpr::Error { span: *span }, Type::Error))
                }
                Type::Error => Ok((HirExpr::Error { span: *span }, Type::Error)),
            }
        }
        Expr::UnaryNot { expr, span } => {
            let (inner, ty) = lower_expr_in_function(ctx, expr, locals)?;
            if ty == Type::Error {
                return Ok((HirExpr::Error { span: *span }, Type::Error));
            }
            if ty != Type::Bool {
                ctx.diags
                    .error_at(ctx.module_path, *span, "Logical not only supports boolean");
                return Ok((HirExpr::Error { span: *span }, Type::Error));
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
            let scope = ctx.scopes.get(ctx.module_idx).ok_or_else(|| TszError::Type {
                message: "Module scope index out of bounds (internal error)".to_string(),
                span: *span,
            })?;
            let Some(callee_id) = scope.get(callee).copied() else {
                ctx.diags
                    .error_at(ctx.module_path, *span, format!("Undefined function: {callee}"));
                return Ok((HirExpr::Error { span: *span }, Type::Error));
            };
            let callee_info = ctx
                .func_infos
                .get(callee_id)
                .ok_or_else(|| TszError::Type {
                    message: "Function index out of bounds (internal error)".to_string(),
                    span: *span,
                })?;

            let mut had_error = false;
            if args.len() != callee_info.params.len() {
                ctx.diags.error_at(
                    ctx.module_path,
                    *span,
                    format!(
                        "Argument count mismatch for {callee}: expected {}, got {}",
                        callee_info.params.len(),
                        args.len()
                    ),
                );
                had_error = true;
            }

            let mut hir_args = Vec::with_capacity(args.len());
            for (idx, arg) in args.iter().enumerate() {
                let (hir, ty) = lower_expr_in_function(ctx, arg, locals)?;
                if let Some(expected) = callee_info.params.get(idx).copied() {
                    if ty == Type::Error {
                        had_error = true;
                    } else if ty != expected {
                        ctx.diags.error_at(
                            ctx.module_path,
                            ast_expr_span(arg),
                            format!(
                                "Argument type mismatch for {callee} (arg {idx}): expected {:?}, got {:?}",
                                expected, ty
                            ),
                        );
                        had_error = true;
                    }
                } else {
                    had_error = true;
                }
                hir_args.push(hir);
            }

            if had_error {
                return Ok((HirExpr::Error { span: *span }, Type::Error));
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
        Expr::Binary { op, left, right, span } => lower_binary_expr(ctx, op, left, right, *span, locals),
        Expr::Error { span } => Ok((HirExpr::Error { span: *span }, Type::Error)),
    }
}

fn lower_console_log_args(
    ctx: &mut LowerCtx<'_>,
    args: &[Expr],
    locals: &LocalScopes,
    span: Span,
) -> Result<Vec<HirExpr>, TszError> {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
        let (hir, ty) = lower_expr_in_function(ctx, arg, locals)?;
        match ty {
            Type::Number | Type::BigInt | Type::Bool | Type::String => {}
            Type::Void => {
                ctx.diags
                    .error_at(ctx.module_path, span, "console.log arguments cannot be void");
            }
            Type::Error => {}
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
        | Expr::Binary { span, .. }
        | Expr::Error { span } => *span,
    }
}

fn lower_binary_expr(
    ctx: &mut LowerCtx<'_>,
    op: &BinaryOp,
    left: &Expr,
    right: &Expr,
    span: Span,
    locals: &LocalScopes,
) -> Result<(HirExpr, Type), TszError> {
    let (hir_left, left_ty) = lower_expr_in_function(ctx, left, locals)?;
    let (hir_right, right_ty) = lower_expr_in_function(ctx, right, locals)?;
    if left_ty == Type::Error || right_ty == Type::Error {
        return Ok((HirExpr::Error { span }, Type::Error));
    }
    if left_ty != right_ty {
        ctx.diags.error_at(
            ctx.module_path,
            span,
            format!(
                "Binary operator type mismatch: left is {:?}, right is {:?}",
                left_ty, right_ty
            ),
        );
        return Ok((HirExpr::Error { span }, Type::Error));
    }

    match *op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            lower_arithmetic_binary(ctx, *op, hir_left, hir_right, left_ty, span)
        }
        BinaryOp::Eq | BinaryOp::Ne => lower_equality_binary(ctx, *op, hir_left, hir_right, left_ty, span),
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            lower_relational_binary(ctx, *op, hir_left, hir_right, left_ty, span)
        }
        BinaryOp::And | BinaryOp::Or => lower_logical_binary(ctx, *op, hir_left, hir_right, left_ty, span),
    }
}

fn lower_arithmetic_binary(
    ctx: &mut LowerCtx<'_>,
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    match left_ty {
        Type::Number | Type::BigInt => {}
        Type::Void | Type::Bool | Type::String => {
            ctx.diags
                .error_at(ctx.module_path, span, "Binary operators only support number/bigint");
            return Ok((HirExpr::Error { span }, Type::Error));
        }
        Type::Error => return Ok((HirExpr::Error { span }, Type::Error)),
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
    ctx: &mut LowerCtx<'_>,
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    match left_ty {
        Type::Number | Type::BigInt | Type::Bool => {}
        Type::String | Type::Void => {
            ctx.diags
                .error_at(ctx.module_path, span, "Equality only supports number/bigint/boolean");
            return Ok((HirExpr::Error { span }, Type::Error));
        }
        Type::Error => return Ok((HirExpr::Error { span }, Type::Error)),
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
    ctx: &mut LowerCtx<'_>,
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    match left_ty {
        Type::Number | Type::BigInt => {}
        Type::Bool | Type::String | Type::Void => {
            ctx.diags
                .error_at(ctx.module_path, span, "Relational operators only support number/bigint");
            return Ok((HirExpr::Error { span }, Type::Error));
        }
        Type::Error => return Ok((HirExpr::Error { span }, Type::Error)),
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
    ctx: &mut LowerCtx<'_>,
    op: BinaryOp,
    left: HirExpr,
    right: HirExpr,
    left_ty: Type,
    span: Span,
) -> Result<(HirExpr, Type), TszError> {
    if left_ty != Type::Bool {
        if left_ty != Type::Error {
            ctx.diags
                .error_at(ctx.module_path, span, "Logical operators only support boolean");
        }
        return Ok((HirExpr::Error { span }, Type::Error));
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
