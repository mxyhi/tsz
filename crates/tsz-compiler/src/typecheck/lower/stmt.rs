use crate::{Expr, HirLocal, HirStmt, Span, Stmt, TszError, Type};

use super::assign;
use super::const_eval::try_eval_const_value;
use super::{
    ast_expr_span, lower_console_log_args, lower_expr_in_function, validate_local_decl_type, validate_local_value_type,
    LocalInfo, LocalValue, LowerCtx, LowerFnState,
};

pub(super) fn lower_stmt_list(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    stmts: &[Stmt],
) -> Result<bool, TszError> {
    let mut terminated = false;
    for stmt in stmts {
        if terminated {
            return Err(TszError::Type {
                message: "Unreachable code after return/break/continue".to_string(),
                span: stmt_span(stmt),
            });
        }
        terminated = lower_stmt(ctx, state, out, stmt)?;
    }
    Ok(terminated)
}

fn lower_stmt(ctx: &LowerCtx<'_>, state: &mut LowerFnState, out: &mut Vec<HirStmt>, stmt: &Stmt) -> Result<bool, TszError> {
    match stmt {
        Stmt::Let {
            name,
            name_span,
            annotated_type,
            expr,
            span,
        } => {
            lower_let_stmt(ctx, state, out, name, *name_span, *annotated_type, expr, *span)?;
            Ok(false)
        }
        Stmt::Const {
            name,
            name_span,
            annotated_type,
            expr,
            span,
        } => {
            lower_const_stmt(ctx, state, name, *name_span, *annotated_type, expr, *span)?;
            Ok(false)
        }
        Stmt::Assign {
            name,
            name_span,
            expr,
            span,
        } => {
            assign::lower_assign_stmt(ctx, state, out, name, *name_span, expr, *span)?;
            Ok(false)
        }
        Stmt::ConsoleLog { args, span } => {
            lower_console_log_stmt(ctx, state, out, args, *span)?;
            Ok(false)
        }
        Stmt::Return { expr, span } => {
            lower_return_stmt(ctx, state, out, expr, *span)?;
            Ok(true)
        }
        Stmt::Block { stmts, .. } => {
            state.local_scopes.push();
            let terminated = lower_stmt_list(ctx, state, out, stmts)?;
            state.local_scopes.pop();
            Ok(terminated)
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            span,
        } => lower_if_stmt(ctx, state, out, cond, then_branch, else_branch.as_deref(), *span),
        Stmt::While { cond, body, span } => lower_while_stmt(ctx, state, out, cond, body, *span),
        Stmt::Break { span } => lower_break_stmt(state, out, *span),
        Stmt::Continue { span } => lower_continue_stmt(state, out, *span),
    }
}

fn lower_if_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    cond: &Expr,
    then_branch: &Stmt,
    else_branch: Option<&Stmt>,
    span: Span,
) -> Result<bool, TszError> {
    let (hir_cond, cond_ty) =
        lower_expr_in_function(ctx.module_idx, cond, ctx.scopes, ctx.func_infos, &state.local_scopes)?;
    if cond_ty != Type::Bool {
        return Err(TszError::Type {
            message: "if condition must be boolean".to_string(),
            span: ast_expr_span(cond),
        });
    }

    let (then_body, then_terminated) = lower_stmt_in_new_scope(ctx, state, then_branch)?;
    let (else_body, else_terminated) = match else_branch {
        None => (None, false),
        Some(stmt) => {
            let (b, t) = lower_stmt_in_new_scope(ctx, state, stmt)?;
            (Some(b), t)
        }
    };

    out.push(HirStmt::If {
        cond: hir_cond,
        then_body,
        else_body,
        span,
    });

    Ok(then_terminated && else_terminated)
}

fn lower_while_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    cond: &Expr,
    body: &Stmt,
    span: Span,
) -> Result<bool, TszError> {
    let (hir_cond, cond_ty) =
        lower_expr_in_function(ctx.module_idx, cond, ctx.scopes, ctx.func_infos, &state.local_scopes)?;
    if cond_ty != Type::Bool {
        return Err(TszError::Type {
            message: "while condition must be boolean".to_string(),
            span: ast_expr_span(cond),
        });
    }

    state.loop_depth += 1;
    let (body_stmts, _body_terminated) = lower_stmt_in_new_scope(ctx, state, body)?;
    state.loop_depth -= 1;

    out.push(HirStmt::While {
        cond: hir_cond,
        body: body_stmts,
        span,
    });

    // Even if the body ends with `return`, the loop condition can be false on entry.
    Ok(false)
}

fn lower_break_stmt(state: &LowerFnState, out: &mut Vec<HirStmt>, span: Span) -> Result<bool, TszError> {
    if state.loop_depth == 0 {
        return Err(TszError::Type {
            message: "break is only allowed inside while".to_string(),
            span,
        });
    }
    out.push(HirStmt::Break { span });
    Ok(true)
}

fn lower_continue_stmt(state: &LowerFnState, out: &mut Vec<HirStmt>, span: Span) -> Result<bool, TszError> {
    if state.loop_depth == 0 {
        return Err(TszError::Type {
            message: "continue is only allowed inside while".to_string(),
            span,
        });
    }
    out.push(HirStmt::Continue { span });
    Ok(true)
}

fn lower_stmt_in_new_scope(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    stmt: &Stmt,
) -> Result<(Vec<HirStmt>, bool), TszError> {
    state.local_scopes.push();
    let mut out = Vec::new();
    let terminated = match stmt {
        // Avoid double-pushing for explicit `{ ... }` blocks.
        Stmt::Block { stmts, .. } => lower_stmt_list(ctx, state, &mut out, stmts)?,
        _ => lower_stmt(ctx, state, &mut out, stmt)?,
    };
    state.local_scopes.pop();
    Ok((out, terminated))
}

fn lower_let_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    name: &str,
    name_span: Span,
    annotated_type: Option<Type>,
    expr: &Expr,
    span: Span,
) -> Result<(), TszError> {
    // To avoid ambiguity between `foo` and `foo()` (variable vs function), disallow collisions with module-level symbols.
    if ctx.module_scope.contains_key(name) {
        return Err(TszError::Type {
            message: format!("Local variable name conflicts with a function/import: {name}"),
            span: name_span,
        });
    }

    let (hir_init, init_ty) =
        lower_expr_in_function(ctx.module_idx, expr, ctx.scopes, ctx.func_infos, &state.local_scopes)?;
    validate_local_value_type(init_ty, ast_expr_span(expr))?;

    let declared_ty = annotated_type.unwrap_or(init_ty);
    validate_local_decl_type(declared_ty, name_span)?;

    if declared_ty != init_ty {
        return Err(TszError::Type {
            message: format!("let initializer type mismatch: declared {:?}, got {:?}", declared_ty, init_ty),
            span: ast_expr_span(expr),
        });
    }

    let local_id = state.locals.len();
    state.locals.push(HirLocal {
        name: name.to_string(),
        ty: declared_ty,
        span: name_span,
    });
    state.local_scopes.insert_current(
        name.to_string(),
        LocalInfo {
            value: LocalValue::Local(local_id),
            ty: declared_ty,
        },
        name_span,
    )?;

    out.push(HirStmt::Let {
        local: local_id,
        init: hir_init,
        span,
    });
    Ok(())
}

fn lower_const_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    name: &str,
    name_span: Span,
    annotated_type: Option<Type>,
    expr: &Expr,
    _span: Span,
) -> Result<(), TszError> {
    // Constraint: to keep the subset small and optimizable, `const` is currently a compile-time constant binding.
    // The initializer must fold into a literal (optionally through unary minus, and optionally referencing other consts).

    // To avoid ambiguity between `foo` and `foo()` (variable vs function), disallow collisions with module-level symbols.
    if ctx.module_scope.contains_key(name) {
        return Err(TszError::Type {
            message: format!("Local constant name conflicts with a function/import: {name}"),
            span: name_span,
        });
    }

    let (hir_init, init_ty) =
        lower_expr_in_function(ctx.module_idx, expr, ctx.scopes, ctx.func_infos, &state.local_scopes)?;
    validate_local_value_type(init_ty, ast_expr_span(expr))?;

    let declared_ty = annotated_type.unwrap_or(init_ty);
    validate_local_decl_type(declared_ty, name_span)?;

    if declared_ty != init_ty {
        return Err(TszError::Type {
            message: format!(
                "Local initializer type mismatch: declared {:?}, got {:?}",
                declared_ty, init_ty
            ),
            span: ast_expr_span(expr),
        });
    }

    let Some(const_value) = try_eval_const_value(&hir_init) else {
        return Err(TszError::Type {
            message: "const initializer must be a compile-time constant (literal/unary minus/binary ops)".to_string(),
            span: ast_expr_span(expr),
        });
    };

    state.local_scopes.insert_current(
        name.to_string(),
        LocalInfo {
            value: LocalValue::Const(const_value),
            ty: declared_ty,
        },
        name_span,
    )?;

    // `const` is compile-time only for now, so it does not produce a HIR statement.
    Ok(())
}

fn lower_console_log_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    args: &[Expr],
    span: Span,
) -> Result<(), TszError> {
    let hir_args = lower_console_log_args(
        ctx.module_idx,
        args,
        ctx.scopes,
        ctx.func_infos,
        &state.local_scopes,
        span,
    )?;
    out.push(HirStmt::ConsoleLog { args: hir_args, span });
    Ok(())
}

fn lower_return_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    expr: &Option<Expr>,
    span: Span,
) -> Result<(), TszError> {
    let (hir_expr, expr_ty) = match expr {
        None => (None, Type::Void),
        Some(e) => {
            let (hir, ty) = lower_expr_in_function(ctx.module_idx, e, ctx.scopes, ctx.func_infos, &state.local_scopes)?;
            (Some(hir), ty)
        }
    };

    if ctx.return_type == Type::Void && expr.is_some() {
        return Err(TszError::Type {
            message: "void functions only allow `return;`".to_string(),
            span,
        });
    }
    if ctx.return_type != expr_ty {
        return Err(TszError::Type {
            message: format!("Return type mismatch: declared {:?}, got {:?}", ctx.return_type, expr_ty),
            span,
        });
    }

    out.push(HirStmt::Return { expr: hir_expr, span });
    Ok(())
}

fn stmt_span(stmt: &Stmt) -> Span {
    match stmt {
        Stmt::Return { span, .. }
        | Stmt::ConsoleLog { span, .. }
        | Stmt::Block { span, .. }
        | Stmt::If { span, .. }
        | Stmt::While { span, .. }
        | Stmt::Break { span }
        | Stmt::Continue { span }
        | Stmt::Let { span, .. }
        | Stmt::Const { span, .. }
        | Stmt::Assign { span, .. } => *span,
    }
}
