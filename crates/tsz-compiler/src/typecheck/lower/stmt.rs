use crate::{Expr, HirExpr, HirLocal, HirStmt, Span, Stmt, TszError, Type};

use super::assign;
use super::const_eval::{try_eval_const_value, ConstValue};
use super::{
    ast_expr_span, lower_console_log_args, lower_expr_in_function, validate_local_decl_type, validate_local_value_type,
    LocalInfo, LocalValue, LowerCtx, LowerFnState,
};

pub(super) fn lower_stmt_list(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    stmts: &[Stmt],
) -> Result<bool, TszError> {
    let mut terminated = false;
    for stmt in stmts {
        if terminated {
            ctx.diags
                .error_at(ctx.module_path, stmt_span(stmt), "Unreachable code after return/break/continue");
        }
        let stmt_terminated = lower_stmt(ctx, state, out, stmt)?;
        if !terminated {
            terminated = stmt_terminated;
        }
    }
    Ok(terminated)
}

fn lower_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    stmt: &Stmt,
) -> Result<bool, TszError> {
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
        Stmt::For {
            init,
            cond,
            update,
            body,
            span,
        } => lower_for_stmt(
            ctx,
            state,
            out,
            init.as_deref(),
            cond.as_ref(),
            update.as_deref(),
            body,
            *span,
        ),
        Stmt::Break { span } => lower_break_stmt(ctx, state, out, *span),
        Stmt::Continue { span } => lower_continue_stmt(ctx, state, out, *span),
        Stmt::Error { span } => {
            out.push(HirStmt::Error { span: *span });
            Ok(false)
        }
    }
}

fn lower_if_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    cond: &Expr,
    then_branch: &Stmt,
    else_branch: Option<&Stmt>,
    span: Span,
) -> Result<bool, TszError> {
    let (hir_cond, cond_ty) = lower_expr_in_function(ctx, cond, &state.local_scopes)?;
    let hir_cond = if cond_ty != Type::Bool {
        if cond_ty != Type::Error {
            ctx.diags
                .error_at(ctx.module_path, ast_expr_span(cond), "if condition must be boolean");
        }
        HirExpr::Error { span: ast_expr_span(cond) }
    } else {
        hir_cond
    };

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
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    cond: &Expr,
    body: &Stmt,
    span: Span,
) -> Result<bool, TszError> {
    let (hir_cond, cond_ty) = lower_expr_in_function(ctx, cond, &state.local_scopes)?;
    let hir_cond = if cond_ty != Type::Bool {
        if cond_ty != Type::Error {
            ctx.diags
                .error_at(ctx.module_path, ast_expr_span(cond), "while condition must be boolean");
        }
        HirExpr::Error { span: ast_expr_span(cond) }
    } else {
        hir_cond
    };

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

fn lower_for_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    init: Option<&Stmt>,
    cond: Option<&Expr>,
    update: Option<&Stmt>,
    body: &Stmt,
    span: Span,
) -> Result<bool, TszError> {
    // `for (...) { ... }` introduces a lexical scope for its init bindings.
    state.local_scopes.push();

    if let Some(init_stmt) = init {
        lower_for_init_stmt(ctx, state, out, init_stmt)?;
    }

    let hir_cond = lower_for_cond_expr(ctx, state, cond, span)?;
    let update_stmts = lower_for_update_stmt(ctx, state, update)?;

    state.loop_depth += 1;
    let (body_stmts, _body_terminated) = lower_stmt_in_new_scope(ctx, state, body)?;
    state.loop_depth -= 1;

    let body_stmts = if update_stmts.is_empty() {
        body_stmts
    } else {
        let mut new_body = inject_for_update_before_continue(&body_stmts, &update_stmts);
        // Normal fallthrough path runs the update before checking the next iteration condition.
        new_body.extend(update_stmts.clone());
        new_body
    };

    out.push(HirStmt::While {
        cond: hir_cond,
        body: body_stmts,
        span,
    });

    state.local_scopes.pop();

    // Without constant-condition analysis, conservatively assume `for` may fall through
    // (the condition can be false on entry).
    Ok(false)
}

fn lower_for_init_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    init: &Stmt,
) -> Result<(), TszError> {
    match init {
        Stmt::Let {
            name,
            name_span,
            annotated_type,
            expr,
            span,
        } => lower_let_stmt(ctx, state, out, name, *name_span, *annotated_type, expr, *span),
        Stmt::Const {
            name,
            name_span,
            annotated_type,
            expr,
            span,
        } => lower_const_stmt(ctx, state, name, *name_span, *annotated_type, expr, *span),
        Stmt::Assign {
            name,
            name_span,
            expr,
            span,
        } => assign::lower_assign_stmt(ctx, state, out, name, *name_span, expr, *span),
        Stmt::Error { span } => {
            out.push(HirStmt::Error { span: *span });
            Ok(())
        }
        _ => {
            ctx.diags.error_at(
                ctx.module_path,
                stmt_span(init),
                "for-loop initializer must be `let/const/<name> = <expr>` or empty",
            );
            out.push(HirStmt::Error { span: stmt_span(init) });
            Ok(())
        }
    }
}

fn lower_for_cond_expr(
    ctx: &mut LowerCtx<'_>,
    state: &LowerFnState,
    cond: Option<&Expr>,
    span: Span,
) -> Result<HirExpr, TszError> {
    let Some(cond) = cond else {
        return Ok(HirExpr::Bool { value: true, span });
    };

    let (hir_cond, cond_ty) = lower_expr_in_function(ctx, cond, &state.local_scopes)?;
    if cond_ty != Type::Bool {
        if cond_ty != Type::Error {
            ctx.diags
                .error_at(ctx.module_path, ast_expr_span(cond), "for-loop condition must be boolean");
        }
        return Ok(HirExpr::Error { span: ast_expr_span(cond) });
    }
    Ok(hir_cond)
}

fn lower_for_update_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    update: Option<&Stmt>,
) -> Result<Vec<HirStmt>, TszError> {
    let Some(update) = update else { return Ok(Vec::new()) };

    let Stmt::Assign {
        name,
        name_span,
        expr,
        span,
    } = update
    else {
        ctx.diags.error_at(
            ctx.module_path,
            stmt_span(update),
            "for-loop update clause must be `<name> = <expr>` or empty",
        );
        return Ok(vec![HirStmt::Error { span: stmt_span(update) }]);
    };

    let mut out = Vec::with_capacity(1);
    assign::lower_assign_stmt(ctx, state, &mut out, name, *name_span, expr, *span)?;
    Ok(out)
}

fn inject_for_update_before_continue(stmts: &[HirStmt], update: &[HirStmt]) -> Vec<HirStmt> {
    let mut out = Vec::with_capacity(stmts.len());
    for stmt in stmts {
        match stmt {
            HirStmt::Continue { span } => {
                // `for`-continue runs the update expression before checking the next iteration condition.
                out.extend_from_slice(update);
                out.push(HirStmt::Continue { span: *span });
            }
            HirStmt::If {
                cond,
                then_body,
                else_body,
                span,
            } => out.push(HirStmt::If {
                cond: cond.clone(),
                then_body: inject_for_update_before_continue(then_body, update),
                else_body: else_body
                    .as_ref()
                    .map(|b| inject_for_update_before_continue(b, update)),
                span: *span,
            }),
            HirStmt::While { .. } => out.push(stmt.clone()),
            HirStmt::Let { .. }
            | HirStmt::Assign { .. }
            | HirStmt::Break { .. }
            | HirStmt::ConsoleLog { .. }
            | HirStmt::Return { .. }
            | HirStmt::Error { .. } => out.push(stmt.clone()),
        }
    }
    out
}

fn lower_break_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &LowerFnState,
    out: &mut Vec<HirStmt>,
    span: Span,
) -> Result<bool, TszError> {
    if state.loop_depth == 0 {
        ctx.diags
            .error_at(ctx.module_path, span, "break is only allowed inside while/for");
        out.push(HirStmt::Error { span });
        return Ok(false);
    }
    out.push(HirStmt::Break { span });
    Ok(true)
}

fn lower_continue_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &LowerFnState,
    out: &mut Vec<HirStmt>,
    span: Span,
) -> Result<bool, TszError> {
    if state.loop_depth == 0 {
        ctx.diags
            .error_at(ctx.module_path, span, "continue is only allowed inside while/for");
        out.push(HirStmt::Error { span });
        return Ok(false);
    }
    out.push(HirStmt::Continue { span });
    Ok(true)
}

fn lower_stmt_in_new_scope(
    ctx: &mut LowerCtx<'_>,
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
    ctx: &mut LowerCtx<'_>,
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
        ctx.diags.error_at(
            ctx.module_path,
            name_span,
            format!("Local variable name conflicts with a function/import: {name}"),
        );
        return Ok(());
    }

    let (hir_init, mut init_ty) = lower_expr_in_function(ctx, expr, &state.local_scopes)?;
    if !validate_local_value_type(ctx, init_ty, ast_expr_span(expr)) {
        init_ty = Type::Error;
    }

    let mut declared_ty = annotated_type.unwrap_or(init_ty);
    if !validate_local_decl_type(ctx, declared_ty, name_span) {
        declared_ty = Type::Error;
    }

    if declared_ty != init_ty && declared_ty != Type::Error && init_ty != Type::Error {
        ctx.diags.error_at(
            ctx.module_path,
            ast_expr_span(expr),
            format!("let initializer type mismatch: declared {:?}, got {:?}", declared_ty, init_ty),
        );
        declared_ty = Type::Error;
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
        ctx.diags,
        ctx.module_path,
    );

    out.push(HirStmt::Let {
        local: local_id,
        init: if declared_ty == Type::Error { HirExpr::Error { span } } else { hir_init },
        span,
    });
    Ok(())
}

fn lower_const_stmt(
    ctx: &mut LowerCtx<'_>,
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
        ctx.diags.error_at(
            ctx.module_path,
            name_span,
            format!("Local constant name conflicts with a function/import: {name}"),
        );
        return Ok(());
    }

    let (hir_init, mut init_ty) = lower_expr_in_function(ctx, expr, &state.local_scopes)?;
    if !validate_local_value_type(ctx, init_ty, ast_expr_span(expr)) {
        init_ty = Type::Error;
    }

    let mut declared_ty = annotated_type.unwrap_or(init_ty);
    if !validate_local_decl_type(ctx, declared_ty, name_span) {
        declared_ty = Type::Error;
    }

    if declared_ty != init_ty && declared_ty != Type::Error && init_ty != Type::Error {
        ctx.diags.error_at(
            ctx.module_path,
            ast_expr_span(expr),
            format!("Local initializer type mismatch: declared {:?}, got {:?}", declared_ty, init_ty),
        );
        declared_ty = Type::Error;
    }

    let const_value = try_eval_const_value(&hir_init);
    if const_value.is_none() && declared_ty != Type::Error {
        ctx.diags.error_at(
            ctx.module_path,
            ast_expr_span(expr),
            "const initializer must be a compile-time constant (literal/unary minus/binary ops)",
        );
        declared_ty = Type::Error;
    }

    let value = const_value.unwrap_or_else(|| ConstValue::Number(0.0));

    state.local_scopes.insert_current(
        name.to_string(),
        LocalInfo {
            value: LocalValue::Const(value),
            ty: declared_ty,
        },
        name_span,
        ctx.diags,
        ctx.module_path,
    );

    // `const` is compile-time only for now, so it does not produce a HIR statement.
    Ok(())
}

fn lower_console_log_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    args: &[Expr],
    span: Span,
) -> Result<(), TszError> {
    let hir_args = lower_console_log_args(ctx, args, &state.local_scopes, span)?;
    out.push(HirStmt::ConsoleLog { args: hir_args, span });
    Ok(())
}

fn lower_return_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    expr: &Option<Expr>,
    span: Span,
) -> Result<(), TszError> {
    let (hir_expr, expr_ty) = match expr {
        None => (None, Type::Void),
        Some(e) => {
            let (hir, ty) = lower_expr_in_function(ctx, e, &state.local_scopes)?;
            (Some(hir), ty)
        }
    };

    if ctx.return_type == Type::Void && expr.is_some() {
        ctx.diags
            .error_at(ctx.module_path, span, "void functions only allow `return;`");
    } else if ctx.return_type != expr_ty && expr_ty != Type::Error && ctx.return_type != Type::Error {
        ctx.diags.error_at(
            ctx.module_path,
            span,
            format!("Return type mismatch: declared {:?}, got {:?}", ctx.return_type, expr_ty),
        );
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
        | Stmt::For { span, .. }
        | Stmt::Break { span }
        | Stmt::Continue { span }
        | Stmt::Let { span, .. }
        | Stmt::Const { span, .. }
        | Stmt::Assign { span, .. }
        | Stmt::Error { span } => *span,
    }
}
