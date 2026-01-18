use crate::{Expr, HirExpr, HirStmt, Span, TszError, Type};

use super::{ast_expr_span, lower_expr_in_function, validate_local_value_type, LocalValue, LowerCtx, LowerFnState};

pub(super) fn lower_assign_stmt(
    ctx: &mut LowerCtx<'_>,
    state: &mut LowerFnState,
    out: &mut Vec<HirStmt>,
    name: &str,
    name_span: Span,
    expr: &Expr,
    span: Span,
) -> Result<(), TszError> {
    let Some(info) = state.local_scopes.lookup(name) else {
        ctx.diags
            .error_at(ctx.module_path, name_span, format!("Undefined variable: {name}"));
        return Ok(());
    };

    let LocalValue::Local(local_id) = info.value else {
        let message = match info.value {
            LocalValue::Const(_) => format!("Cannot assign to const variable: {name}"),
            LocalValue::Param(_) => format!("Cannot assign to parameter: {name}"),
            LocalValue::Local(_) => unreachable!("handled by let-else above"),
        };
        ctx.diags.error_at(ctx.module_path, name_span, message);
        return Ok(());
    };

    let (hir_value, value_ty) = lower_expr_in_function(ctx, expr, &state.local_scopes)?;
    let _ = validate_local_value_type(ctx, value_ty, ast_expr_span(expr));
    if value_ty == Type::Error || info.ty == Type::Error {
        out.push(HirStmt::Assign {
            local: local_id,
            value: HirExpr::Error { span },
            span,
        });
        return Ok(());
    }
    if value_ty != info.ty {
        ctx.diags.error_at(
            ctx.module_path,
            ast_expr_span(expr),
            format!("Assignment type mismatch: expected {:?}, got {:?}", info.ty, value_ty),
        );
        return Ok(());
    }

    out.push(HirStmt::Assign {
        local: local_id,
        value: hir_value,
        span,
    });
    Ok(())
}
