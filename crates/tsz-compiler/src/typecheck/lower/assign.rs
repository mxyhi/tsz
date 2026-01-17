use crate::{Expr, HirStmt, Span, TszError};

use super::{ast_expr_span, lower_expr_in_function, validate_local_value_type, LocalValue, LowerCtx, LowerFnState};

pub(super) fn lower_assign_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    name: &str,
    name_span: Span,
    expr: &Expr,
    span: Span,
) -> Result<(), TszError> {
    let Some(info) = state.local_scopes.lookup(name) else {
        return Err(TszError::Type {
            message: format!("Undefined variable: {name}"),
            span: name_span,
        });
    };

    let LocalValue::Local(local_id) = info.value else {
        let message = match info.value {
            LocalValue::Const(_) => format!("Cannot assign to const variable: {name}"),
            LocalValue::Param(_) => format!("Cannot assign to parameter: {name}"),
            LocalValue::Local(_) => unreachable!("handled by let-else above"),
        };
        return Err(TszError::Type { message, span: name_span });
    };

    let (hir_value, value_ty) =
        lower_expr_in_function(ctx.module_idx, expr, ctx.scopes, ctx.func_infos, &state.local_scopes)?;
    validate_local_value_type(value_ty, ast_expr_span(expr))?;
    if value_ty != info.ty {
        return Err(TszError::Type {
            message: format!("Assignment type mismatch: expected {:?}, got {:?}", info.ty, value_ty),
            span: ast_expr_span(expr),
        });
    }

    state.body.push(HirStmt::Assign {
        local: local_id,
        value: hir_value,
        span,
    });
    Ok(())
}

