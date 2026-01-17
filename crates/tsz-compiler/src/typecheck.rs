use crate::{Expr, Module, Span, Stmt, TszError, Type};

pub fn typecheck_entry(module: &Module) -> Result<(), TszError> {
    let Some(main) = module
        .functions
        .iter()
        .find(|f| f.is_export && f.name == "main")
    else {
        return Err(TszError::Type {
            message: "缺少入口函数：export function main(): <type> { ... }".to_string(),
            span: Span { start: 0, end: 0 },
        });
    };

    // 目前仅支持 `return <expr>;` 的最小函数体
    let Some(Stmt::Return { expr, span }) = main.body.first() else {
        return Err(TszError::Type {
            message: "main 函数体暂只支持 return <expr>;".to_string(),
            span: main.span,
        });
    };

    let expr_ty = expr_type(expr)?;
    if main.return_type != expr_ty {
        return Err(TszError::Type {
            message: format!(
                "main 返回类型不匹配：声明为 {:?}，实际为 {:?}",
                main.return_type, expr_ty
            ),
            span: *span,
        });
    }

    Ok(())
}

fn expr_type(expr: &Expr) -> Result<Type, TszError> {
    match expr {
        Expr::Number { .. } => Ok(Type::Number),
        Expr::BigInt { .. } => Ok(Type::BigInt),
        Expr::UnaryMinus { expr, .. } => expr_type(expr),
    }
}
