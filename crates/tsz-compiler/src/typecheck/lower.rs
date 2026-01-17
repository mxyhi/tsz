use super::FuncInfo;
use crate::{Expr, HirExpr, HirLocal, HirLocalId, HirStmt, Span, Stmt, TszError, Type};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum ConstValue {
    Number(f64),
    BigInt(i64),
}

#[derive(Debug, Clone, Copy)]
enum LocalValue {
    Runtime(HirLocalId),
    Const(ConstValue),
}

#[derive(Debug, Clone, Copy)]
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
            // No nested blocks yet, but a stack structure extends naturally to future `{ ... }` blocks.
            stack: vec![HashMap::new()],
        }
    }

    fn lookup(&self, name: &str) -> Option<LocalInfo> {
        self.stack.iter().rev().find_map(|scope| scope.get(name).copied())
    }

    fn insert_current(&mut self, name: String, info: LocalInfo, name_span: Span) -> Result<(), TszError> {
        let current = self.stack.last_mut().expect("at least one scope");
        if current.contains_key(&name) {
            return Err(TszError::Type {
                message: format!("Duplicate local variable declaration: {name}"),
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
}

struct LowerFnState {
    locals: Vec<HirLocal>,
    local_scopes: LocalScopes,
    body: Vec<HirStmt>,
}

impl LowerFnState {
    fn new(stmt_count: usize) -> Self {
        Self {
            locals: Vec::new(),
            local_scopes: LocalScopes::new(),
            body: Vec::with_capacity(stmt_count),
        }
    }
}

pub(super) fn lower_function_body(
    module_idx: usize,
    func: &crate::FunctionDecl,
    module_scope: &HashMap<String, usize>,
    scopes: &[HashMap<String, usize>],
    func_infos: &[FuncInfo],
) -> Result<(Vec<HirLocal>, Vec<HirStmt>), TszError> {
    if func.body.is_empty() {
        return Err(TszError::Type {
            message: "Empty function body (the current minimal subset requires a return)".to_string(),
            span: func.span,
        });
    }

    let ctx = LowerCtx {
        module_idx,
        module_scope,
        scopes,
        func_infos,
    };
    let mut state = LowerFnState::new(func.body.len());

    let (last, prefix) = func.body.split_last().expect("checked non-empty");
    for stmt in prefix {
        match stmt {
            Stmt::Let {
                name,
                name_span,
                annotated_type,
                expr,
                span,
            } => lower_let_stmt(&ctx, &mut state, name, *name_span, *annotated_type, expr, *span)?,
            Stmt::Const {
                name,
                name_span,
                annotated_type,
                expr,
                span,
            } => lower_const_stmt(&ctx, &mut state, name, *name_span, *annotated_type, expr, *span)?,
            Stmt::ConsoleLog { args, span } => lower_console_log_stmt(&ctx, &mut state, args, *span)?,
            Stmt::Return { span, .. } => {
                return Err(TszError::Type {
                    message: "return must be the last statement in the function body".to_string(),
                    span: *span,
                });
            }
        }
    }

    match last {
        Stmt::Return { expr, span } => lower_return_stmt(&ctx, &mut state, func.return_type, expr, *span)?,
        Stmt::Let { span, .. } | Stmt::Const { span, .. } | Stmt::ConsoleLog { span, .. } => {
            return Err(TszError::Type {
                message: "The last statement in the function body must be return".to_string(),
                span: *span,
            });
        }
    }

    Ok((state.locals, state.body))
}

fn lower_let_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
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

    // `let` currently supports only number/bigint to avoid pulling in string/bool runtime semantics.
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
            value: LocalValue::Runtime(local_id),
            ty: declared_ty,
        },
        name_span,
    )?;

    state.body.push(HirStmt::Let {
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
            message: "const initializer must be a compile-time constant (literal or unary minus)".to_string(),
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

fn try_eval_const_value(expr: &HirExpr) -> Option<ConstValue> {
    match expr {
        HirExpr::Number { value, .. } => Some(ConstValue::Number(*value)),
        HirExpr::BigInt { value, .. } => Some(ConstValue::BigInt(*value)),
        HirExpr::UnaryMinus { expr, .. } => match try_eval_const_value(expr)? {
            ConstValue::Number(v) => Some(ConstValue::Number(-v)),
            ConstValue::BigInt(v) => Some(ConstValue::BigInt(v.checked_neg()?)),
        },
        HirExpr::String { .. } | HirExpr::Local { .. } | HirExpr::Call { .. } => None,
    }
}

fn lower_console_log_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
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
    state.body.push(HirStmt::ConsoleLog { args: hir_args, span });
    Ok(())
}

fn lower_return_stmt(
    ctx: &LowerCtx<'_>,
    state: &mut LowerFnState,
    return_type: Type,
    expr: &Option<Expr>,
    span: Span,
) -> Result<(), TszError> {
    if return_type == Type::Void && expr.is_some() {
        return Err(TszError::Type {
            message: "void functions only allow `return;`".to_string(),
            span,
        });
    }

    let (hir_expr, expr_ty) = match expr {
        None => (None, Type::Void),
        Some(e) => {
            let (hir, ty) = lower_expr_in_function(ctx.module_idx, e, ctx.scopes, ctx.func_infos, &state.local_scopes)?;
            (Some(hir), ty)
        }
    };

    if return_type != expr_ty {
        return Err(TszError::Type {
            message: format!("Return type mismatch: declared {:?}, got {:?}", return_type, expr_ty),
            span,
        });
    }

    state.body.push(HirStmt::Return { expr: hir_expr, span });
    Ok(())
}

fn validate_local_decl_type(ty: Type, span: Span) -> Result<(), TszError> {
    match ty {
        Type::Number | Type::BigInt => Ok(()),
        Type::Void => Err(TszError::Type {
            message: "Local variable type cannot be void".to_string(),
            span,
        }),
        Type::Bool | Type::String => Err(TszError::Type {
            message: "Local variables currently only support number/bigint".to_string(),
            span,
        }),
    }
}

fn validate_local_value_type(ty: Type, span: Span) -> Result<(), TszError> {
    match ty {
        Type::Number | Type::BigInt => Ok(()),
        Type::Void => Err(TszError::Type {
            message: "Local initializer expression cannot be void".to_string(),
            span,
        }),
        Type::Bool | Type::String => Err(TszError::Type {
            message: "Local initializer currently only supports number/bigint".to_string(),
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
        Expr::String { value, span } => Ok((HirExpr::String { value: value.clone(), span: *span }, Type::String)),
        Expr::Ident { name, span } => {
            let Some(info) = locals.lookup(name) else {
                return Err(TszError::Type {
                    message: format!("Undefined variable: {name}"),
                    span: *span,
                });
            };
            match info.value {
                LocalValue::Runtime(id) => Ok((HirExpr::Local { local: id, span: *span }, info.ty)),
                LocalValue::Const(v) => match v {
                    ConstValue::Number(value) => Ok((HirExpr::Number { value, span: *span }, Type::Number)),
                    ConstValue::BigInt(value) => Ok((HirExpr::BigInt { value, span: *span }, Type::BigInt)),
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
        Expr::Call { callee, span } => {
            let scope = scopes.get(module_idx).ok_or_else(|| TszError::Type {
                message: "Module scope index out of bounds (internal error)".to_string(),
                span: *span,
            })?;
            let callee_id = scope.get(callee).copied().ok_or_else(|| TszError::Type {
                message: format!("Undefined function: {callee}"),
                span: *span,
            })?;
            let ret_ty = func_infos
                .get(callee_id)
                .ok_or_else(|| TszError::Type {
                    message: "Function index out of bounds (internal error)".to_string(),
                    span: *span,
                })?
                .return_type;
            Ok((HirExpr::Call { callee: callee_id, span: *span }, ret_ty))
        }
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
            Type::Number | Type::BigInt => {}
            Type::String => {
                // The current string runtime only supports string literals.
                if !matches!(hir, HirExpr::String { .. }) {
                    return Err(TszError::Type {
                        message: "string currently only supports string literals".to_string(),
                        span,
                    });
                }
            }
            Type::Void | Type::Bool => {
                return Err(TszError::Type {
                    message: "console.log arguments only support number/bigint/string".to_string(),
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
        | Expr::String { span, .. }
        | Expr::Ident { span, .. }
        | Expr::UnaryMinus { span, .. }
        | Expr::Call { span, .. } => *span,
    }
}

