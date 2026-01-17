use crate::{
    Expr, HirExpr, HirFunction, HirLocal, HirLocalId, HirProgram, HirStmt, Program, Span, Stmt, TszError, Type,
};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// 语义分析 + 最小类型检查，并输出 HIR 供后续 codegen 使用。
///
/// TSZ v0/v0.1 约束（当前实现）：
/// - 仅支持 0 参数函数
/// - 函数体支持 `let`/`console.log(...)`，且最后一条语句必须是 `return`
/// - 表达式支持：字面量、标识符（局部变量）、一元负号、0 参调用
/// - import 仅支持 `import { a, b } from "<specifier>";`
pub fn analyze(program: &Program) -> Result<HirProgram, TszError> {
    let module_index = build_module_index(program)?;
    let entry_module_idx = module_index
        .get(&program.entry)
        .copied()
        .ok_or_else(|| TszError::Type {
            message: format!("入口模块未加载: {:?}", program.entry),
            span: Span { start: 0, end: 0 },
        })?;

    let (func_infos, key_to_id) = index_functions(program)?;
    let scopes = build_scopes(program, &module_index, &func_infos, &key_to_id)?;

    let entry_id = find_entry_main(program, entry_module_idx, &key_to_id)?;
    validate_entry_return_type(entry_id, &func_infos)?;

    let hir_functions = build_hir_functions(program, &func_infos, &scopes, entry_id)?;

    Ok(HirProgram {
        entry: entry_id,
        functions: hir_functions,
    })
}

fn build_module_index(program: &Program) -> Result<HashMap<PathBuf, usize>, TszError> {
    let mut map = HashMap::new();
    for (idx, m) in program.modules.iter().enumerate() {
        if map.insert(m.path.clone(), idx).is_some() {
            return Err(TszError::Type {
                message: format!("重复加载模块: {:?}", m.path),
                span: Span { start: 0, end: 0 },
            });
        }
    }
    Ok(map)
}

#[derive(Debug, Clone)]
struct FuncInfo {
    module_idx: usize,
    func_idx: usize,
    return_type: Type,
    is_export: bool,
    name: String,
    span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FuncKey {
    module_path: PathBuf,
    name: String,
}

fn index_functions(program: &Program) -> Result<(Vec<FuncInfo>, HashMap<FuncKey, usize>), TszError> {
    let mut infos = Vec::new();
    let mut key_to_id = HashMap::new();

    for (module_idx, module) in program.modules.iter().enumerate() {
        let mut seen = HashSet::new();
        for (func_idx, f) in module.functions.iter().enumerate() {
            if !seen.insert(f.name.clone()) {
                return Err(TszError::Type {
                    message: format!("模块内重复函数名: {} ({:?})", f.name, module.path),
                    span: f.span,
                });
            }

            let id = infos.len();
            let key = FuncKey {
                module_path: module.path.clone(),
                name: f.name.clone(),
            };
            if key_to_id.insert(key, id).is_some() {
                return Err(TszError::Type {
                    message: "函数定义重复（内部错误）".to_string(),
                    span: f.span,
                });
            }
            infos.push(FuncInfo {
                module_idx,
                func_idx,
                return_type: f.return_type,
                is_export: f.is_export,
                name: f.name.clone(),
                span: f.span,
            });
        }
    }

    Ok((infos, key_to_id))
}

fn build_scopes(
    program: &Program,
    module_index: &HashMap<PathBuf, usize>,
    func_infos: &[FuncInfo],
    key_to_id: &HashMap<FuncKey, usize>,
) -> Result<Vec<HashMap<String, usize>>, TszError> {
    let mut scopes: Vec<HashMap<String, usize>> = vec![HashMap::new(); program.modules.len()];

    // 先把模块内函数放入作用域（允许模块内互相调用）
    for (id, info) in func_infos.iter().enumerate() {
        let scope = scopes
            .get_mut(info.module_idx)
            .ok_or_else(|| TszError::Type {
                message: "模块索引越界（内部错误）".to_string(),
                span: info.span,
            })?;
        scope.insert(info.name.clone(), id);
    }

    // 再处理 import：只允许导入 export 函数
    for (current_module_idx, module) in program.modules.iter().enumerate() {
        for import in &module.imports {
            let Some(dep_path) = &import.resolved_path else {
                return Err(TszError::Type {
                    message: "import 缺少 resolved_path（loader 内部错误）".to_string(),
                    span: import.span,
                });
            };
            let dep_idx = module_index.get(dep_path).copied().ok_or_else(|| TszError::Type {
                message: format!("import 目标模块未加载: {:?}", dep_path),
                span: import.span,
            })?;

            for name in &import.names {
                let target_key = FuncKey {
                    module_path: program.modules[dep_idx].path.clone(),
                    name: name.name.clone(),
                };
                let target_id = key_to_id.get(&target_key).copied().ok_or_else(|| TszError::Type {
                    message: format!("导入的符号不存在: {} from {:?}", name.name, dep_path),
                    span: name.span,
                })?;

                if !func_infos[target_id].is_export {
                    return Err(TszError::Type {
                        message: format!("只能导入 export 函数: {} from {:?}", name.name, dep_path),
                        span: name.span,
                    });
                }

                let scope = &mut scopes[current_module_idx];
                if scope.contains_key(&name.name) {
                    return Err(TszError::Type {
                        message: format!("重复绑定名称（与本地函数或其他导入冲突）: {}", name.name),
                        span: name.span,
                    });
                }
                scope.insert(name.name.clone(), target_id);
            }
        }
    }

    Ok(scopes)
}

fn find_entry_main(
    program: &Program,
    entry_module_idx: usize,
    key_to_id: &HashMap<FuncKey, usize>,
) -> Result<usize, TszError> {
    let entry_module = program
        .modules
        .get(entry_module_idx)
        .ok_or_else(|| TszError::Type {
            message: "入口模块索引越界（内部错误）".to_string(),
            span: Span { start: 0, end: 0 },
        })?;

    let main = entry_module
        .functions
        .iter()
        .find(|f| f.is_export && f.name == "main")
        .ok_or_else(|| TszError::Type {
            message: "缺少入口函数：export function main(): <type> { ... }".to_string(),
            span: Span { start: 0, end: 0 },
        })?;

    let key = FuncKey {
        module_path: entry_module.path.clone(),
        name: main.name.clone(),
    };
    key_to_id.get(&key).copied().ok_or_else(|| TszError::Type {
        message: "入口函数索引失败（内部错误）".to_string(),
        span: main.span,
    })
}

fn validate_entry_return_type(entry_id: usize, func_infos: &[FuncInfo]) -> Result<(), TszError> {
    match func_infos
        .get(entry_id)
        .ok_or_else(|| TszError::Type {
            message: "入口函数索引越界（内部错误）".to_string(),
            span: Span { start: 0, end: 0 },
        })?
        .return_type
    {
        Type::Number | Type::BigInt | Type::Void => Ok(()),
        Type::Bool | Type::String => Err(TszError::Type {
            message: "入口返回类型只支持 number/bigint/void".to_string(),
            span: Span { start: 0, end: 0 },
        }),
    }
}

fn build_hir_functions(
    program: &Program,
    func_infos: &[FuncInfo],
    scopes: &[HashMap<String, usize>],
    entry_id: usize,
) -> Result<Vec<HirFunction>, TszError> {
    let mut out = Vec::with_capacity(func_infos.len());

    for (id, info) in func_infos.iter().enumerate() {
        let module = program.modules.get(info.module_idx).ok_or_else(|| TszError::Type {
            message: "模块索引越界（内部错误）".to_string(),
            span: info.span,
        })?;
        let func = module.functions.get(info.func_idx).ok_or_else(|| TszError::Type {
            message: "函数索引越界（内部错误）".to_string(),
            span: info.span,
        })?;

        validate_user_fn_return_type(func.return_type, func.span)?;

        let module_scope = scopes.get(info.module_idx).ok_or_else(|| TszError::Type {
            message: "模块作用域索引越界（内部错误）".to_string(),
            span: func.span,
        })?;
        let (locals, body) = lower_function_body(info.module_idx, func, module_scope, scopes, func_infos)?;

        let symbol = if id == entry_id {
            "__tsz_user_main".to_string()
        } else {
            mangle_symbol(info.module_idx, &func.name)
        };

        out.push(HirFunction {
            symbol,
            return_type: func.return_type,
            locals,
            body,
            source: crate::HirSourceInfo {
                module_path: module.path.clone(),
                name: func.name.clone(),
                is_export: func.is_export,
                span: func.span,
            },
        });
    }

    Ok(out)
}

fn validate_user_fn_return_type(return_type: Type, span: Span) -> Result<(), TszError> {
    match return_type {
        Type::Number | Type::BigInt | Type::Void => Ok(()),
        Type::Bool | Type::String => Err(TszError::Type {
            message: "当前最小实现只支持 number/bigint/void 作为函数返回类型".to_string(),
            span,
        }),
    }
}

#[derive(Debug, Clone, Copy)]
struct LocalInfo {
    id: HirLocalId,
    ty: Type,
}

#[derive(Debug)]
struct LocalScopes {
    stack: Vec<HashMap<String, LocalInfo>>,
}

impl LocalScopes {
    fn new() -> Self {
        Self {
            // 当前还没有嵌套 block；但用栈结构能自然扩展到未来的 `{ ... }`。
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
                message: format!("重复声明的局部变量: {name}"),
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

fn lower_function_body(
    module_idx: usize,
    func: &crate::FunctionDecl,
    module_scope: &HashMap<String, usize>,
    scopes: &[HashMap<String, usize>],
    func_infos: &[FuncInfo],
) -> Result<(Vec<HirLocal>, Vec<HirStmt>), TszError> {
    if func.body.is_empty() {
        return Err(TszError::Type {
            message: "空函数体（当前最小实现要求必须有 return）".to_string(),
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
            Stmt::ConsoleLog { args, span } => lower_console_log_stmt(&ctx, &mut state, args, *span)?,
            Stmt::Return { span, .. } => {
                return Err(TszError::Type {
                    message: "return 必须是函数体最后一条语句".to_string(),
                    span: *span,
                });
            }
        }
    }

    match last {
        Stmt::Return { expr, span } => lower_return_stmt(&ctx, &mut state, func.return_type, expr, *span)?,
        Stmt::Let { span, .. } | Stmt::ConsoleLog { span, .. } => {
            return Err(TszError::Type {
                message: "函数体最后一条语句必须是 return".to_string(),
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
    // 为了避免 `foo`/`foo()` 在“变量 vs 函数”间产生歧义，先禁止与模块级符号重名。
    if ctx.module_scope.contains_key(name) {
        return Err(TszError::Type {
            message: format!("局部变量与函数/导入重名: {name}"),
            span: name_span,
        });
    }

    let (hir_init, init_ty) =
        lower_expr_in_function(ctx.module_idx, expr, ctx.scopes, ctx.func_infos, &state.local_scopes)?;

    // 当前 let 仅支持 number/bigint：避免引入 string/bool 运行时语义。
    validate_local_value_type(init_ty, ast_expr_span(expr))?;

    let declared_ty = annotated_type.unwrap_or(init_ty);
    validate_local_decl_type(declared_ty, name_span)?;

    if declared_ty != init_ty {
        return Err(TszError::Type {
            message: format!("let 初始化表达式类型不匹配：声明为 {:?}，实际为 {:?}", declared_ty, init_ty),
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
            id: local_id,
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
            message: "void 函数只允许 `return;`".to_string(),
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
            message: format!("返回类型不匹配：声明为 {:?}，实际为 {:?}", return_type, expr_ty),
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
            message: "let 变量类型不能是 void".to_string(),
            span,
        }),
        Type::Bool | Type::String => Err(TszError::Type {
            message: "当前 let 仅支持 number/bigint".to_string(),
            span,
        }),
    }
}

fn validate_local_value_type(ty: Type, span: Span) -> Result<(), TszError> {
    match ty {
        Type::Number | Type::BigInt => Ok(()),
        Type::Void => Err(TszError::Type {
            message: "let 初始化表达式不能是 void".to_string(),
            span,
        }),
        Type::Bool | Type::String => Err(TszError::Type {
            message: "当前 let 初始化表达式仅支持 number/bigint".to_string(),
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
                    message: format!("未定义的变量: {name}"),
                    span: *span,
                });
            };
            Ok((HirExpr::Local { local: info.id, span: *span }, info.ty))
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
                    message: "不支持对该类型取负号".to_string(),
                    span: *span,
                }),
            }
        }
        Expr::Call { callee, span } => {
            let scope = scopes.get(module_idx).ok_or_else(|| TszError::Type {
                message: "模块作用域索引越界（内部错误）".to_string(),
                span: *span,
            })?;
            let callee_id = scope.get(callee).copied().ok_or_else(|| TszError::Type {
                message: format!("未定义的函数: {callee}"),
                span: *span,
            })?;
            let ret_ty = func_infos
                .get(callee_id)
                .ok_or_else(|| TszError::Type {
                    message: "函数索引越界（内部错误）".to_string(),
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
                // 当前 string 运行时仅支持“字符串字面量直出”。
                if !matches!(hir, HirExpr::String { .. }) {
                    return Err(TszError::Type {
                        message: "当前 string 仅支持字符串字面量".to_string(),
                        span,
                    });
                }
            }
            Type::Void | Type::Bool => {
                return Err(TszError::Type {
                    message: "console.log 参数只支持 number/bigint/string".to_string(),
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

fn mangle_symbol(module_idx: usize, name: &str) -> String {
    // 约束：符号名必须稳定、可读、且仅包含常见字符（便于跨平台链接）。
    let sanitized: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    format!("__tsz_mod{module_idx}__{sanitized}")
}
