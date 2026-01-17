use crate::{HirFunction, HirProgram, Program, Span, TszError, Type};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

mod lower;

/// Semantic analysis + minimal type checking, producing HIR for codegen.
///
/// TSZ v0/v0.1 constraints (current implementation):
/// - Functions can have typed parameters (number/bigint); entry `main()` must be 0-arg
/// - Function bodies support `let`/`const`/`console.log(...)`, and the last statement must be `return`
/// - Expressions: literals, identifiers (locals/params), unary minus, calls, + - * /, and parentheses
/// - Imports: only `import { a, b } from "<specifier>";`
pub fn analyze(program: &Program) -> Result<HirProgram, TszError> {
    let module_index = build_module_index(program)?;
    let entry_module_idx = module_index
        .get(&program.entry)
        .copied()
        .ok_or_else(|| TszError::Type {
            message: format!("Entry module not loaded: {:?}", program.entry),
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
                message: format!("Duplicate module load: {:?}", m.path),
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
    params: Vec<Type>,
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
                    message: format!("Duplicate function name in module: {} ({:?})", f.name, module.path),
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
                    message: "Duplicate function definition (internal error)".to_string(),
                    span: f.span,
                });
            }
            infos.push(FuncInfo {
                module_idx,
                func_idx,
                params: f.params.iter().map(|p| p.ty).collect(),
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

    // Insert module-local functions into scope (allow intra-module calls).
    for (id, info) in func_infos.iter().enumerate() {
        let scope = scopes
            .get_mut(info.module_idx)
            .ok_or_else(|| TszError::Type {
                message: "Module index out of bounds (internal error)".to_string(),
                span: info.span,
            })?;
        scope.insert(info.name.clone(), id);
    }

    // Then handle imports: only `export` functions can be imported.
    for (current_module_idx, module) in program.modules.iter().enumerate() {
        for import in &module.imports {
            let Some(dep_path) = &import.resolved_path else {
                return Err(TszError::Type {
                    message: "import is missing resolved_path (internal loader error)".to_string(),
                    span: import.span,
                });
            };
            let dep_idx = module_index.get(dep_path).copied().ok_or_else(|| TszError::Type {
                message: format!("Import target module not loaded: {:?}", dep_path),
                span: import.span,
            })?;

            for name in &import.names {
                let target_key = FuncKey {
                    module_path: program.modules[dep_idx].path.clone(),
                    name: name.name.clone(),
                };
                let target_id = key_to_id.get(&target_key).copied().ok_or_else(|| TszError::Type {
                    message: format!("Imported symbol does not exist: {} from {:?}", name.name, dep_path),
                    span: name.span,
                })?;

                if !func_infos[target_id].is_export {
                    return Err(TszError::Type {
                        message: format!("Only export functions can be imported: {} from {:?}", name.name, dep_path),
                        span: name.span,
                    });
                }

                let scope = &mut scopes[current_module_idx];
                if scope.contains_key(&name.name) {
                    return Err(TszError::Type {
                        message: format!(
                            "Duplicate binding name (conflicts with a local function or another import): {}",
                            name.name
                        ),
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
            message: "Entry module index out of bounds (internal error)".to_string(),
            span: Span { start: 0, end: 0 },
        })?;

    let main = entry_module
        .functions
        .iter()
        .find(|f| f.is_export && f.name == "main")
        .ok_or_else(|| TszError::Type {
            message: "Missing entry function: export function main(): <type> { ... }".to_string(),
            span: Span { start: 0, end: 0 },
        })?;

    if !main.params.is_empty() {
        return Err(TszError::Type {
            message: "Entry function main must have 0 parameters".to_string(),
            span: main.span,
        });
    }

    let key = FuncKey {
        module_path: entry_module.path.clone(),
        name: main.name.clone(),
    };
    key_to_id.get(&key).copied().ok_or_else(|| TszError::Type {
        message: "Failed to index entry function (internal error)".to_string(),
        span: main.span,
    })
}

fn validate_entry_return_type(entry_id: usize, func_infos: &[FuncInfo]) -> Result<(), TszError> {
    match func_infos
        .get(entry_id)
        .ok_or_else(|| TszError::Type {
            message: "Entry function index out of bounds (internal error)".to_string(),
            span: Span { start: 0, end: 0 },
        })?
        .return_type
    {
        Type::Number | Type::BigInt | Type::Void => Ok(()),
        Type::Bool | Type::String => Err(TszError::Type {
            message: "Entry return type only supports number/bigint/void".to_string(),
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
            message: "Module index out of bounds (internal error)".to_string(),
            span: info.span,
        })?;
        let func = module.functions.get(info.func_idx).ok_or_else(|| TszError::Type {
            message: "Function index out of bounds (internal error)".to_string(),
            span: info.span,
        })?;

        for p in &func.params {
            validate_user_fn_param_type(p.ty, p.span)?;
        }
        validate_user_fn_return_type(func.return_type, func.span)?;

        let module_scope = scopes.get(info.module_idx).ok_or_else(|| TszError::Type {
            message: "Module scope index out of bounds (internal error)".to_string(),
            span: func.span,
        })?;
        let (params, locals, body) =
            lower::lower_function_body(info.module_idx, func, module_scope, scopes, func_infos)?;

        let symbol = if id == entry_id {
            "__tsz_user_main".to_string()
        } else {
            mangle_symbol(info.module_idx, &func.name)
        };

        out.push(HirFunction {
            symbol,
            return_type: func.return_type,
            params,
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

fn validate_user_fn_param_type(ty: Type, span: Span) -> Result<(), TszError> {
    match ty {
        Type::Number | Type::BigInt | Type::Bool | Type::String => Ok(()),
        Type::Void => Err(TszError::Type {
            message: "Parameter type cannot be void".to_string(),
            span,
        }),
    }
}

fn validate_user_fn_return_type(return_type: Type, _span: Span) -> Result<(), TszError> {
    match return_type {
        Type::Number | Type::BigInt | Type::Void | Type::Bool | Type::String => Ok(()),
    }
}

fn mangle_symbol(module_idx: usize, name: &str) -> String {
    // Constraint: symbols should be stable, readable, and use only common characters (cross-platform linking).
    let sanitized: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    format!("__tsz_mod{module_idx}__{sanitized}")
}
