use crate::diagnostics::Diagnostics;
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
pub fn analyze(program: &Program, diags: &mut Diagnostics) -> Result<HirProgram, TszError> {
    let module_index = build_module_index(program)?;
    let entry_module_idx = module_index
        .get(&program.entry)
        .copied()
        .ok_or_else(|| TszError::Type {
            message: format!("Entry module not loaded: {:?}", program.entry),
            span: Span { start: 0, end: 0 },
        })?;

    let (func_infos, key_to_id) = index_functions(program, diags)?;
    let scopes = build_scopes(program, &module_index, &func_infos, &key_to_id, diags)?;

    let entry_id = find_entry_main(program, entry_module_idx, &key_to_id, diags);
    if let Some(entry_id) = entry_id {
        let entry_module = program
            .modules
            .get(entry_module_idx)
            .map(|m| m.path.as_path());
        validate_entry_return_type(entry_id, &func_infos, entry_module, diags);
    }

    let hir_functions = build_hir_functions(program, &func_infos, &scopes, entry_id.unwrap_or(0), diags)?;

    Ok(HirProgram {
        entry: entry_id.unwrap_or(0),
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

fn index_functions(
    program: &Program,
    diags: &mut Diagnostics,
) -> Result<(Vec<FuncInfo>, HashMap<FuncKey, usize>), TszError> {
    let mut infos = Vec::new();
    let mut key_to_id = HashMap::new();

    for (module_idx, module) in program.modules.iter().enumerate() {
        let mut seen = HashSet::new();
        for (func_idx, f) in module.functions.iter().enumerate() {
            if !seen.insert(f.name.clone()) {
                diags.error_at(
                    &module.path,
                    f.span,
                    format!("Duplicate function name in module: {} ({:?})", f.name, module.path),
                );
                continue;
            }

            let id = infos.len();
            let key = FuncKey {
                module_path: module.path.clone(),
                name: f.name.clone(),
            };
            if key_to_id.insert(key, id).is_some() {
                diags.error_at(&module.path, f.span, "Duplicate function definition".to_string());
                continue;
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
    diags: &mut Diagnostics,
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
                diags.error_at(
                    &module.path,
                    import.span,
                    "import resolution failed (unresolved path)".to_string(),
                );
                continue;
            };
            let Some(dep_idx) = module_index.get(dep_path).copied() else {
                diags.error_at(
                    &module.path,
                    import.span,
                    format!("Import target module not loaded: {:?}", dep_path),
                );
                continue;
            };

            for name in &import.names {
                let target_key = FuncKey {
                    module_path: program.modules[dep_idx].path.clone(),
                    name: name.name.clone(),
                };
                let Some(target_id) = key_to_id.get(&target_key).copied() else {
                    diags.error_at(
                        &module.path,
                        name.span,
                        format!("Imported symbol does not exist: {} from {:?}", name.name, dep_path),
                    );
                    continue;
                };

                if !func_infos[target_id].is_export {
                    diags.error_at(
                        &module.path,
                        name.span,
                        format!("Only export functions can be imported: {} from {:?}", name.name, dep_path),
                    );
                    continue;
                }

                let scope = &mut scopes[current_module_idx];
                if scope.contains_key(&name.name) {
                    diags.error_at(
                        &module.path,
                        name.span,
                        format!(
                            "Duplicate binding name (conflicts with a local function or another import): {}",
                            name.name
                        ),
                    );
                    continue;
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
    diags: &mut Diagnostics,
) -> Option<usize> {
    let entry_module = program.modules.get(entry_module_idx)?;

    let main = entry_module
        .functions
        .iter()
        .find(|f| f.is_export && f.name == "main");

    let Some(main) = main else {
        diags.error_at(
            &entry_module.path,
            Span { start: 0, end: 0 },
            "Missing entry function: export function main(): <type> { ... }".to_string(),
        );
        return None;
    };

    if !main.params.is_empty() {
        diags.error_at(
            &entry_module.path,
            main.span,
            "Entry function main must have 0 parameters".to_string(),
        );
        return None;
    }

    let key = FuncKey {
        module_path: entry_module.path.clone(),
        name: main.name.clone(),
    };
    key_to_id.get(&key).copied()
}

fn validate_entry_return_type(
    entry_id: usize,
    func_infos: &[FuncInfo],
    entry_module: Option<&std::path::Path>,
    diags: &mut Diagnostics,
) {
    let Some(info) = func_infos.get(entry_id) else { return };
    match info.return_type {
        Type::Number | Type::BigInt | Type::Void => {}
        Type::Bool | Type::String => {
            if let Some(path) = entry_module {
                diags.error_at(
                    path,
                    Span { start: 0, end: 0 },
                    "Entry return type only supports number/bigint/void".to_string(),
                );
            } else {
                diags.error("Entry return type only supports number/bigint/void");
            }
        }
        Type::Error => {}
    }
}

fn build_hir_functions(
    program: &Program,
    func_infos: &[FuncInfo],
    scopes: &[HashMap<String, usize>],
    entry_id: usize,
    diags: &mut Diagnostics,
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
            validate_user_fn_param_type(diags, &module.path, p.ty, p.span);
        }
        validate_user_fn_return_type(diags, &module.path, func.return_type, func.span);

        let module_scope = scopes.get(info.module_idx).ok_or_else(|| TszError::Type {
            message: "Module scope index out of bounds (internal error)".to_string(),
            span: func.span,
        })?;
        let (params, locals, body) =
            lower::lower_function_body(info.module_idx, func, &module.path, module_scope, scopes, func_infos, diags)?;

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

fn validate_user_fn_param_type(diags: &mut Diagnostics, module_path: &std::path::Path, ty: Type, span: Span) {
    match ty {
        Type::Number | Type::BigInt | Type::Bool | Type::String => {}
        Type::Void => {
            diags.error_at(module_path, span, "Parameter type cannot be void");
        }
        Type::Error => {}
    }
}

fn validate_user_fn_return_type(_diags: &mut Diagnostics, _module_path: &std::path::Path, return_type: Type, _span: Span) {
    match return_type {
        Type::Number | Type::BigInt | Type::Void | Type::Bool | Type::String | Type::Error => {}
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
