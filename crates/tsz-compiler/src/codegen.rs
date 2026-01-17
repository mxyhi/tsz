use crate::{HirExpr, HirProgram, HirStmt, OptLevel, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::{ir, isa};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module as _};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;
use std::sync::Arc;

pub fn emit_object(program: &HirProgram, opt_level: OptLevel) -> Result<Vec<u8>, TszError> {
    let isa = build_isa(opt_level)?;
    let mut object_module = build_object_module(isa)?;

    // Declare all TSZ functions first so later `call` sites can resolve `FuncId`.
    let mut func_sigs = Vec::with_capacity(program.functions.len());
    let mut func_ids = Vec::with_capacity(program.functions.len());
    for f in &program.functions {
        let sig = user_fn_sig(&*object_module.isa(), f.return_type);
        let id = object_module
            .declare_function(&f.symbol, Linkage::Local, &sig)
            .map_err(|e| TszError::Codegen {
                message: format!("declare_function failed: {}: {e}", f.symbol),
            })?;
        func_sigs.push(sig);
        func_ids.push(id);
    }

    // Wrapper entry: C ABI `int main()` (exported).
    let wrapper_sig = wrapper_main_sig(&*object_module.isa());
    let wrapper_id = object_module
        .declare_function("main", Linkage::Export, &wrapper_sig)
        .map_err(|e| TszError::Codegen {
            message: format!("declare wrapper main failed: {e}"),
        })?;

    let runtime = declare_runtime_funcs(&mut object_module)?;
    let mut string_pool = StringPool::new();

    // Define all TSZ functions.
    for (idx, f) in program.functions.iter().enumerate() {
        define_user_fn(
            &mut object_module,
            func_ids[idx],
            &func_sigs[idx],
            f,
            program,
            &func_ids,
            &runtime,
            &mut string_pool,
        )?;
    }

    // Define wrapper main.
    define_wrapper_main(&mut object_module, wrapper_id, &wrapper_sig, program, &func_ids)?;

    finalize_and_emit(object_module)
}

fn build_isa(opt_level: OptLevel) -> Result<Arc<dyn isa::TargetIsa>, TszError> {
    let opt = match opt_level {
        OptLevel::None => "none",
        OptLevel::Speed => "speed",
    };
    let mut flag_builder = settings::builder();
    flag_builder
        .set("opt_level", opt)
        .map_err(|e| TszError::Codegen {
            message: format!("Failed to set cranelift opt_level: {e}"),
        })?;
    // macOS/modern Linux default to PIE; the executable should be PIC to avoid text relocations.
    flag_builder
        .set("is_pic", "true")
        .map_err(|e| TszError::Codegen {
            message: format!("Failed to set cranelift is_pic: {e}"),
        })?;
    let flags = settings::Flags::new(flag_builder);

    let isa_builder = cranelift_native::builder().map_err(|e| TszError::Codegen {
        message: format!("Failed to get native ISA builder: {e}"),
    })?;
    isa_builder.finish(flags).map_err(|e| TszError::Codegen {
        message: format!("Failed to build ISA: {e}"),
    })
}

fn build_object_module(isa: Arc<dyn isa::TargetIsa>) -> Result<ObjectModule, TszError> {
    let builder = ObjectBuilder::new(
        isa,
        "tsz",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| TszError::Codegen {
        message: format!("Failed to create ObjectBuilder: {e}"),
    })?;
    Ok(ObjectModule::new(builder))
}

fn define_user_fn(
    object_module: &mut ObjectModule,
    func_id: FuncId,
    sig: &ir::Signature,
    f: &crate::HirFunction,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
) -> Result<(), TszError> {
    let mut ctx = object_module.make_context();
    ctx.func.signature = sig.clone();

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let block = fn_builder.create_block();
    fn_builder.switch_to_block(block);
    fn_builder.seal_block(block);

    // Declare local variables (SSA variables) and build the HirLocalId -> Variable mapping.
    let local_vars = declare_local_vars(&mut fn_builder, &f.locals)?;
    codegen_user_body(
        &mut fn_builder,
        object_module,
        program,
        func_ids,
        runtime,
        string_pool,
        f,
        &local_vars,
    )?;

    fn_builder.finalize();

    object_module
        .define_function(func_id, &mut ctx)
        .map_err(|e| TszError::Codegen {
            message: format!("define_function failed: {e}"),
        })?;
    object_module.clear_context(&mut ctx);
    Ok(())
}

fn declare_local_vars(
    fn_builder: &mut FunctionBuilder<'_>,
    locals: &[crate::HirLocal],
) -> Result<Vec<Variable>, TszError> {
    let mut local_vars = Vec::with_capacity(locals.len());
    for (idx, local) in locals.iter().enumerate() {
        let var = Variable::from_u32(u32::try_from(idx).map_err(|_| TszError::Codegen {
            message: "Too many local variables (exceeds u32)".to_string(),
        })?);
        let clif_ty = match local.ty {
            Type::Number => ir::types::F64,
            Type::BigInt => ir::types::I64,
            Type::Void | Type::Bool | Type::String => {
                return Err(TszError::Codegen {
                    message: format!("Unsupported local variable type: {:?}", local.ty),
                });
            }
        };
        fn_builder.declare_var(var, clif_ty);
        local_vars.push(var);
    }
    Ok(local_vars)
}

fn codegen_user_body(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    local_vars: &[Variable],
) -> Result<(), TszError> {
    let (last, prefix) = func
        .body
        .split_last()
        .ok_or_else(|| TszError::Codegen {
            message: format!("Empty function body (missing return): {}", func.source.name),
        })?;

    for stmt in prefix {
        match stmt {
            HirStmt::Let { local, init, .. } => {
                codegen_let_stmt(builder, object_module, program, func_ids, func, local_vars, *local, init)?
            }
            HirStmt::ConsoleLog { args, .. } => codegen_console_log_stmt(
                builder,
                object_module,
                program,
                func_ids,
                runtime,
                string_pool,
                func,
                local_vars,
                args,
            )?,
            HirStmt::Return { .. } => {
                return Err(TszError::Codegen {
                    message: format!("return must be the last statement in the function body: {}", func.source.name),
                });
            }
        }
    }

    let HirStmt::Return { expr, .. } = last else {
        return Err(TszError::Codegen {
            message: format!("Missing trailing return: {}", func.source.name),
        });
    };
    codegen_return_stmt(builder, object_module, program, func_ids, func, local_vars, expr.as_ref())
}

fn codegen_let_stmt(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    local_vars: &[Variable],
    local: usize,
    init: &HirExpr,
) -> Result<(), TszError> {
    let var = local_vars.get(local).copied().ok_or_else(|| TszError::Codegen {
        message: "Local variable index out of bounds (internal error)".to_string(),
    })?;
    let v = codegen_expr(builder, object_module, program, func_ids, func, local_vars, init)?;
    builder.def_var(var, v);
    Ok(())
}

fn codegen_return_stmt(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    local_vars: &[Variable],
    expr: Option<&HirExpr>,
) -> Result<(), TszError> {
    match func.return_type {
        Type::Void => {
            // typecheck guarantees `return;` and no expression.
            builder.ins().return_(&[]);
            Ok(())
        }
        Type::Number | Type::BigInt => {
            let Some(expr) = expr else {
                return Err(TszError::Codegen {
                    message: format!("Missing return value expression: {}", func.source.name),
                });
            };
            let v = codegen_expr(builder, object_module, program, func_ids, func, local_vars, expr)?;
            builder.ins().return_(&[v]);
            Ok(())
        }
        Type::Bool | Type::String => Err(TszError::Codegen {
            message: "The current minimal subset only supports number/bigint/void".to_string(),
        }),
    }
}

fn define_wrapper_main(
    object_module: &mut ObjectModule,
    wrapper_id: FuncId,
    wrapper_sig: &ir::Signature,
    program: &HirProgram,
    func_ids: &[FuncId],
) -> Result<(), TszError> {
    let mut ctx = object_module.make_context();
    ctx.func.signature = wrapper_sig.clone();

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let block = fn_builder.create_block();
    fn_builder.switch_to_block(block);
    fn_builder.seal_block(block);

    // Entry ABI: C `int main()`; the user `main()` return value maps to the process exit code.
    let user_id = func_ids
        .get(program.entry)
        .copied()
        .ok_or_else(|| TszError::Codegen {
            message: "Entry function FuncId missing (internal error)".to_string(),
        })?;
    let callee = object_module.declare_func_in_func(user_id, fn_builder.func);
    let call = fn_builder.ins().call(callee, &[]);
    let exit_code = build_exit_code(&mut fn_builder, call, program.functions[program.entry].return_type)?;

    fn_builder.ins().return_(&[exit_code]);
    fn_builder.finalize();

    object_module
        .define_function(wrapper_id, &mut ctx)
        .map_err(|e| TszError::Codegen {
            message: format!("define wrapper main failed: {e}"),
        })?;
    object_module.clear_context(&mut ctx);
    Ok(())
}

fn build_exit_code(
    fn_builder: &mut FunctionBuilder<'_>,
    call: ir::Inst,
    return_type: Type,
) -> Result<ir::Value, TszError> {
    Ok(match return_type {
        Type::Void => fn_builder.ins().iconst(ir::types::I32, 0),
        Type::BigInt => {
            let v = fn_builder.inst_results(call)[0];
            fn_builder.ins().ireduce(ir::types::I32, v)
        }
        Type::Number => {
            let v = fn_builder.inst_results(call)[0];
            fn_builder.ins().fcvt_to_sint(ir::types::I32, v)
        }
        Type::Bool | Type::String => {
            return Err(TszError::Codegen {
                message: "Entry return type only supports number/bigint/void".to_string(),
            });
        }
    })
}

fn finalize_and_emit(object_module: ObjectModule) -> Result<Vec<u8>, TszError> {
    let product = object_module.finish();
    product.emit().map_err(|e| TszError::Codegen {
        message: format!("emit object failed: {e}"),
    })
}

fn user_fn_sig(target_isa: &dyn isa::TargetIsa, return_type: Type) -> ir::Signature {
    let mut sig = ir::Signature::new(target_isa.default_call_conv());
    match return_type {
        Type::Void => {}
        Type::BigInt => sig.returns.push(ir::AbiParam::new(ir::types::I64)),
        Type::Number => sig.returns.push(ir::AbiParam::new(ir::types::F64)),
        Type::Bool => sig.returns.push(ir::AbiParam::new(ir::types::I8)),
        Type::String => sig.returns.push(ir::AbiParam::new(ir::types::I64)),
    }
    sig
}

fn wrapper_main_sig(target_isa: &dyn isa::TargetIsa) -> ir::Signature {
    let mut sig = ir::Signature::new(target_isa.default_call_conv());
    sig.returns.push(ir::AbiParam::new(ir::types::I32));
    sig
}

fn codegen_expr(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    local_vars: &[Variable],
    expr: &HirExpr,
) -> Result<ir::Value, TszError> {
    let ty = hir_expr_type(program, func, expr)?;

    match expr {
        HirExpr::Number { value, .. } => Ok(builder.ins().f64const(*value)),
        HirExpr::BigInt { value, .. } => Ok(builder.ins().iconst(ir::types::I64, *value)),
        HirExpr::String { .. } => Err(TszError::Codegen {
            message: "String expressions are currently only supported in console.log".to_string(),
        }),
        HirExpr::Local { local, .. } => {
            let var = local_vars.get(*local).copied().ok_or_else(|| TszError::Codegen {
                message: "Local variable index out of bounds (internal error)".to_string(),
            })?;
            Ok(builder.use_var(var))
        }
        HirExpr::UnaryMinus { expr, .. } => {
            let v = codegen_expr(builder, object_module, program, func_ids, func, local_vars, expr)?;
            match ty {
                Type::BigInt => Ok(builder.ins().ineg(v)),
                Type::Number => Ok(builder.ins().fneg(v)),
                Type::Void | Type::Bool | Type::String => Err(TszError::Codegen {
                    message: "Invalid unary minus type (should have been blocked by typecheck)".to_string(),
                }),
            }
        }
        HirExpr::Call { callee, .. } => {
            let callee_id = func_ids.get(*callee).copied().ok_or_else(|| TszError::Codegen {
                message: "callee FuncId missing (internal error)".to_string(),
            })?;
            let callee_ref = object_module.declare_func_in_func(callee_id, builder.func);
            let call = builder.ins().call(callee_ref, &[]);

            match ty {
                Type::Void => Err(TszError::Codegen {
                    message: "A void function cannot be used as an expression value".to_string(),
                }),
                Type::BigInt | Type::Number | Type::Bool | Type::String => Ok(builder.inst_results(call)[0]),
            }
        }
    }
}

fn hir_expr_type(program: &HirProgram, func: &crate::HirFunction, expr: &HirExpr) -> Result<Type, TszError> {
    Ok(match expr {
        HirExpr::Number { .. } => Type::Number,
        HirExpr::BigInt { .. } => Type::BigInt,
        HirExpr::String { .. } => Type::String,
        HirExpr::Local { local, .. } => func
            .locals
            .get(*local)
            .ok_or_else(|| TszError::Codegen {
                message: "Local variable index out of bounds (internal error)".to_string(),
            })?
            .ty,
        HirExpr::UnaryMinus { expr, .. } => hir_expr_type(program, func, expr)?,
        HirExpr::Call { callee, .. } => program
            .functions
            .get(*callee)
            .ok_or_else(|| TszError::Codegen {
                message: "callee HIR out of bounds (internal error)".to_string(),
            })?
            .return_type,
    })
}

#[derive(Debug, Clone)]
struct RuntimeFuncs {
    log_i64: FuncId,
    log_f64: FuncId,
    log_str: FuncId,
    log_space: FuncId,
    log_newline: FuncId,
}

fn declare_runtime_funcs(object_module: &mut ObjectModule) -> Result<RuntimeFuncs, TszError> {
    let ptr_ty = object_module.isa().pointer_type();

    let log_i64 = declare_import_fn(
        object_module,
        "tsz_log_i64",
        &[ir::types::I64],
        &[],
    )?;
    let log_f64 = declare_import_fn(
        object_module,
        "tsz_log_f64",
        &[ir::types::F64],
        &[],
    )?;
    let log_str = declare_import_fn(
        object_module,
        "tsz_log_str",
        &[ptr_ty, ir::types::I64],
        &[],
    )?;
    let log_space = declare_import_fn(object_module, "tsz_log_space", &[], &[])?;
    let log_newline = declare_import_fn(object_module, "tsz_log_newline", &[], &[])?;

    Ok(RuntimeFuncs {
        log_i64,
        log_f64,
        log_str,
        log_space,
        log_newline,
    })
}

fn declare_import_fn(
    object_module: &mut ObjectModule,
    name: &str,
    params: &[ir::Type],
    returns: &[ir::Type],
) -> Result<FuncId, TszError> {
    let mut sig = ir::Signature::new(object_module.isa().default_call_conv());
    for &p in params {
        sig.params.push(ir::AbiParam::new(p));
    }
    for &r in returns {
        sig.returns.push(ir::AbiParam::new(r));
    }
    object_module
        .declare_function(name, Linkage::Import, &sig)
        .map_err(|e| TszError::Codegen {
            message: format!("declare runtime function failed: {name}: {e}"),
        })
}

#[derive(Debug, Default)]
struct StringPool {
    next_id: usize,
    ids: HashMap<String, DataId>,
}

impl StringPool {
    fn new() -> Self {
        Self::default()
    }
}

fn codegen_console_log_stmt(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    local_vars: &[Variable],
    args: &[HirExpr],
) -> Result<(), TszError> {
    for (idx, arg) in args.iter().enumerate() {
        if idx != 0 {
            call_runtime0(builder, object_module, runtime.log_space);
        }
        codegen_console_log_arg(
            builder,
            object_module,
            program,
            func_ids,
            runtime,
            string_pool,
            func,
            local_vars,
            arg,
        )?;
    }

    call_runtime0(builder, object_module, runtime.log_newline);
    Ok(())
}

fn codegen_console_log_arg(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    local_vars: &[Variable],
    arg: &HirExpr,
) -> Result<(), TszError> {
    match hir_expr_type(program, func, arg)? {
        Type::Number => {
            let v = codegen_expr(builder, object_module, program, func_ids, func, local_vars, arg)?;
            call_runtime1(builder, object_module, runtime.log_f64, v);
            Ok(())
        }
        Type::BigInt => {
            let v = codegen_expr(builder, object_module, program, func_ids, func, local_vars, arg)?;
            call_runtime1(builder, object_module, runtime.log_i64, v);
            Ok(())
        }
        Type::String => codegen_console_log_string(builder, object_module, runtime, string_pool, arg),
        Type::Void | Type::Bool => Err(TszError::Codegen {
            message: "Invalid console.log argument type (should have been blocked by typecheck)".to_string(),
        }),
    }
}

fn codegen_console_log_string(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    arg: &HirExpr,
) -> Result<(), TszError> {
    let HirExpr::String { value, .. } = arg else {
        return Err(TszError::Codegen {
            message: "string argument is not a string literal (should have been blocked by typecheck)".to_string(),
        });
    };

    let data_id = get_or_define_string_data(object_module, string_pool, value)?;
    let gv = object_module.declare_data_in_func(data_id, builder.func);
    let ptr_ty = object_module.isa().pointer_type();
    let ptr = builder.ins().global_value(ptr_ty, gv);
    let len = builder.ins().iconst(ir::types::I64, value.as_bytes().len() as i64);

    call_runtime2(builder, object_module, runtime.log_str, ptr, len);
    Ok(())
}

fn get_or_define_string_data(
    object_module: &mut ObjectModule,
    pool: &mut StringPool,
    s: &str,
) -> Result<DataId, TszError> {
    if let Some(id) = pool.ids.get(s).copied() {
        return Ok(id);
    }

    let name = format!("__tsz_str_{}", pool.next_id);
    pool.next_id += 1;
    let data_id = object_module
        .declare_data(&name, Linkage::Local, false, false)
        .map_err(|e| TszError::Codegen {
            message: format!("declare data failed: {name}: {e}"),
        })?;

    let mut data = DataDescription::new();
    data.define(s.as_bytes().to_vec().into_boxed_slice());
    object_module.define_data(data_id, &data).map_err(|e| TszError::Codegen {
        message: format!("define data failed: {name}: {e}"),
    })?;

    pool.ids.insert(s.to_string(), data_id);
    Ok(data_id)
}

fn call_runtime0(builder: &mut FunctionBuilder<'_>, object_module: &mut ObjectModule, func: FuncId) {
    let callee = object_module.declare_func_in_func(func, builder.func);
    builder.ins().call(callee, &[]);
}

fn call_runtime1(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    func: FuncId,
    a0: ir::Value,
) {
    let callee = object_module.declare_func_in_func(func, builder.func);
    builder.ins().call(callee, &[a0]);
}

fn call_runtime2(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    func: FuncId,
    a0: ir::Value,
    a1: ir::Value,
) {
    let callee = object_module.declare_func_in_func(func, builder.func);
    builder.ins().call(callee, &[a0, a1]);
}
