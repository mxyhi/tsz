use crate::{HirConsoleLog, HirExpr, HirProgram, OptLevel, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::{ir, isa};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module as _};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;
use std::sync::Arc;

pub fn emit_object(program: &HirProgram, opt_level: OptLevel) -> Result<Vec<u8>, TszError> {
    let isa = build_isa(opt_level)?;
    let mut object_module = build_object_module(isa)?;

    // 先声明所有 TSZ 函数，确保后续 call 时都能拿到 FuncId。
    let mut func_sigs = Vec::with_capacity(program.functions.len());
    let mut func_ids = Vec::with_capacity(program.functions.len());
    for f in &program.functions {
        let sig = user_fn_sig(&*object_module.isa(), f.return_type);
        let id = object_module
            .declare_function(&f.symbol, Linkage::Local, &sig)
            .map_err(|e| TszError::Codegen {
                message: format!("declare function 失败: {}: {e}", f.symbol),
            })?;
        func_sigs.push(sig);
        func_ids.push(id);
    }

    // 壳入口：C ABI 的 `int main()`（对外导出）。
    let wrapper_sig = wrapper_main_sig(&*object_module.isa());
    let wrapper_id = object_module
        .declare_function("main", Linkage::Export, &wrapper_sig)
        .map_err(|e| TszError::Codegen {
            message: format!("declare wrapper main 失败: {e}"),
        })?;

    let runtime = declare_runtime_funcs(&mut object_module)?;
    let mut string_pool = StringPool::new();

    // 定义所有 TSZ 函数。
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

    // 定义 wrapper main。
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
            message: format!("设置 cranelift opt_level 失败: {e}"),
        })?;
    // macOS/现代 Linux 默认链接 PIE，可执行文件要求代码为 PIC，避免 text-relocation。
    flag_builder
        .set("is_pic", "true")
        .map_err(|e| TszError::Codegen {
            message: format!("设置 cranelift is_pic 失败: {e}"),
        })?;
    let flags = settings::Flags::new(flag_builder);

    let isa_builder = cranelift_native::builder().map_err(|e| TszError::Codegen {
        message: format!("获取本机 ISA 失败: {e}"),
    })?;
    isa_builder.finish(flags).map_err(|e| TszError::Codegen {
        message: format!("构建 ISA 失败: {e}"),
    })
}

fn build_object_module(isa: Arc<dyn isa::TargetIsa>) -> Result<ObjectModule, TszError> {
    let builder = ObjectBuilder::new(
        isa,
        "tsz",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| TszError::Codegen {
        message: format!("创建 ObjectBuilder 失败: {e}"),
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

    for log in &f.body.logs {
        codegen_console_log_stmt(
            &mut fn_builder,
            object_module,
            program,
            func_ids,
            runtime,
            string_pool,
            log,
        )?;
    }

    match f.return_type {
        Type::Void => {
            // typecheck 已保证 return; / expr 不存在
            fn_builder.ins().return_(&[]);
        }
        Type::Number | Type::BigInt => {
            let Some(expr) = &f.body.ret.expr else {
                return Err(TszError::Codegen {
                    message: format!("缺少返回值表达式: {}", f.source.name),
                });
            };
            let v = codegen_expr(&mut fn_builder, object_module, program, func_ids, expr)?;
            fn_builder.ins().return_(&[v]);
        }
        Type::Bool | Type::String => {
            return Err(TszError::Codegen {
                message: "当前最小实现只支持 number/bigint/void".to_string(),
            });
        }
    }

    fn_builder.finalize();

    object_module
        .define_function(func_id, &mut ctx)
        .map_err(|e| TszError::Codegen {
            message: format!("define function 失败: {e}"),
        })?;
    object_module.clear_context(&mut ctx);
    Ok(())
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

    // 入口 ABI：C 的 `int main()`；用户 `main()` 的返回值映射为进程退出码。
    let user_id = func_ids
        .get(program.entry)
        .copied()
        .ok_or_else(|| TszError::Codegen {
            message: "入口函数 FuncId 缺失（内部错误）".to_string(),
        })?;
    let callee = object_module.declare_func_in_func(user_id, fn_builder.func);
    let call = fn_builder.ins().call(callee, &[]);
    let exit_code = build_exit_code(&mut fn_builder, call, program.functions[program.entry].return_type)?;

    fn_builder.ins().return_(&[exit_code]);
    fn_builder.finalize();

    object_module
        .define_function(wrapper_id, &mut ctx)
        .map_err(|e| TszError::Codegen {
            message: format!("define wrapper main 失败: {e}"),
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
                message: "当前入口返回类型只支持 number/bigint/void".to_string(),
            });
        }
    })
}

fn finalize_and_emit(object_module: ObjectModule) -> Result<Vec<u8>, TszError> {
    let product = object_module.finish();
    product.emit().map_err(|e| TszError::Codegen {
        message: format!("emit object 失败: {e}"),
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
    expr: &HirExpr,
) -> Result<ir::Value, TszError> {
    let ty = hir_expr_type(program, expr)?;

    match expr {
        HirExpr::Number { value, .. } => Ok(builder.ins().f64const(*value)),
        HirExpr::BigInt { value, .. } => Ok(builder.ins().iconst(ir::types::I64, *value)),
        HirExpr::String { .. } => Err(TszError::Codegen {
            message: "string 表达式当前仅支持用于 console.log".to_string(),
        }),
        HirExpr::UnaryMinus { expr, .. } => {
            let v = codegen_expr(builder, object_module, program, func_ids, expr)?;
            match ty {
                Type::BigInt => Ok(builder.ins().ineg(v)),
                Type::Number => Ok(builder.ins().fneg(v)),
                Type::Void | Type::Bool | Type::String => Err(TszError::Codegen {
                    message: "无效的一元负号类型（typecheck 应已拦截）".to_string(),
                }),
            }
        }
        HirExpr::Call { callee, .. } => {
            let callee_id = func_ids.get(*callee).copied().ok_or_else(|| TszError::Codegen {
                message: "callee FuncId 缺失（内部错误）".to_string(),
            })?;
            let callee_ref = object_module.declare_func_in_func(callee_id, builder.func);
            let call = builder.ins().call(callee_ref, &[]);

            match ty {
                Type::Void => Err(TszError::Codegen {
                    message: "void 函数不能作为表达式值使用".to_string(),
                }),
                Type::BigInt | Type::Number | Type::Bool | Type::String => Ok(builder.inst_results(call)[0]),
            }
        }
    }
}

fn hir_expr_type(program: &HirProgram, expr: &HirExpr) -> Result<Type, TszError> {
    Ok(match expr {
        HirExpr::Number { .. } => Type::Number,
        HirExpr::BigInt { .. } => Type::BigInt,
        HirExpr::String { .. } => Type::String,
        HirExpr::UnaryMinus { expr, .. } => hir_expr_type(program, expr)?,
        HirExpr::Call { callee, .. } => program
            .functions
            .get(*callee)
            .ok_or_else(|| TszError::Codegen {
                message: "callee HIR 越界（内部错误）".to_string(),
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
            message: format!("declare runtime function 失败: {name}: {e}"),
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
    log: &HirConsoleLog,
) -> Result<(), TszError> {
    for (idx, arg) in log.args.iter().enumerate() {
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
    arg: &HirExpr,
) -> Result<(), TszError> {
    match hir_expr_type(program, arg)? {
        Type::Number => {
            let v = codegen_expr(builder, object_module, program, func_ids, arg)?;
            call_runtime1(builder, object_module, runtime.log_f64, v);
            Ok(())
        }
        Type::BigInt => {
            let v = codegen_expr(builder, object_module, program, func_ids, arg)?;
            call_runtime1(builder, object_module, runtime.log_i64, v);
            Ok(())
        }
        Type::String => codegen_console_log_string(builder, object_module, runtime, string_pool, arg),
        Type::Void | Type::Bool => Err(TszError::Codegen {
            message: "console.log 参数类型非法（typecheck 应已拦截）".to_string(),
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
            message: "string 参数不是字符串字面量（typecheck 应已拦截）".to_string(),
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
            message: format!("declare data 失败: {name}: {e}"),
        })?;

    let mut data = DataDescription::new();
    data.define(s.as_bytes().to_vec().into_boxed_slice());
    object_module.define_data(data_id, &data).map_err(|e| TszError::Codegen {
        message: format!("define data 失败: {name}: {e}"),
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
