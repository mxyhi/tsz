use crate::{HirExpr, HirProgram, OptLevel, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::{ir, isa};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId, Linkage, Module as _};
use cranelift_object::{ObjectBuilder, ObjectModule};
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

    // 定义所有 TSZ 函数。
    for (idx, f) in program.functions.iter().enumerate() {
        define_user_fn(
            &mut object_module,
            func_ids[idx],
            &func_sigs[idx],
            f,
            program,
            &func_ids,
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
) -> Result<(), TszError> {
    let mut ctx = object_module.make_context();
    ctx.func.signature = sig.clone();

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let block = fn_builder.create_block();
    fn_builder.switch_to_block(block);
    fn_builder.seal_block(block);

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

