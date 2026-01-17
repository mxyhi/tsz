use crate::{Expr, FunctionDecl, Module, OptLevel, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::{ir, isa};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId, Linkage, Module as _};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::sync::Arc;

pub fn emit_object(module_ast: &Module, opt_level: OptLevel) -> Result<Vec<u8>, TszError> {
    // 设计目标：对 TSZ 的静态语义做“强约束”，以换取可预测布局与 AOT 优化空间。
    let isa = build_isa(opt_level)?;
    let mut object_module = build_object_module(isa)?;

    let main_fn = find_entry_main(module_ast)?;
    let user_sig = user_main_sig(&*object_module.isa(), main_fn.return_type);
    let wrapper_sig = wrapper_main_sig(&*object_module.isa());
    let ids = declare_entry_functions(&mut object_module, &user_sig, &wrapper_sig)?;

    define_user_main(&mut object_module, ids.user_main, &user_sig, main_fn)?;
    define_wrapper_main(&mut object_module, ids.wrapper_main, &wrapper_sig, ids.user_main, main_fn)?;

    finalize_and_emit(object_module)
}

struct EntryIds {
    user_main: FuncId,
    wrapper_main: FuncId,
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

fn find_entry_main(module_ast: &Module) -> Result<&FunctionDecl, TszError> {
    module_ast
        .functions
        .iter()
        .find(|f| f.is_export && f.name == "main")
        .ok_or_else(|| TszError::Codegen {
            message: "缺少 main 函数（typecheck 应该已拦截）".to_string(),
        })
}

fn declare_entry_functions(
    object_module: &mut ObjectModule,
    user_sig: &ir::Signature,
    wrapper_sig: &ir::Signature,
) -> Result<EntryIds, TszError> {
    let user_main = object_module
        .declare_function("__tsz_user_main", Linkage::Local, user_sig)
        .map_err(|e| TszError::Codegen {
            message: format!("declare user main 失败: {e}"),
        })?;

    let wrapper_main = object_module
        .declare_function("main", Linkage::Export, wrapper_sig)
        .map_err(|e| TszError::Codegen {
            message: format!("declare wrapper main 失败: {e}"),
        })?;

    Ok(EntryIds {
        user_main,
        wrapper_main,
    })
}

fn define_user_main(
    object_module: &mut ObjectModule,
    user_id: FuncId,
    user_sig: &ir::Signature,
    main_fn: &FunctionDecl,
) -> Result<(), TszError> {
    let mut ctx = object_module.make_context();
    ctx.func.signature = user_sig.clone();

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let block = fn_builder.create_block();
    fn_builder.switch_to_block(block);
    fn_builder.seal_block(block);

    let return_value = codegen_expr(&mut fn_builder, &main_fn.body, main_fn.return_type)?;
    match main_fn.return_type {
        Type::Void => fn_builder.ins().return_(&[]),
        _ => fn_builder.ins().return_(&[return_value]),
    };
    fn_builder.finalize();

    object_module
        .define_function(user_id, &mut ctx)
        .map_err(|e| TszError::Codegen {
            message: format!("define user main 失败: {e}"),
        })?;
    object_module.clear_context(&mut ctx);
    Ok(())
}

fn define_wrapper_main(
    object_module: &mut ObjectModule,
    wrapper_id: FuncId,
    wrapper_sig: &ir::Signature,
    user_id: FuncId,
    main_fn: &FunctionDecl,
) -> Result<(), TszError> {
    let mut ctx = object_module.make_context();
    ctx.func.signature = wrapper_sig.clone();

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let block = fn_builder.create_block();
    fn_builder.switch_to_block(block);
    fn_builder.seal_block(block);

    // 入口 ABI：C 的 `int main()`；用户 `main()` 的返回值映射为进程退出码。
    let callee = object_module.declare_func_in_func(user_id, fn_builder.func);
    let call = fn_builder.ins().call(callee, &[]);
    let exit_code = build_exit_code(&mut fn_builder, call, main_fn.return_type)?;
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

fn user_main_sig(target_isa: &dyn isa::TargetIsa, return_type: Type) -> ir::Signature {
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
    body: &[crate::Stmt],
    return_type: Type,
) -> Result<ir::Value, TszError> {
    // 目前 body 只支持单条 return
    let crate::Stmt::Return { expr, .. } = body
        .first()
        .ok_or_else(|| TszError::Codegen {
            message: "空函数体".to_string(),
        })?;

    match return_type {
        Type::Void => Ok(builder.ins().iconst(ir::types::I32, 0)),
        Type::BigInt => codegen_bigint_expr(builder, expr),
        Type::Number => codegen_number_expr(builder, expr),
        Type::Bool | Type::String => Err(TszError::Codegen {
            message: "当前最小实现只支持 number/bigint/void".to_string(),
        }),
    }
}

fn codegen_bigint_expr(builder: &mut FunctionBuilder<'_>, expr: &Expr) -> Result<ir::Value, TszError> {
    match expr {
        Expr::BigInt { value, .. } => Ok(builder.ins().iconst(ir::types::I64, *value)),
        Expr::UnaryMinus { expr, .. } => {
            let v = codegen_bigint_expr(builder, expr)?;
            Ok(builder.ins().ineg(v))
        }
        Expr::Number { .. } => Err(TszError::Codegen {
            message: "bigint 返回值不能是 number".to_string(),
        }),
    }
}

fn codegen_number_expr(builder: &mut FunctionBuilder<'_>, expr: &Expr) -> Result<ir::Value, TszError> {
    match expr {
        Expr::Number { value, .. } => Ok(builder.ins().f64const(*value)),
        Expr::UnaryMinus { expr, .. } => {
            let v = codegen_number_expr(builder, expr)?;
            Ok(builder.ins().fneg(v))
        }
        Expr::BigInt { .. } => Err(TszError::Codegen {
            message: "number 返回值不能是 bigint".to_string(),
        }),
    }
}
