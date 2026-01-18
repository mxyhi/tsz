use crate::{HirProgram, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::{ir, isa};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{FuncId, Module as _};
use cranelift_object::ObjectModule;

pub(super) fn user_fn_sig(
    target_isa: &dyn isa::TargetIsa,
    params: &[crate::HirParam],
    return_type: Type,
) -> Result<ir::Signature, TszError> {
    let ptr_ty = target_isa.pointer_type();
    let mut sig = ir::Signature::new(target_isa.default_call_conv());
    for p in params {
        sig.params.push(ir::AbiParam::new(match p.ty {
            Type::Number => ir::types::F64,
            Type::BigInt => ir::types::I64,
            Type::Bool => ir::types::I8,
            Type::String => ptr_ty,
            Type::Void | Type::Error => {
                return Err(TszError::Codegen {
                    message: "Unsupported parameter type (should have been blocked by typecheck)".to_string(),
                });
            }
        }));
    }

    match return_type {
        Type::Void => {}
        Type::BigInt => sig.returns.push(ir::AbiParam::new(ir::types::I64)),
        Type::Number => sig.returns.push(ir::AbiParam::new(ir::types::F64)),
        Type::Bool => sig.returns.push(ir::AbiParam::new(ir::types::I8)),
        Type::String => sig.returns.push(ir::AbiParam::new(ptr_ty)),
        Type::Error => {
            return Err(TszError::Codegen {
                message: "Unsupported return type (error)".to_string(),
            });
        }
    }

    Ok(sig)
}

pub(super) fn wrapper_main_sig(target_isa: &dyn isa::TargetIsa) -> ir::Signature {
    let mut sig = ir::Signature::new(target_isa.default_call_conv());
    sig.returns.push(ir::AbiParam::new(ir::types::I32));
    sig
}

pub(super) fn define_wrapper_main(
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

fn build_exit_code(fn_builder: &mut FunctionBuilder<'_>, call: ir::Inst, return_type: Type) -> Result<ir::Value, TszError> {
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
        Type::Bool | Type::String | Type::Error => {
            return Err(TszError::Codegen {
                message: "Entry return type only supports number/bigint/void".to_string(),
            });
        }
    })
}
