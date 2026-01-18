use crate::{HirProgram, TszError, Type};
use cranelift_codegen::ir;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{FuncId, Module as _};
use cranelift_object::ObjectModule;

use super::runtime::RuntimeFuncs;
use super::stmt;
use super::string_pool::StringPool;

pub(super) fn define_user_fn(
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
    fn_builder.append_block_params_for_function_params(block);
    fn_builder.switch_to_block(block);
    fn_builder.seal_block(block);

    let ptr_ty = object_module.isa().pointer_type();

    // Declare param/local variables (SSA variables) and build HIR id -> Variable mappings.
    let param_vars = declare_param_vars(&mut fn_builder, block, ptr_ty, &f.params)?;
    let local_vars = declare_local_vars(&mut fn_builder, ptr_ty, f.params.len(), &f.locals)?;

    let mut state = stmt::StmtGenState::new(block);
    let fallthrough = stmt::codegen_stmt_list(
        &mut state,
        &mut fn_builder,
        object_module,
        program,
        func_ids,
        runtime,
        string_pool,
        f,
        &param_vars,
        &local_vars,
        &f.body,
    )?;
    if fallthrough {
        return Err(TszError::Codegen {
            message: format!("Missing trailing return (should have been blocked by typecheck): {}", f.source.name),
        });
    }

    fn_builder.finalize();

    object_module
        .define_function(func_id, &mut ctx)
        .map_err(|e| TszError::Codegen {
            message: format!("define_function failed: {e}"),
        })?;
    object_module.clear_context(&mut ctx);
    Ok(())
}

pub(super) fn build_user_fn_ir(
    object_module: &mut ObjectModule,
    sig: &ir::Signature,
    f: &crate::HirFunction,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
) -> Result<String, TszError> {
    let mut ctx = object_module.make_context();
    ctx.func.signature = sig.clone();

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut fn_builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
    let block = fn_builder.create_block();
    fn_builder.append_block_params_for_function_params(block);
    fn_builder.switch_to_block(block);
    fn_builder.seal_block(block);

    let ptr_ty = object_module.isa().pointer_type();

    let param_vars = declare_param_vars(&mut fn_builder, block, ptr_ty, &f.params)?;
    let local_vars = declare_local_vars(&mut fn_builder, ptr_ty, f.params.len(), &f.locals)?;

    let mut state = stmt::StmtGenState::new(block);
    let fallthrough = stmt::codegen_stmt_list(
        &mut state,
        &mut fn_builder,
        object_module,
        program,
        func_ids,
        runtime,
        string_pool,
        f,
        &param_vars,
        &local_vars,
        &f.body,
    )?;
    if fallthrough {
        return Err(TszError::Codegen {
            message: format!("Missing trailing return (should have been blocked by typecheck): {}", f.source.name),
        });
    }

    fn_builder.finalize();
    let rendered = ctx.func.display().to_string();
    object_module.clear_context(&mut ctx);
    Ok(rendered)
}

fn declare_param_vars(
    fn_builder: &mut FunctionBuilder<'_>,
    entry_block: ir::Block,
    ptr_ty: ir::Type,
    params: &[crate::HirParam],
) -> Result<Vec<Variable>, TszError> {
    // Copy out block params first: we need to mutate `fn_builder` while iterating.
    let block_params: Vec<ir::Value> = fn_builder.block_params(entry_block).to_vec();
    if block_params.len() != params.len() {
        return Err(TszError::Codegen {
            message: "Function parameter count mismatch (internal error)".to_string(),
        });
    }

    let mut param_vars = Vec::with_capacity(params.len());
    for (idx, param) in params.iter().enumerate() {
        let var = Variable::from_u32(u32::try_from(idx).map_err(|_| TszError::Codegen {
            message: "Too many parameters (exceeds u32)".to_string(),
        })?);
        let clif_ty = clif_param_ty(ptr_ty, param.ty)?;
        fn_builder.declare_var(var, clif_ty);
        fn_builder.def_var(var, block_params[idx]);
        param_vars.push(var);
    }
    Ok(param_vars)
}

fn declare_local_vars(
    fn_builder: &mut FunctionBuilder<'_>,
    ptr_ty: ir::Type,
    param_count: usize,
    locals: &[crate::HirLocal],
) -> Result<Vec<Variable>, TszError> {
    let mut local_vars = Vec::with_capacity(locals.len());
    for (idx, local) in locals.iter().enumerate() {
        let var_idx = param_count
            .checked_add(idx)
            .ok_or_else(|| TszError::Codegen {
                message: "Too many parameters + locals (overflow)".to_string(),
            })?;
        let var = Variable::from_u32(u32::try_from(var_idx).map_err(|_| TszError::Codegen {
            message: "Too many local variables (exceeds u32)".to_string(),
        })?);
        let clif_ty = clif_param_ty(ptr_ty, local.ty).map_err(|_| TszError::Codegen {
            message: format!("Unsupported local variable type: {:?}", local.ty),
        })?;
        fn_builder.declare_var(var, clif_ty);
        local_vars.push(var);
    }
    Ok(local_vars)
}

fn clif_param_ty(ptr_ty: ir::Type, ty: Type) -> Result<ir::Type, TszError> {
    match ty {
        Type::Number => Ok(ir::types::F64),
        Type::BigInt => Ok(ir::types::I64),
        Type::Bool => Ok(ir::types::I8),
        Type::String => Ok(ptr_ty),
        Type::Void | Type::Error => Err(TszError::Codegen {
            message: "Unsupported parameter type: void".to_string(),
        }),
    }
}
