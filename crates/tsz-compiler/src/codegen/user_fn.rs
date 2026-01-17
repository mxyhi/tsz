use crate::{HirExpr, HirProgram, HirStmt, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::{ir};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{FuncId, Module as _};
use cranelift_object::ObjectModule;

use super::expr;
use super::runtime::{call_runtime0, call_runtime1, call_runtime2, RuntimeFuncs};
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

    codegen_user_body(
        &mut fn_builder,
        object_module,
        program,
        func_ids,
        runtime,
        string_pool,
        f,
        &param_vars,
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
        Type::Void => Err(TszError::Codegen {
            message: "Unsupported parameter type: void".to_string(),
        }),
    }
}

fn codegen_user_body(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
) -> Result<(), TszError> {
    let (last, prefix) = func.body.split_last().ok_or_else(|| TszError::Codegen {
        message: format!("Empty function body (missing return): {}", func.source.name),
    })?;

    for stmt in prefix {
        codegen_non_return_stmt(
            builder,
            object_module,
            program,
            func_ids,
            runtime,
            string_pool,
            func,
            param_vars,
            local_vars,
            stmt,
        )?;
    }

    codegen_last_stmt(
        builder,
        object_module,
        program,
        func_ids,
        func,
        param_vars,
        local_vars,
        string_pool,
        last,
    )
}

fn codegen_non_return_stmt(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    stmt: &HirStmt,
) -> Result<(), TszError> {
    match stmt {
        HirStmt::Let { local, init, .. } => codegen_let_stmt(
            builder,
            object_module,
            program,
            func_ids,
            func,
            param_vars,
            local_vars,
            string_pool,
            *local,
            init,
        ),
        HirStmt::ConsoleLog { args, .. } => codegen_console_log_stmt(
            builder,
            object_module,
            program,
            func_ids,
            runtime,
            string_pool,
            func,
            param_vars,
            local_vars,
            args,
        ),
        HirStmt::Return { .. } => Err(TszError::Codegen {
            message: format!("return must be the last statement in the function body: {}", func.source.name),
        }),
    }
}

fn codegen_last_stmt(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    string_pool: &mut StringPool,
    last: &HirStmt,
) -> Result<(), TszError> {
    let HirStmt::Return { expr, .. } = last else {
        return Err(TszError::Codegen {
            message: format!("Missing trailing return: {}", func.source.name),
        });
    };

    codegen_return_stmt(
        builder,
        object_module,
        program,
        func_ids,
        func,
        param_vars,
        local_vars,
        string_pool,
        expr.as_ref(),
    )
}

fn codegen_let_stmt(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    string_pool: &mut StringPool,
    local: usize,
    init: &HirExpr,
) -> Result<(), TszError> {
    let var = local_vars.get(local).copied().ok_or_else(|| TszError::Codegen {
        message: "Local variable index out of bounds (internal error)".to_string(),
    })?;
    let v = expr::codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        init,
    )?;
    builder.def_var(var, v);
    Ok(())
}

fn codegen_return_stmt(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    string_pool: &mut StringPool,
    expr: Option<&HirExpr>,
) -> Result<(), TszError> {
    match func.return_type {
        Type::Void => {
            // typecheck guarantees `return;` and no expression.
            builder.ins().return_(&[]);
            Ok(())
        }
        Type::Number | Type::BigInt | Type::Bool | Type::String => {
            let Some(expr) = expr else {
                return Err(TszError::Codegen {
                    message: format!("Missing return value expression: {}", func.source.name),
                });
            };
            let v = expr::codegen_expr(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                expr,
            )?;
            builder.ins().return_(&[v]);
            Ok(())
        }
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
    param_vars: &[Variable],
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
            param_vars,
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
    param_vars: &[Variable],
    local_vars: &[Variable],
    arg: &HirExpr,
) -> Result<(), TszError> {
    match expr::hir_expr_type(program, func, arg)? {
        Type::Number => {
            let v = expr::codegen_expr(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                arg,
            )?;
            call_runtime1(builder, object_module, runtime.log_f64, v);
            Ok(())
        }
        Type::BigInt => {
            let v = expr::codegen_expr(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                arg,
            )?;
            call_runtime1(builder, object_module, runtime.log_i64, v);
            Ok(())
        }
        Type::Bool => {
            let v = expr::codegen_expr(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                arg,
            )?;
            call_runtime1(builder, object_module, runtime.log_bool, v);
            Ok(())
        }
        Type::String => codegen_console_log_string(
            builder,
            object_module,
            program,
            func_ids,
            runtime,
            string_pool,
            func,
            param_vars,
            local_vars,
            arg,
        ),
        Type::Void => Err(TszError::Codegen {
            message: "Invalid console.log argument type (should have been blocked by typecheck)".to_string(),
        }),
    }
}

fn codegen_console_log_string(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    arg: &HirExpr,
) -> Result<(), TszError> {
    // String layout: [i64 len][u8 bytes...]. A string value points to the first byte.
    let ptr = expr::codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        arg,
    )?;
    let len_addr = builder.ins().iadd_imm(ptr, -8);
    let len = builder
        .ins()
        .load(ir::types::I64, ir::MemFlags::new(), len_addr, 0);

    call_runtime2(builder, object_module, runtime.log_str, ptr, len);
    Ok(())
}
