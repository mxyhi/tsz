use crate::{HirExpr, HirProgram, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::{self};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{DataId, FuncId, Module as _};
use cranelift_object::ObjectModule;

use super::string_pool::StringPool;
mod ops;
use ops::{codegen_binary, codegen_logical_and, codegen_logical_or, codegen_unary_minus, codegen_unary_not};

pub(super) fn codegen_expr(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    expr: &HirExpr,
) -> Result<ir::Value, TszError> {
    match expr {
        HirExpr::Number { value, .. } => Ok(builder.ins().f64const(*value)),
        HirExpr::BigInt { value, .. } => Ok(builder.ins().iconst(ir::types::I64, *value)),
        HirExpr::Bool { value, .. } => Ok(builder.ins().iconst(ir::types::I8, if *value { 1 } else { 0 })),
        HirExpr::String { value, .. } => codegen_string_literal(builder, object_module, string_pool, value),
        HirExpr::Param { param, .. } => codegen_param_value(builder, param_vars, *param),
        HirExpr::Local { local, .. } => codegen_local_value(builder, local_vars, *local),
        HirExpr::UnaryMinus { expr, .. } => {
            let ty = hir_expr_type(program, func, expr)?;
            codegen_unary_minus(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                ty,
                expr,
            )
        }
        HirExpr::UnaryNot { expr, .. } => codegen_unary_not(
            builder,
            object_module,
            program,
            func_ids,
            string_pool,
            func,
            param_vars,
            local_vars,
            expr,
        ),
        HirExpr::Binary { op, left, right, .. } => match op {
            crate::HirBinaryOp::And => codegen_logical_and(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                left,
                right,
            ),
            crate::HirBinaryOp::Or => codegen_logical_or(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                left,
                right,
            ),
            _ => codegen_binary(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                *op,
                left,
                right,
            ),
        },
        HirExpr::Call { callee, args, .. } => {
            let ty = hir_expr_type(program, func, expr)?;
            codegen_call(
                builder,
                object_module,
                program,
                func_ids,
                string_pool,
                func,
                param_vars,
                local_vars,
                ty,
                *callee,
                args,
            )
        }
    }
}

fn codegen_string_literal(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    string_pool: &mut StringPool,
    value: &str,
) -> Result<ir::Value, TszError> {
    let data_id: DataId = string_pool.get_or_define_data(object_module, value)?;
    let gv = object_module.declare_data_in_func(data_id, builder.func);
    let ptr_ty = object_module.isa().pointer_type();
    let base_ptr = builder.ins().global_value(ptr_ty, gv);
    // String layout: [i64 len][u8 bytes...]. A string value points to the first byte.
    Ok(builder.ins().iadd_imm(base_ptr, 8))
}

fn codegen_param_value(
    builder: &mut FunctionBuilder<'_>,
    param_vars: &[Variable],
    param: usize,
) -> Result<ir::Value, TszError> {
    let var = param_vars.get(param).copied().ok_or_else(|| TszError::Codegen {
        message: "Parameter index out of bounds (internal error)".to_string(),
    })?;
    Ok(builder.use_var(var))
}

fn codegen_local_value(
    builder: &mut FunctionBuilder<'_>,
    local_vars: &[Variable],
    local: usize,
) -> Result<ir::Value, TszError> {
    let var = local_vars.get(local).copied().ok_or_else(|| TszError::Codegen {
        message: "Local variable index out of bounds (internal error)".to_string(),
    })?;
    Ok(builder.use_var(var))
}


fn codegen_call(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    ty: Type,
    callee: usize,
    args: &[HirExpr],
) -> Result<ir::Value, TszError> {
    let callee_id = func_ids.get(callee).copied().ok_or_else(|| TszError::Codegen {
        message: "callee FuncId missing (internal error)".to_string(),
    })?;
    let callee_ref = object_module.declare_func_in_func(callee_id, builder.func);

    let mut arg_values = Vec::with_capacity(args.len());
    for arg in args {
        arg_values.push(codegen_expr(
            builder,
            object_module,
            program,
            func_ids,
            string_pool,
            func,
            param_vars,
            local_vars,
            arg,
        )?);
    }

    let call = builder.ins().call(callee_ref, &arg_values);
    match ty {
        Type::Void => Err(TszError::Codegen {
            message: "A void function cannot be used as an expression value".to_string(),
        }),
        Type::BigInt | Type::Number | Type::Bool | Type::String => Ok(builder.inst_results(call)[0]),
    }
}

pub(super) fn hir_expr_type(program: &HirProgram, func: &crate::HirFunction, expr: &HirExpr) -> Result<Type, TszError> {
    Ok(match expr {
        HirExpr::Number { .. } => Type::Number,
        HirExpr::BigInt { .. } => Type::BigInt,
        HirExpr::Bool { .. } => Type::Bool,
        HirExpr::String { .. } => Type::String,
        HirExpr::Param { param, .. } => func
            .params
            .get(*param)
            .ok_or_else(|| TszError::Codegen {
                message: "Parameter index out of bounds (internal error)".to_string(),
            })?
            .ty,
        HirExpr::Local { local, .. } => func
            .locals
            .get(*local)
            .ok_or_else(|| TszError::Codegen {
                message: "Local variable index out of bounds (internal error)".to_string(),
            })?
            .ty,
        HirExpr::UnaryMinus { expr, .. } => hir_expr_type(program, func, expr)?,
        HirExpr::UnaryNot { .. } => Type::Bool,
        HirExpr::Binary { op, left, .. } => match op {
            crate::HirBinaryOp::Add
            | crate::HirBinaryOp::Sub
            | crate::HirBinaryOp::Mul
            | crate::HirBinaryOp::Div => hir_expr_type(program, func, left)?,
            crate::HirBinaryOp::Eq
            | crate::HirBinaryOp::Ne
            | crate::HirBinaryOp::Lt
            | crate::HirBinaryOp::Le
            | crate::HirBinaryOp::Gt
            | crate::HirBinaryOp::Ge
            | crate::HirBinaryOp::And
            | crate::HirBinaryOp::Or => Type::Bool,
        },
        HirExpr::Call { callee, .. } => program
            .functions
            .get(*callee)
            .ok_or_else(|| TszError::Codegen {
                message: "callee HIR out of bounds (internal error)".to_string(),
            })?
            .return_type,
    })
}
