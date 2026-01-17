use crate::{HirExpr, HirProgram, TszError, Type};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::{self};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{FuncId, Module as _};
use cranelift_object::ObjectModule;

pub(super) fn codegen_expr(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    param_vars: &[Variable],
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
        HirExpr::Param { param, .. } => codegen_param_value(builder, param_vars, *param),
        HirExpr::Local { local, .. } => codegen_local_value(builder, local_vars, *local),
        HirExpr::UnaryMinus { expr, .. } => codegen_unary_minus(
            builder,
            object_module,
            program,
            func_ids,
            func,
            param_vars,
            local_vars,
            ty,
            expr,
        ),
        HirExpr::Binary { op, left, right, .. } => codegen_binary(
            builder,
            object_module,
            program,
            func_ids,
            func,
            param_vars,
            local_vars,
            ty,
            *op,
            left,
            right,
        ),
        HirExpr::Call { callee, args, .. } => codegen_call(
            builder,
            object_module,
            program,
            func_ids,
            func,
            param_vars,
            local_vars,
            ty,
            *callee,
            args,
        ),
    }
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

fn codegen_unary_minus(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    ty: Type,
    expr: &HirExpr,
) -> Result<ir::Value, TszError> {
    let v = codegen_expr(builder, object_module, program, func_ids, func, param_vars, local_vars, expr)?;
    match ty {
        Type::BigInt => Ok(builder.ins().ineg(v)),
        Type::Number => Ok(builder.ins().fneg(v)),
        Type::Void | Type::Bool | Type::String => Err(TszError::Codegen {
            message: "Invalid unary minus type (should have been blocked by typecheck)".to_string(),
        }),
    }
}

fn codegen_binary(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    ty: Type,
    op: crate::HirBinaryOp,
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    let lhs = codegen_expr(builder, object_module, program, func_ids, func, param_vars, local_vars, left)?;
    let rhs = codegen_expr(builder, object_module, program, func_ids, func, param_vars, local_vars, right)?;
    match ty {
        Type::Number => Ok(match op {
            crate::HirBinaryOp::Add => builder.ins().fadd(lhs, rhs),
            crate::HirBinaryOp::Sub => builder.ins().fsub(lhs, rhs),
            crate::HirBinaryOp::Mul => builder.ins().fmul(lhs, rhs),
            crate::HirBinaryOp::Div => builder.ins().fdiv(lhs, rhs),
        }),
        Type::BigInt => Ok(match op {
            crate::HirBinaryOp::Add => builder.ins().iadd(lhs, rhs),
            crate::HirBinaryOp::Sub => builder.ins().isub(lhs, rhs),
            crate::HirBinaryOp::Mul => builder.ins().imul(lhs, rhs),
            crate::HirBinaryOp::Div => builder.ins().sdiv(lhs, rhs),
        }),
        Type::Void | Type::Bool | Type::String => Err(TszError::Codegen {
            message: "Invalid binary operator type (should have been blocked by typecheck)".to_string(),
        }),
    }
}

fn codegen_call(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
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
        HirExpr::Binary { left, .. } => hir_expr_type(program, func, left)?,
        HirExpr::Call { callee, .. } => program
            .functions
            .get(*callee)
            .ok_or_else(|| TszError::Codegen {
                message: "callee HIR out of bounds (internal error)".to_string(),
            })?
            .return_type,
    })
}
