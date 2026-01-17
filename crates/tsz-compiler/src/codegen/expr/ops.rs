use crate::{HirExpr, HirProgram, TszError, Type};
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::{self};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;

use super::{codegen_expr, hir_expr_type};
use super::super::string_pool::StringPool;

pub(super) fn codegen_unary_minus(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    ty: Type,
    expr: &HirExpr,
) -> Result<ir::Value, TszError> {
    let v = codegen_expr(
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
    match ty {
        Type::BigInt => Ok(builder.ins().ineg(v)),
        Type::Number => Ok(builder.ins().fneg(v)),
        Type::Void | Type::Bool | Type::String => Err(TszError::Codegen {
            message: "Invalid unary minus type (should have been blocked by typecheck)".to_string(),
        }),
    }
}

pub(super) fn codegen_unary_not(
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
    let v = codegen_expr(
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
    let zero = builder.ins().iconst(ir::types::I8, 0);
    let is_false = builder.ins().icmp(IntCC::Equal, v, zero);
    Ok(bool_to_i8(builder, is_false))
}

fn bool_to_i8(builder: &mut FunctionBuilder<'_>, cond: ir::Value) -> ir::Value {
    let one = builder.ins().iconst(ir::types::I8, 1);
    let zero = builder.ins().iconst(ir::types::I8, 0);
    builder.ins().select(cond, one, zero)
}

fn normalize_bool(builder: &mut FunctionBuilder<'_>, value: ir::Value) -> ir::Value {
    let zero = builder.ins().iconst(ir::types::I8, 0);
    let is_true = builder.ins().icmp(IntCC::NotEqual, value, zero);
    bool_to_i8(builder, is_true)
}

pub(super) fn codegen_binary(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    op: crate::HirBinaryOp,
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    let ty = hir_expr_type(program, func, left)?;
    match op {
        crate::HirBinaryOp::Add
        | crate::HirBinaryOp::Sub
        | crate::HirBinaryOp::Mul
        | crate::HirBinaryOp::Div => codegen_arithmetic_binary(
            builder,
            object_module,
            program,
            func_ids,
            string_pool,
            func,
            param_vars,
            local_vars,
            ty,
            op,
            left,
            right,
        ),
        crate::HirBinaryOp::Eq
        | crate::HirBinaryOp::Ne
        | crate::HirBinaryOp::Lt
        | crate::HirBinaryOp::Le
        | crate::HirBinaryOp::Gt
        | crate::HirBinaryOp::Ge => codegen_compare_binary(
            builder,
            object_module,
            program,
            func_ids,
            string_pool,
            func,
            param_vars,
            local_vars,
            ty,
            op,
            left,
            right,
        ),
        crate::HirBinaryOp::And | crate::HirBinaryOp::Or => Err(TszError::Codegen {
            message: "Logical operators should be lowered with short-circuit codegen".to_string(),
        }),
    }
}

fn codegen_arithmetic_binary(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    ty: Type,
    op: crate::HirBinaryOp,
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    let lhs = codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        left,
    )?;
    let rhs = codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        right,
    )?;
    match ty {
        Type::Number => Ok(match op {
            crate::HirBinaryOp::Add => builder.ins().fadd(lhs, rhs),
            crate::HirBinaryOp::Sub => builder.ins().fsub(lhs, rhs),
            crate::HirBinaryOp::Mul => builder.ins().fmul(lhs, rhs),
            crate::HirBinaryOp::Div => builder.ins().fdiv(lhs, rhs),
            _ => unreachable!("checked arithmetic op"),
        }),
        Type::BigInt => Ok(match op {
            crate::HirBinaryOp::Add => builder.ins().iadd(lhs, rhs),
            crate::HirBinaryOp::Sub => builder.ins().isub(lhs, rhs),
            crate::HirBinaryOp::Mul => builder.ins().imul(lhs, rhs),
            crate::HirBinaryOp::Div => builder.ins().sdiv(lhs, rhs),
            _ => unreachable!("checked arithmetic op"),
        }),
        Type::Void | Type::Bool | Type::String => Err(TszError::Codegen {
            message: "Invalid arithmetic operator type (should have been blocked by typecheck)".to_string(),
        }),
    }
}

fn codegen_compare_binary(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    ty: Type,
    op: crate::HirBinaryOp,
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    let cond = codegen_compare_cond(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        ty,
        op,
        left,
        right,
    )?;
    Ok(bool_to_i8(builder, cond))
}

fn codegen_compare_cond(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    ty: Type,
    op: crate::HirBinaryOp,
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    let lhs = codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        left,
    )?;
    let rhs = codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        right,
    )?;

    match ty {
        Type::Number => Ok(builder.ins().fcmp(float_cc_for(op), lhs, rhs)),
        Type::BigInt | Type::Bool => Ok(builder.ins().icmp(int_cc_for(op), lhs, rhs)),
        Type::Void | Type::String => Err(TszError::Codegen {
            message: "Invalid comparison type (should have been blocked by typecheck)".to_string(),
        }),
    }
}

fn float_cc_for(op: crate::HirBinaryOp) -> FloatCC {
    match op {
        crate::HirBinaryOp::Eq => FloatCC::Equal,
        crate::HirBinaryOp::Ne => FloatCC::NotEqual,
        crate::HirBinaryOp::Lt => FloatCC::LessThan,
        crate::HirBinaryOp::Le => FloatCC::LessThanOrEqual,
        crate::HirBinaryOp::Gt => FloatCC::GreaterThan,
        crate::HirBinaryOp::Ge => FloatCC::GreaterThanOrEqual,
        _ => unreachable!("checked compare op"),
    }
}

fn int_cc_for(op: crate::HirBinaryOp) -> IntCC {
    match op {
        crate::HirBinaryOp::Eq => IntCC::Equal,
        crate::HirBinaryOp::Ne => IntCC::NotEqual,
        crate::HirBinaryOp::Lt => IntCC::SignedLessThan,
        crate::HirBinaryOp::Le => IntCC::SignedLessThanOrEqual,
        crate::HirBinaryOp::Gt => IntCC::SignedGreaterThan,
        crate::HirBinaryOp::Ge => IntCC::SignedGreaterThanOrEqual,
        _ => unreachable!("checked compare op"),
    }
}

pub(super) fn codegen_logical_and(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    codegen_logical_short_circuit(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        LogicalOp::And,
        left,
        right,
    )
}

pub(super) fn codegen_logical_or(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    codegen_logical_short_circuit(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        LogicalOp::Or,
        left,
        right,
    )
}

#[derive(Debug, Clone, Copy)]
enum LogicalOp {
    And,
    Or,
}

fn codegen_logical_short_circuit(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    op: LogicalOp,
    left: &HirExpr,
    right: &HirExpr,
) -> Result<ir::Value, TszError> {
    // Short-circuit: AND skips rhs on false, OR skips rhs on true.
    let lhs =
        codegen_expr(builder, object_module, program, func_ids, string_pool, func, param_vars, local_vars, left)?;
    let zero = builder.ins().iconst(ir::types::I8, 0);
    let one = builder.ins().iconst(ir::types::I8, 1);
    let is_true = builder.ins().icmp(IntCC::NotEqual, lhs, zero);

    let fast_block = builder.create_block();
    let rhs_block = builder.create_block();
    let join_block = builder.create_block();
    let join_value = builder.append_block_param(join_block, ir::types::I8);

    match op {
        LogicalOp::And => builder.ins().brif(is_true, rhs_block, &[], fast_block, &[]),
        LogicalOp::Or => builder.ins().brif(is_true, fast_block, &[], rhs_block, &[]),
    }
    builder.seal_block(fast_block);
    builder.seal_block(rhs_block);

    builder.switch_to_block(rhs_block);
    let rhs_val =
        codegen_expr(builder, object_module, program, func_ids, string_pool, func, param_vars, local_vars, right)?;
    let rhs_bool = normalize_bool(builder, rhs_val);
    builder.ins().jump(join_block, &[rhs_bool]);

    builder.switch_to_block(fast_block);
    let fast_value = match op {
        LogicalOp::And => zero,
        LogicalOp::Or => one,
    };
    builder.ins().jump(join_block, &[fast_value]);

    builder.seal_block(join_block);
    builder.switch_to_block(join_block);
    Ok(join_value)
}
