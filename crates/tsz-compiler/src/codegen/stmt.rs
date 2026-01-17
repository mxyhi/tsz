use crate::{HirExpr, HirProgram, HirStmt, TszError, Type};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{self, InstBuilder};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;

use super::expr;
use super::runtime::{call_runtime0, call_runtime1, call_runtime2, RuntimeFuncs};
use super::string_pool::StringPool;

#[derive(Debug, Clone, Copy)]
struct LoopCtx {
    header_block: ir::Block,
    after_block: ir::Block,
}

pub(super) struct StmtGenState {
    pub(super) cur_block: ir::Block,
    loop_stack: Vec<LoopCtx>,
}

impl StmtGenState {
    pub(super) fn new(entry_block: ir::Block) -> Self {
        Self {
            cur_block: entry_block,
            loop_stack: Vec::new(),
        }
    }
}

pub(super) fn codegen_stmt_list(
    state: &mut StmtGenState,
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    stmts: &[HirStmt],
) -> Result<bool, TszError> {
    let mut can_continue = true;
    for stmt in stmts {
        if !can_continue {
            break;
        }
        can_continue = codegen_stmt(
            state,
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
    Ok(can_continue)
}

fn codegen_stmt(
    state: &mut StmtGenState,
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
) -> Result<bool, TszError> {
    match stmt {
        HirStmt::Let { local, init, .. } => {
            let var = local_vars.get(*local).copied().ok_or_else(|| TszError::Codegen {
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
            Ok(true)
        }
        HirStmt::Assign { local, value, .. } => {
            let var = local_vars.get(*local).copied().ok_or_else(|| TszError::Codegen {
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
                value,
            )?;
            builder.def_var(var, v);
            Ok(true)
        }
        HirStmt::ConsoleLog { args, .. } => {
            codegen_console_log_stmt(
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
            )?;
            Ok(true)
        }
        HirStmt::Return { expr, .. } => {
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
            )?;
            Ok(false)
        }
        HirStmt::Break { .. } => {
            let Some(loop_ctx) = state.loop_stack.last().copied() else {
                return Err(TszError::Codegen {
                    message: "break used outside of while (should have been blocked by typecheck)".to_string(),
                });
            };
            builder.ins().jump(loop_ctx.after_block, &[]);
            Ok(false)
        }
        HirStmt::Continue { .. } => {
            let Some(loop_ctx) = state.loop_stack.last().copied() else {
                return Err(TszError::Codegen {
                    message: "continue used outside of while (should have been blocked by typecheck)".to_string(),
                });
            };
            builder.ins().jump(loop_ctx.header_block, &[]);
            Ok(false)
        }
        HirStmt::If {
            cond,
            then_body,
            else_body,
            ..
        } => codegen_if_stmt(
            state,
            builder,
            object_module,
            program,
            func_ids,
            runtime,
            string_pool,
            func,
            param_vars,
            local_vars,
            cond,
            then_body,
            else_body.as_deref(),
        ),
        HirStmt::While { cond, body, .. } => codegen_while_stmt(
            state,
            builder,
            object_module,
            program,
            func_ids,
            runtime,
            string_pool,
            func,
            param_vars,
            local_vars,
            cond,
            body,
        ),
    }
}

fn codegen_if_stmt(
    state: &mut StmtGenState,
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    cond: &HirExpr,
    then_body: &[HirStmt],
    else_body: Option<&[HirStmt]>,
) -> Result<bool, TszError> {
    let then_block = builder.create_block();
    let else_block = builder.create_block();
    let join_block = builder.create_block();

    let cond_val = expr::codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        cond,
    )?;
    let zero = builder.ins().iconst(ir::types::I8, 0);
    let is_true = builder.ins().icmp(IntCC::NotEqual, cond_val, zero);
    builder
        .ins()
        .brif(is_true, then_block, &[], else_block, &[]);

    // `then_block` / `else_block` have a single predecessor from the current block.
    builder.seal_block(then_block);
    builder.seal_block(else_block);

    // then: ...
    builder.switch_to_block(then_block);
    state.cur_block = then_block;
    let then_fallthrough = codegen_stmt_list(
        state,
        builder,
        object_module,
        program,
        func_ids,
        runtime,
        string_pool,
        func,
        param_vars,
        local_vars,
        then_body,
    )?;
    if then_fallthrough {
        builder.ins().jump(join_block, &[]);
    }

    // else: ...
    builder.switch_to_block(else_block);
    state.cur_block = else_block;
    let else_fallthrough = match else_body {
        None => {
            builder.ins().jump(join_block, &[]);
            true
        }
        Some(body) => {
            let fall = codegen_stmt_list(
                state,
                builder,
                object_module,
                program,
                func_ids,
                runtime,
                string_pool,
                func,
                param_vars,
                local_vars,
                body,
            )?;
            if fall {
                builder.ins().jump(join_block, &[]);
            }
            fall
        }
    };

    let join_reachable = then_fallthrough || else_fallthrough;
    builder.seal_block(join_block);

    if join_reachable {
        builder.switch_to_block(join_block);
        state.cur_block = join_block;
        Ok(true)
    } else {
        Ok(false)
    }
}

fn codegen_while_stmt(
    state: &mut StmtGenState,
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    program: &HirProgram,
    func_ids: &[FuncId],
    runtime: &RuntimeFuncs,
    string_pool: &mut StringPool,
    func: &crate::HirFunction,
    param_vars: &[Variable],
    local_vars: &[Variable],
    cond: &HirExpr,
    body: &[HirStmt],
) -> Result<bool, TszError> {
    let header_block = builder.create_block();
    let body_block = builder.create_block();
    let after_block = builder.create_block();

    // Jump to loop header first.
    builder.ins().jump(header_block, &[]);

    // header:
    builder.switch_to_block(header_block);
    state.cur_block = header_block;

    let cond_val = expr::codegen_expr(
        builder,
        object_module,
        program,
        func_ids,
        string_pool,
        func,
        param_vars,
        local_vars,
        cond,
    )?;
    let zero = builder.ins().iconst(ir::types::I8, 0);
    let is_true = builder.ins().icmp(IntCC::NotEqual, cond_val, zero);
    builder
        .ins()
        .brif(is_true, body_block, &[], after_block, &[]);

    // body has a single predecessor from header.
    builder.seal_block(body_block);

    // body:
    builder.switch_to_block(body_block);
    state.cur_block = body_block;

    state.loop_stack.push(LoopCtx {
        header_block,
        after_block,
    });
    let body_fallthrough = codegen_stmt_list(
        state,
        builder,
        object_module,
        program,
        func_ids,
        runtime,
        string_pool,
        func,
        param_vars,
        local_vars,
        body,
    )?;
    state.loop_stack.pop();

    if body_fallthrough {
        builder.ins().jump(header_block, &[]);
    }

    // By now, all `continue` jumps (to header) and `break` jumps (to after) have been inserted.
    builder.seal_block(header_block);
    builder.seal_block(after_block);

    // after:
    builder.switch_to_block(after_block);
    state.cur_block = after_block;
    Ok(true)
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
