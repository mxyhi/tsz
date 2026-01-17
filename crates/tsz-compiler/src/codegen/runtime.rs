use crate::TszError;
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir;
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{FuncId, Linkage, Module as _};
use cranelift_object::ObjectModule;

#[derive(Debug, Clone)]
pub(super) struct RuntimeFuncs {
    pub(super) log_i64: FuncId,
    pub(super) log_f64: FuncId,
    pub(super) log_str: FuncId,
    pub(super) log_space: FuncId,
    pub(super) log_newline: FuncId,
}

pub(super) fn declare_runtime_funcs(object_module: &mut ObjectModule) -> Result<RuntimeFuncs, TszError> {
    let ptr_ty = object_module.isa().pointer_type();

    let log_i64 = declare_import_fn(object_module, "tsz_log_i64", &[ir::types::I64], &[])?;
    let log_f64 = declare_import_fn(object_module, "tsz_log_f64", &[ir::types::F64], &[])?;
    let log_str = declare_import_fn(object_module, "tsz_log_str", &[ptr_ty, ir::types::I64], &[])?;
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
            message: format!("declare runtime function failed: {name}: {e}"),
        })
}

pub(super) fn call_runtime0(builder: &mut FunctionBuilder<'_>, object_module: &mut ObjectModule, func: FuncId) {
    let callee = object_module.declare_func_in_func(func, builder.func);
    builder.ins().call(callee, &[]);
}

pub(super) fn call_runtime1(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    func: FuncId,
    a0: ir::Value,
) {
    let callee = object_module.declare_func_in_func(func, builder.func);
    builder.ins().call(callee, &[a0]);
}

pub(super) fn call_runtime2(
    builder: &mut FunctionBuilder<'_>,
    object_module: &mut ObjectModule,
    func: FuncId,
    a0: ir::Value,
    a1: ir::Value,
) {
    let callee = object_module.declare_func_in_func(func, builder.func);
    builder.ins().call(callee, &[a0, a1]);
}
