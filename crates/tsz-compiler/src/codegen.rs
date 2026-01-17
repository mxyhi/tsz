use crate::{HirProgram, OptLevel, TszError};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::{isa};
use cranelift_module::{Linkage, Module as _};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::sync::Arc;

mod expr;
mod runtime;
mod stmt;
mod string_pool;
mod user_fn;
mod wrapper;

pub fn emit_object(program: &HirProgram, opt_level: OptLevel) -> Result<Vec<u8>, TszError> {
    let isa = build_isa(opt_level)?;
    let mut object_module = build_object_module(isa)?;

    // Declare all TSZ functions first so later `call` sites can resolve `FuncId`.
    let mut func_sigs = Vec::with_capacity(program.functions.len());
    let mut func_ids = Vec::with_capacity(program.functions.len());
    for f in &program.functions {
        let sig = wrapper::user_fn_sig(&*object_module.isa(), &f.params, f.return_type)?;
        let id = object_module
            .declare_function(&f.symbol, Linkage::Local, &sig)
            .map_err(|e| TszError::Codegen {
                message: format!("declare_function failed: {}: {e}", f.symbol),
            })?;
        func_sigs.push(sig);
        func_ids.push(id);
    }

    // Wrapper entry: C ABI `int main()` (exported).
    let wrapper_sig = wrapper::wrapper_main_sig(&*object_module.isa());
    let wrapper_id = object_module
        .declare_function("main", Linkage::Export, &wrapper_sig)
        .map_err(|e| TszError::Codegen {
            message: format!("declare wrapper main failed: {e}"),
        })?;

    let runtime = runtime::declare_runtime_funcs(&mut object_module)?;
    let mut string_pool = string_pool::StringPool::new();

    // Define all TSZ functions.
    for (idx, f) in program.functions.iter().enumerate() {
        user_fn::define_user_fn(
            &mut object_module,
            func_ids[idx],
            &func_sigs[idx],
            f,
            program,
            &func_ids,
            &runtime,
            &mut string_pool,
        )?;
    }

    // Define wrapper main.
    wrapper::define_wrapper_main(&mut object_module, wrapper_id, &wrapper_sig, program, &func_ids)?;

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
            message: format!("Failed to set cranelift opt_level: {e}"),
        })?;
    // macOS/modern Linux default to PIE; the executable should be PIC to avoid text relocations.
    flag_builder
        .set("is_pic", "true")
        .map_err(|e| TszError::Codegen {
            message: format!("Failed to set cranelift is_pic: {e}"),
        })?;
    let flags = settings::Flags::new(flag_builder);

    let isa_builder = cranelift_native::builder().map_err(|e| TszError::Codegen {
        message: format!("Failed to get native ISA builder: {e}"),
    })?;
    isa_builder.finish(flags).map_err(|e| TszError::Codegen {
        message: format!("Failed to build ISA: {e}"),
    })
}

fn build_object_module(isa: Arc<dyn isa::TargetIsa>) -> Result<ObjectModule, TszError> {
    let builder = ObjectBuilder::new(
        isa,
        "tsz",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| TszError::Codegen {
        message: format!("Failed to create ObjectBuilder: {e}"),
    })?;
    Ok(ObjectModule::new(builder))
}

fn finalize_and_emit(object_module: ObjectModule) -> Result<Vec<u8>, TszError> {
    let product = object_module.finish();
    product.emit().map_err(|e| TszError::Codegen {
        message: format!("emit object failed: {e}"),
    })
}
