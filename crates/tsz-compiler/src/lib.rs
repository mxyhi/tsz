mod ast;
mod codegen;
mod error;
mod hir;
mod lexer;
mod linker;
mod parser;
mod resolver;
mod span;
mod typecheck;

pub use ast::*;
pub use error::*;
pub use hir::*;
pub use span::*;

use std::path::{Path, PathBuf};

/// 编译产物：目前只支持生成原生可执行文件（AOT）。
#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub entry: PathBuf,
    pub output: PathBuf,
    pub opt_level: OptLevel,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    None,
    Speed,
}

pub async fn build_executable(options: BuildOptions) -> Result<(), TszError> {
    let program = resolver::load_program(&options.entry).await?;
    let hir = typecheck::analyze(&program)?;
    let object_bytes = codegen::emit_object(&hir, options.opt_level)?;

    let object_path = default_object_path(&options.output);
    tokio::fs::write(&object_path, object_bytes)
        .await
        .map_err(|e| TszError::Io {
            path: object_path.clone(),
            source: e,
        })?;

    linker::link_executable(&object_path, &options.output).await?;
    Ok(())
}

fn default_object_path(output_exe: &Path) -> PathBuf {
    let mut p = output_exe.to_path_buf();
    if cfg!(windows) {
        p.set_extension("obj");
    } else {
        p.set_extension("o");
    }
    p
}
