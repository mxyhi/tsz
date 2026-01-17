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

/// 只做“解析 + 类型检查”，不生成/不链接任何产物。
pub async fn check(entry: &Path) -> Result<(), TszError> {
    let program = resolver::load_program(entry).await?;
    // 当前类型检查会生成用于 codegen 的 HIR；check 只关心是否能通过语义/类型约束。
    let _hir = typecheck::analyze(&program)?;
    Ok(())
}

/// Build output: currently only native executables (AOT) are supported.
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
