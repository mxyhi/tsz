mod ast;
mod codegen;
mod diagnostics;
mod error;
mod hir;
mod lexer;
mod linker;
mod parser;
mod resolver;
mod span;
mod typecheck;

pub use ast::*;
pub use diagnostics::*;
pub use error::*;
pub use hir::*;
pub use span::*;

use std::path::{Path, PathBuf};

pub struct CheckOptions {
    pub entry: PathBuf,
    pub max_errors: usize,
}

pub struct CompileOutput {
    pub diagnostics: Diagnostics,
    pub sources: SourceMap,
}

/// 只做“解析 + 类型检查”，不生成/不链接任何产物。
pub async fn check(options: CheckOptions) -> Result<CompileOutput, TszError> {
    let mut diags = Diagnostics::new(options.max_errors);
    let program = resolver::load_program(&options.entry, &mut diags).await?;
    let sources = program.sources.clone();
    let _hir = typecheck::analyze(&program, &mut diags)?;
    Ok(CompileOutput {
        diagnostics: diags,
        sources,
    })
}

/// Build output: currently only native executables (AOT) are supported.
#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub entry: PathBuf,
    pub output: PathBuf,
    pub opt_level: OptLevel,
    pub max_errors: usize,
    pub emit: EmitKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    None,
    Speed,
    Size,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmitKind {
    Exe,
    Obj,
    Ir,
}

pub async fn build_executable(options: BuildOptions) -> Result<CompileOutput, TszError> {
    let mut diags = Diagnostics::new(options.max_errors);
    let program = resolver::load_program(&options.entry, &mut diags).await?;
    let sources = program.sources.clone();
    let hir = typecheck::analyze(&program, &mut diags)?;
    if diags.has_errors() {
        return Ok(CompileOutput {
            diagnostics: diags,
            sources,
        });
    }

    match options.emit {
        EmitKind::Exe => {
            let object_bytes = codegen::emit_object(&hir, options.opt_level)?;
            let temp_dir = tempfile::tempdir().map_err(|e| TszError::Io {
                path: PathBuf::from("<tempdir>"),
                source: e,
            })?;
            let object_path = temp_object_path(&temp_dir, &options.output);
            tokio::fs::write(&object_path, object_bytes)
                .await
                .map_err(|e| TszError::Io {
                    path: object_path.clone(),
                    source: e,
                })?;
            linker::link_executable(&object_path, &options.output).await?;
        }
        EmitKind::Obj => {
            let object_bytes = codegen::emit_object(&hir, options.opt_level)?;
            tokio::fs::write(&options.output, object_bytes)
                .await
                .map_err(|e| TszError::Io {
                    path: options.output.clone(),
                    source: e,
                })?;
        }
        EmitKind::Ir => {
            let ir = codegen::emit_ir(&hir, options.opt_level)?;
            tokio::fs::write(&options.output, ir)
                .await
                .map_err(|e| TszError::Io {
                    path: options.output.clone(),
                    source: e,
                })?;
        }
    }
    Ok(CompileOutput {
        diagnostics: diags,
        sources,
    })
}

fn temp_object_path(temp_dir: &tempfile::TempDir, output_exe: &Path) -> PathBuf {
    let name = output_exe
        .file_name()
        .map(|s| s.to_os_string())
        .unwrap_or_else(|| "a.out".into());
    let mut p = PathBuf::from(name);
    if cfg!(windows) {
        p.set_extension("obj");
    } else {
        p.set_extension("o");
    }
    temp_dir.path().join(p)
}
