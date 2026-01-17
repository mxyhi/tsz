use crate::TszError;
use std::path::{Path, PathBuf};
use tokio::process::Command;

const TSZ_RUNTIME_C: &str = include_str!("../runtime/tsz_runtime.c");

pub async fn link_executable(object_path: &Path, output_exe: &Path) -> Result<(), TszError> {
    let linker = std::env::var_os("CC")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(default_cc()));

    let runtime_obj = compile_runtime_object(object_path, &linker).await?;

    let mut cmd = Command::new(&linker);
    cmd.arg(object_path).arg(&runtime_obj).arg("-o").arg(output_exe);

    // Keep the linking process diagnosable; include stderr on failure.
    let out = cmd.output().await.map_err(|e| TszError::Link {
        message: format!("Failed to spawn linker: {linker:?}: {e}"),
    })?;

    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        let stdout = String::from_utf8_lossy(&out.stdout);
        return Err(TszError::Link {
            message: format!(
                "Linker exited with non-zero status: {status}\nstdout:\n{stdout}\nstderr:\n{stderr}",
                status = out.status
            ),
        });
    }

    Ok(())
}

async fn compile_runtime_object(object_path: &Path, cc: &Path) -> Result<PathBuf, TszError> {
    let out_dir = object_path.parent().unwrap_or_else(|| Path::new("."));
    let c_path = out_dir.join("__tsz_runtime.c");

    let mut obj_path = out_dir.join("__tsz_runtime");
    if cfg!(windows) {
        obj_path.set_extension("obj");
    } else {
        obj_path.set_extension("o");
    }

    tokio::fs::write(&c_path, TSZ_RUNTIME_C.as_bytes())
        .await
        .map_err(|e| TszError::Io {
            path: c_path.clone(),
            source: e,
        })?;

    let mut cmd = Command::new(cc);
    cmd.arg("-std=c99").arg("-O2").arg("-c").arg(&c_path).arg("-o").arg(&obj_path);

    let out = cmd.output().await.map_err(|e| TszError::Runtime {
        message: format!("Failed to spawn C compiler: {cc:?}: {e}"),
    })?;

    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        let stdout = String::from_utf8_lossy(&out.stdout);
        return Err(TszError::Runtime {
            message: format!(
                "Failed to compile TSZ runtime: {status}\nstdout:\n{stdout}\nstderr:\n{stderr}",
                status = out.status
            ),
        });
    }

    // Keep only the object file; avoid leaving a .c file in the output directory.
    let _ = tokio::fs::remove_file(&c_path).await;

    Ok(obj_path)
}

fn default_cc() -> &'static str {
    if cfg!(windows) {
        "clang"
    } else {
        "cc"
    }
}
