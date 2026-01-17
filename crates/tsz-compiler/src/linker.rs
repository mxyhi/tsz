use crate::TszError;
use std::path::{Path, PathBuf};
use tokio::process::Command;

pub async fn link_executable(object_path: &Path, output_exe: &Path) -> Result<(), TszError> {
    let linker = std::env::var_os("CC")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(default_cc()));

    let mut cmd = Command::new(&linker);
    cmd.arg(object_path).arg("-o").arg(output_exe);

    // 尽量保持链接过程可诊断；失败时把 stderr 带出来
    let out = cmd.output().await.map_err(|e| TszError::Link {
        message: format!("启动链接器失败: {linker:?}: {e}"),
    })?;

    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        let stdout = String::from_utf8_lossy(&out.stdout);
        return Err(TszError::Link {
            message: format!(
                "链接器返回失败码: {status}\nstdout:\n{stdout}\nstderr:\n{stderr}",
                status = out.status
            ),
        });
    }

    Ok(())
}

fn default_cc() -> &'static str {
    if cfg!(windows) {
        "clang"
    } else {
        "cc"
    }
}

