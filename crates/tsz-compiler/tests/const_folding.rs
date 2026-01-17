use std::path::{Path, PathBuf};
use tsz_compiler::{BuildOptions, OptLevel, TszError, build_executable};

fn exe_name(stem: &str) -> String {
    if cfg!(windows) {
        format!("{stem}.exe")
    } else {
        stem.to_string()
    }
}

fn write_file(path: &Path, content: &str) -> Result<(), std::io::Error> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, content)?;
    Ok(())
}

#[test]
fn build_and_run_const_fold_binary_ops() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .expect("tokio runtime");

    rt.block_on(async {
        let dir = tempfile::tempdir().expect("tempdir");
        let entry = dir.path().join("main.ts");
        write_file(
            &entry,
            r#"
export function main(): bigint {
  const a: bigint = 20n;
  const b: bigint = a + 1n;
  const c: bigint = (b * 2n - 0n) / 1n;
  console.log(c);
  return c;
}
"#,
        )
        .expect("write");

        let out_dir = tempfile::tempdir().map_err(|e| TszError::Io {
            path: PathBuf::from("<tempdir>"),
            source: e,
        })?;
        let output = out_dir.path().join(exe_name("tsz_test_out"));

        build_executable(BuildOptions {
            entry,
            output: output.clone(),
            opt_level: OptLevel::None,
        })
        .await?;

        let out = tokio::process::Command::new(&output)
            .output()
            .await
            .map_err(|e| TszError::Io {
                path: output.clone(),
                source: e,
            })?;

        assert_eq!(out.status.code().unwrap_or(1), 42);
        assert_eq!(String::from_utf8_lossy(&out.stdout), "42\n");
        Ok::<(), TszError>(())
    })
    .expect("ok");
}

