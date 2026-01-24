use std::path::{Path, PathBuf};
use tsz_compiler::{BuildOptions, CompileOutput, EmitKind, OptLevel, TszError, build_executable};

const MAX_ERRORS: usize = 50;

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

fn assert_no_errors(output: &CompileOutput) {
    assert!(
        !output.diagnostics.has_errors(),
        "expected no diagnostics errors"
    );
}

#[test]
fn build_and_run_syntax_polish_end_to_end() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .expect("tokio runtime");

    rt.block_on(async {
        let dir = tempfile::tempdir().expect("tempdir");
        let entry = dir.path().join("main.ts");

        write_file(
            &dir.path().join("lib.ts"),
            r#"
export function fortyTwo(): bigint {
  return 42n;
}
"#,
        )
        .expect("write");

        write_file(
            &entry,
            r#"
import { fortyTwo, } from "./lib.ts";

function id(x: bigint,): bigint {
  return x;
}

export function main(): bigint {
  console.log("a\nb\tc\\d\"e\'f\u0041", 1e3,);
  return id(fortyTwo(),);
}
"#,
        )
        .expect("write");

        let out_dir = tempfile::tempdir().map_err(|e| TszError::Io {
            path: PathBuf::from("<tempdir>"),
            source: e,
        })?;
        let output = out_dir.path().join(exe_name("tsz_test_out"));

        let compile = build_executable(BuildOptions {
            entry,
            output: output.clone(),
            opt_level: OptLevel::None,
            max_errors: MAX_ERRORS,
            emit: EmitKind::Exe,
        })
        .await?;
        assert_no_errors(&compile);

        let out = tokio::process::Command::new(&output)
            .output()
            .await
            .map_err(|e| TszError::Io {
                path: output.clone(),
                source: e,
            })?;

        assert_eq!(out.status.code().unwrap_or(1), 42);
        assert_eq!(
            String::from_utf8_lossy(&out.stdout),
            "a\nb\tc\\d\"e'fA 1000\n"
        );
        Ok::<(), TszError>(())
    })
    .expect("ok");
}
