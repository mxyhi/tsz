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

async fn build_and_run(entry: PathBuf, expected_exit: i32) -> Result<(), TszError> {
    let dir = tempfile::tempdir().map_err(|e| TszError::Io {
        path: PathBuf::from("<tempdir>"),
        source: e,
    })?;
    let output = dir.path().join(exe_name("tsz_test_out"));

    build_executable(BuildOptions {
        entry,
        output: output.clone(),
        opt_level: OptLevel::None,
    })
    .await?;

    let status = tokio::process::Command::new(&output)
        .status()
        .await
        .map_err(|e| TszError::Io {
            path: output.clone(),
            source: e,
        })?;

    assert_eq!(status.code().unwrap_or(1), expected_exit);
    Ok(())
}

#[test]
fn build_and_run_single_file_bigint() {
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
  return 7n;
}
"#,
        )
        .expect("write");

        build_and_run(entry, 7).await
    })
    .expect("ok");
}

#[test]
fn build_and_run_void_main() {
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
export function main(): void {
  return;
}
"#,
        )
        .expect("write");

        build_and_run(entry, 0).await
    })
    .expect("ok");
}

#[test]
fn build_and_run_package_entry_with_relative_import() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .expect("tokio runtime");

    rt.block_on(async {
        let dir = tempfile::tempdir().expect("tempdir");
        let pkg = dir.path().join("pkg");
        write_file(
            &pkg.join("package.json"),
            r#"{ "name": "pkg", "version": "0.0.0", "tsz": { "entry": "src/main.ts" } }"#,
        )
        .expect("write");
        write_file(
            &pkg.join("src/lib.ts"),
            r#"
export function fortyTwo(): bigint {
  return 42n;
}
"#,
        )
        .expect("write");
        write_file(
            &pkg.join("src/main.ts"),
            r#"
import { fortyTwo } from "./lib.ts";
export function main(): bigint {
  return fortyTwo();
}
"#,
        )
        .expect("write");

        build_and_run(pkg, 42).await
    })
    .expect("ok");
}

#[test]
fn build_and_run_node_modules_package_import() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .expect("tokio runtime");

    rt.block_on(async {
        let dir = tempfile::tempdir().expect("tempdir");
        let app = dir.path().join("app");
        write_file(
            &app.join("package.json"),
            r#"{ "name": "app", "version": "0.0.0", "tsz": { "entry": "src/main.ts" } }"#,
        )
        .expect("write");
        write_file(
            &app.join("src/main.ts"),
            r#"
import { fortyTwo } from "dep";
export function main(): bigint {
  return fortyTwo();
}
"#,
        )
        .expect("write");

        // node_modules/dep
        let dep = app.join("node_modules/dep");
        write_file(
            &dep.join("package.json"),
            r#"{ "name": "dep", "version": "0.0.0", "tsz": { "entry": "src/dep.ts" } }"#,
        )
        .expect("write");
        write_file(
            &dep.join("src/dep.ts"),
            r#"
export function fortyTwo(): bigint {
  return 42n;
}
"#,
        )
        .expect("write");

        build_and_run(app, 42).await
    })
    .expect("ok");
}

#[test]
fn build_and_run_node_modules_package_subpath_import() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .expect("tokio runtime");

    rt.block_on(async {
        let dir = tempfile::tempdir().expect("tempdir");
        let app = dir.path().join("app");
        write_file(
            &app.join("package.json"),
            r#"{ "name": "app", "version": "0.0.0", "tsz": { "entry": "src/main.ts" } }"#,
        )
        .expect("write");
        write_file(
            &app.join("src/main.ts"),
            r#"
import { fortyTwo } from "dep/subpath";
export function main(): bigint {
  return fortyTwo();
}
"#,
        )
        .expect("write");

        // node_modules/dep
        let dep = app.join("node_modules/dep");
        write_file(
            &dep.join("package.json"),
            r#"{ "name": "dep", "version": "0.0.0", "tsz": { "entry": "src/dep.ts" } }"#,
        )
        .expect("write");
        write_file(
            &dep.join("src/dep.ts"),
            r#"
export function unused(): bigint {
  return 0n;
}
"#,
        )
        .expect("write");
        write_file(
            &dep.join("subpath.ts"),
            r#"
export function fortyTwo(): bigint {
  return 42n;
}
"#,
        )
        .expect("write");

        build_and_run(app, 42).await
    })
    .expect("ok");
}

#[test]
fn build_and_run_node_modules_scoped_package_subpath_import() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .expect("tokio runtime");

    rt.block_on(async {
        let dir = tempfile::tempdir().expect("tempdir");
        let app = dir.path().join("app");
        write_file(
            &app.join("package.json"),
            r#"{ "name": "app", "version": "0.0.0", "tsz": { "entry": "src/main.ts" } }"#,
        )
        .expect("write");
        write_file(
            &app.join("src/main.ts"),
            r#"
import { fortyTwo } from "@scope/dep/subpath";
export function main(): bigint {
  return fortyTwo();
}
"#,
        )
        .expect("write");

        // node_modules/@scope/dep
        let dep = app.join("node_modules/@scope/dep");
        write_file(
            &dep.join("package.json"),
            r#"{ "name": "@scope/dep", "version": "0.0.0", "tsz": { "entry": "src/dep.ts" } }"#,
        )
        .expect("write");
        write_file(
            &dep.join("src/dep.ts"),
            r#"
export function unused(): bigint {
  return 0n;
}
"#,
        )
        .expect("write");
        write_file(
            &dep.join("subpath.ts"),
            r#"
export function fortyTwo(): bigint {
  return 42n;
}
"#,
        )
        .expect("write");

        build_and_run(app, 42).await
    })
    .expect("ok");
}

#[test]
fn build_and_run_console_log_stdout() {
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
  console.log("hi", 1, 2n);
  console.log();
  return 0n;
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

        assert_eq!(out.status.code().unwrap_or(1), 0);
        assert_eq!(String::from_utf8_lossy(&out.stdout), "hi 1 2\n\n");
        Ok::<(), TszError>(())
    })
    .expect("ok");
}

#[test]
fn build_and_run_let_locals_stdout() {
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
  let x: bigint = 7n;
  console.log(x);
  let y = -x;
  console.log(y);
  return x;
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

        assert_eq!(out.status.code().unwrap_or(1), 7);
        assert_eq!(String::from_utf8_lossy(&out.stdout), "7\n-7\n");
        Ok::<(), TszError>(())
    })
    .expect("ok");
}

#[test]
fn build_and_run_const_fold_and_inline() {
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
  const x: bigint = 7n;
  console.log(x);
  const y = -x;
  console.log(y);
  return x;
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

        assert_eq!(out.status.code().unwrap_or(1), 7);
        assert_eq!(String::from_utf8_lossy(&out.stdout), "7\n-7\n");
        Ok::<(), TszError>(())
    })
    .expect("ok");
}

#[test]
fn build_and_run_function_params_and_binary_ops() {
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
function add(a: bigint, b: bigint): bigint {
  return a + b;
}

export function main(): bigint {
  return (add(20n, 1n) + 0n) * 2n;
}
"#,
        )
        .expect("write");

        build_and_run(entry, 42).await
    })
    .expect("ok");
}

#[test]
fn type_error_const_initializer_must_be_const() {
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
function fortyTwo(): bigint {
  return 42n;
}

export function main(): bigint {
  const x: bigint = fortyTwo();
  return x;
}
"#,
        )
        .expect("write");

        let out_dir = tempfile::tempdir().map_err(|e| TszError::Io {
            path: PathBuf::from("<tempdir>"),
            source: e,
        })?;
        let output = out_dir.path().join(exe_name("tsz_test_out"));

        let err = build_executable(BuildOptions {
            entry,
            output,
            opt_level: OptLevel::None,
        })
        .await
        .expect_err("should fail");

        match err {
            TszError::Type { message, .. } => {
                assert!(
                    message.contains("const initializer must be a compile-time constant"),
                    "unexpected error message: {message}"
                );
            }
            other => panic!("expected type error, got: {other:?}"),
        }
        Ok::<(), TszError>(())
    })
    .expect("ok");
}
