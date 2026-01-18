use clap::{Parser, Subcommand, ValueEnum};
use std::path::{Path, PathBuf};
use tsz_compiler::{render_diagnostics, BuildOptions, CheckOptions, CompileOutput, OptLevel};

#[derive(Debug, Parser)]
#[command(name = "tsz", version, about = "TSZ compiler (high-performance AOT TS subset)")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Compile the entry file into a native executable
    Build {
        /// Entry TSZ file path
        entry: PathBuf,
        /// Output executable path (default: ./a.out or ./a.exe)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Optimization level (default: speed)
        #[arg(long, value_enum, default_value_t = OptArg::Speed)]
        opt: OptArg,
        /// Max error count before aborting diagnostics collection
        #[arg(long, default_value_t = 50)]
        max_errors: usize,
    },
    /// Compile and run the entry file (exit code is returned by main())
    Run {
        /// Entry TSZ file path
        entry: PathBuf,
        /// Optimization level (default: speed)
        #[arg(long, value_enum, default_value_t = OptArg::Speed)]
        opt: OptArg,
        /// Max error count before aborting diagnostics collection
        #[arg(long, default_value_t = 50)]
        max_errors: usize,
    },
    /// Parse + typecheck only (no codegen/link)
    Check {
        /// Entry TSZ file path
        entry: PathBuf,
        /// Max error count before aborting diagnostics collection
        #[arg(long, default_value_t = 50)]
        max_errors: usize,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum OptArg {
    None,
    Speed,
}

impl From<OptArg> for OptLevel {
    fn from(v: OptArg) -> Self {
        match v {
            OptArg::None => OptLevel::None,
            OptArg::Speed => OptLevel::Speed,
        }
    }
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();
    let exit_code = match run_cli(cli).await {
        Ok(code) => code,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    };
    std::process::exit(exit_code);
}

async fn run_cli(cli: Cli) -> Result<i32, tsz_compiler::TszError> {
    match cli.command {
        Command::Build {
            entry,
            output,
            opt,
            max_errors,
        } => {
            let output = output.unwrap_or_else(default_output_path);
            let output = build(entry, output, opt.into(), max_errors).await?;
            let has_errors = report_diagnostics(&output);
            Ok(if has_errors { 1 } else { 0 })
        }
        Command::Run { entry, opt, max_errors } => {
            let dir = tempfile::tempdir().map_err(|e| tsz_compiler::TszError::Io {
                path: PathBuf::from("<tempdir>"),
                source: e,
            })?;
            let output = dir.path().join(default_binary_name());
            let compile = build(entry, output.clone(), opt.into(), max_errors).await?;
            let has_errors = report_diagnostics(&compile);
            if has_errors {
                return Ok(1);
            }

            let status = tokio::process::Command::new(&output)
                .status()
                .await
                .map_err(|e| tsz_compiler::TszError::Io {
                    path: output.clone(),
                    source: e,
                })?;
            Ok(status.code().unwrap_or(1))
        }
        Command::Check { entry, max_errors } => {
            let output = tsz_compiler::check(CheckOptions { entry, max_errors }).await?;
            let has_errors = report_diagnostics(&output);
            Ok(if has_errors { 1 } else { 0 })
        }
    }
}

async fn build(
    entry: PathBuf,
    output: PathBuf,
    opt_level: OptLevel,
    max_errors: usize,
) -> Result<CompileOutput, tsz_compiler::TszError> {
    if let Some(parent) = output.parent() {
        ensure_dir(parent).await?;
    }

    tsz_compiler::build_executable(BuildOptions {
        entry,
        output,
        opt_level,
        max_errors,
    })
    .await
}

async fn ensure_dir(path: &Path) -> Result<(), tsz_compiler::TszError> {
    tokio::fs::create_dir_all(path).await.map_err(|e| tsz_compiler::TszError::Io {
        path: path.to_path_buf(),
        source: e,
    })
}

fn default_binary_name() -> &'static str {
    if cfg!(windows) {
        "a.exe"
    } else {
        "a.out"
    }
}

fn default_output_path() -> PathBuf {
    PathBuf::from(default_binary_name())
}

fn report_diagnostics(output: &CompileOutput) -> bool {
    if output.diagnostics.items().is_empty() {
        return false;
    }
    let rendered = render_diagnostics(&output.diagnostics, &output.sources);
    eprintln!("{rendered}");
    output.diagnostics.has_errors()
}
