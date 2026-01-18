use clap::{Parser, Subcommand, ValueEnum};
use std::path::{Path, PathBuf};
use std::time::Instant;
use tsz_compiler::{render_diagnostics, BuildOptions, CheckOptions, CompileOutput, EmitKind, OptLevel};

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
        /// Emit kind (default: exe)
        #[arg(long, value_enum, default_value_t = EmitArg::Exe)]
        emit: EmitArg,
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
    /// Benchmark compile time + runtime for one entry file
    Bench {
        /// Entry TSZ file path
        #[arg(default_value = "examples/control-flow.ts")]
        entry: PathBuf,
        /// Optimization level (default: speed)
        #[arg(long, value_enum, default_value_t = OptArg::Speed)]
        opt: OptArg,
        /// Warmup iterations for compile/run (default: 3)
        #[arg(long, default_value_t = 3)]
        warmup: usize,
        /// Compile iterations (default: 10)
        #[arg(long, default_value_t = 10)]
        compile_iters: usize,
        /// Run iterations (default: 30)
        #[arg(long, default_value_t = 30)]
        run_iters: usize,
        /// Optional compile budget in ms (mean)
        #[arg(long)]
        budget_compile_ms: Option<u64>,
        /// Optional run budget in ms (mean)
        #[arg(long)]
        budget_run_ms: Option<u64>,
        /// Max error count before aborting diagnostics collection
        #[arg(long, default_value_t = 50)]
        max_errors: usize,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum OptArg {
    None,
    Speed,
    Size,
}

impl From<OptArg> for OptLevel {
    fn from(v: OptArg) -> Self {
        match v {
            OptArg::None => OptLevel::None,
            OptArg::Speed => OptLevel::Speed,
            OptArg::Size => OptLevel::Size,
        }
    }
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum EmitArg {
    Exe,
    Obj,
    Ir,
}

impl From<EmitArg> for EmitKind {
    fn from(v: EmitArg) -> Self {
        match v {
            EmitArg::Exe => EmitKind::Exe,
            EmitArg::Obj => EmitKind::Obj,
            EmitArg::Ir => EmitKind::Ir,
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
            emit,
            max_errors,
        } => {
            let output = output.unwrap_or_else(|| default_output_path(emit));
            let output = build(entry, output, opt.into(), emit.into(), max_errors).await?;
            let has_errors = report_diagnostics(&output);
            Ok(if has_errors { 1 } else { 0 })
        }
        Command::Run { entry, opt, max_errors } => {
            let dir = tempfile::tempdir().map_err(|e| tsz_compiler::TszError::Io {
                path: PathBuf::from("<tempdir>"),
                source: e,
            })?;
            let output = dir.path().join(default_binary_name());
            let compile = build(entry, output.clone(), opt.into(), EmitKind::Exe, max_errors).await?;
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
        Command::Bench {
            entry,
            opt,
            warmup,
            compile_iters,
            run_iters,
            budget_compile_ms,
            budget_run_ms,
            max_errors,
        } => {
            let report = bench(entry, opt.into(), warmup, compile_iters, run_iters, max_errors).await?;
            let Some(report) = report else {
                return Ok(1);
            };
            let mut over_budget = false;
            if let Some(budget) = budget_compile_ms {
                if report.compile_mean_ms > budget as f64 {
                    over_budget = true;
                }
            }
            if let Some(budget) = budget_run_ms {
                if report.run_mean_ms > budget as f64 {
                    over_budget = true;
                }
            }

            println!("compile mean: {:.2} ms (n={})", report.compile_mean_ms, report.compile_samples);
            println!("run mean: {:.2} ms (n={})", report.run_mean_ms, report.run_samples);
            Ok(if over_budget { 1 } else { 0 })
        }
    }
}

async fn build(
    entry: PathBuf,
    output: PathBuf,
    opt_level: OptLevel,
    emit: EmitKind,
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
        emit,
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

fn default_output_path(emit: EmitArg) -> PathBuf {
    let mut path = PathBuf::from(default_binary_name());
    match emit {
        EmitArg::Exe => path,
        EmitArg::Obj => {
            if cfg!(windows) {
                path.set_extension("obj");
            } else {
                path.set_extension("o");
            }
            path
        }
        EmitArg::Ir => {
            path.set_extension("clif");
            path
        }
    }
}

fn report_diagnostics(output: &CompileOutput) -> bool {
    if output.diagnostics.items().is_empty() {
        return false;
    }
    let rendered = render_diagnostics(&output.diagnostics, &output.sources);
    eprintln!("{rendered}");
    output.diagnostics.has_errors()
}

struct BenchReport {
    compile_mean_ms: f64,
    run_mean_ms: f64,
    compile_samples: usize,
    run_samples: usize,
}

async fn bench(
    entry: PathBuf,
    opt_level: OptLevel,
    warmup: usize,
    compile_iters: usize,
    run_iters: usize,
    max_errors: usize,
) -> Result<Option<BenchReport>, tsz_compiler::TszError> {
    let compile_mean_ms = match bench_compile(&entry, opt_level, warmup, compile_iters, max_errors).await? {
        Some(value) => value,
        None => return Ok(None),
    };
    let run_mean_ms = match bench_run(&entry, opt_level, warmup, run_iters, max_errors).await? {
        Some(value) => value,
        None => return Ok(None),
    };
    Ok(Some(BenchReport {
        compile_mean_ms,
        run_mean_ms,
        compile_samples: compile_iters,
        run_samples: run_iters,
    }))
}

async fn bench_compile(
    entry: &Path,
    opt_level: OptLevel,
    warmup: usize,
    iters: usize,
    max_errors: usize,
) -> Result<Option<f64>, tsz_compiler::TszError> {
    let total_iters = warmup.saturating_add(iters);
    let mut times = Vec::with_capacity(iters);
    for idx in 0..total_iters {
        let dir = tempfile::tempdir().map_err(|e| tsz_compiler::TszError::Io {
            path: PathBuf::from("<tempdir>"),
            source: e,
        })?;
        let output = dir.path().join(default_binary_name());
        let start = Instant::now();
        let compile = build(entry.to_path_buf(), output, opt_level, EmitKind::Exe, max_errors).await?;
        if report_diagnostics(&compile) {
            return Ok(None);
        }
        if idx >= warmup {
            times.push(start.elapsed());
        }
    }
    Ok(Some(mean_ms(&times)))
}

async fn bench_run(
    entry: &Path,
    opt_level: OptLevel,
    warmup: usize,
    iters: usize,
    max_errors: usize,
) -> Result<Option<f64>, tsz_compiler::TszError> {
    let dir = tempfile::tempdir().map_err(|e| tsz_compiler::TszError::Io {
        path: PathBuf::from("<tempdir>"),
        source: e,
    })?;
    let output = dir.path().join(default_binary_name());
    let compile = build(entry.to_path_buf(), output.clone(), opt_level, EmitKind::Exe, max_errors).await?;
    if report_diagnostics(&compile) {
        return Ok(None);
    }

    let total_iters = warmup.saturating_add(iters);
    let mut times = Vec::with_capacity(iters);
    for idx in 0..total_iters {
        let start = Instant::now();
        let status = tokio::process::Command::new(&output)
            .status()
            .await
            .map_err(|e| tsz_compiler::TszError::Io {
                path: output.clone(),
                source: e,
            })?;
        if !status.success() {
            return Err(tsz_compiler::TszError::Runtime {
                message: format!("Benchmark run exited with {status}"),
            });
        }
        if idx >= warmup {
            times.push(start.elapsed());
        }
    }
    Ok(Some(mean_ms(&times)))
}

fn mean_ms(samples: &[std::time::Duration]) -> f64 {
    if samples.is_empty() {
        return 0.0;
    }
    let total: f64 = samples.iter().map(|d| d.as_secs_f64() * 1000.0).sum();
    total / samples.len() as f64
}
