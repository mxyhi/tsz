use clap::{Parser, Subcommand, ValueEnum};
use std::path::{Path, PathBuf};
use tsz_compiler::{BuildOptions, OptLevel};

#[derive(Debug, Parser)]
#[command(name = "tsz", version, about = "TSZ compiler (high-performance AOT TS subset)")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// 编译入口文件为原生可执行文件
    Build {
        /// 入口 TSZ 文件路径
        entry: PathBuf,
        /// 输出可执行文件路径（默认：./a.out 或 ./a.exe）
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// 优化等级（默认 speed）
        #[arg(long, value_enum, default_value_t = OptArg::Speed)]
        opt: OptArg,
    },
    /// 编译并运行入口文件（返回码由 main() 决定）
    Run {
        /// 入口 TSZ 文件路径
        entry: PathBuf,
        /// 优化等级（默认 speed）
        #[arg(long, value_enum, default_value_t = OptArg::Speed)]
        opt: OptArg,
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
        Command::Build { entry, output, opt } => {
            let output = output.unwrap_or_else(default_output_path);
            build(entry, output, opt.into()).await?;
            Ok(0)
        }
        Command::Run { entry, opt } => {
            let dir = tempfile::tempdir().map_err(|e| tsz_compiler::TszError::Io {
                path: PathBuf::from("<tempdir>"),
                source: e,
            })?;
            let output = dir.path().join(default_binary_name());
            build(entry, output.clone(), opt.into()).await?;

            let status = tokio::process::Command::new(&output)
                .status()
                .await
                .map_err(|e| tsz_compiler::TszError::Io {
                    path: output.clone(),
                    source: e,
                })?;
            Ok(status.code().unwrap_or(1))
        }
    }
}

async fn build(entry: PathBuf, output: PathBuf, opt_level: OptLevel) -> Result<(), tsz_compiler::TszError> {
    if let Some(parent) = output.parent() {
        ensure_dir(parent).await?;
    }

    tsz_compiler::build_executable(BuildOptions {
        entry,
        output,
        opt_level,
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

fn default_output_path(_entry: PathBuf) -> PathBuf {
    PathBuf::from(default_binary_name())
}
