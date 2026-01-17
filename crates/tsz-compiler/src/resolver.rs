use crate::{parser, Program, TszError};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

const DEFAULT_SOURCE_EXT: &str = "ts";
const LEGACY_SOURCE_EXT: &str = "tsz";

/// 递归加载入口模块及其依赖模块（`import { ... } from "<specifier>"`）。
///
/// 设计目标：
/// - 规则极简、可预测
/// - 异步 I/O（tokio）
/// - 显式报错（不做 Node 的复杂兼容）
pub async fn load_program(entry: &Path) -> Result<Program, TszError> {
    let entry_file = resolve_entry_path(entry).await?;
    let entry_file = canonicalize(&entry_file).await?;

    let mut loader = ModuleLoader::new();
    loader.load_module(&entry_file).await?;
    Ok(Program {
        entry: entry_file,
        modules: loader.finish(),
    })
}

struct ModuleLoader {
    state: HashMap<PathBuf, VisitState>,
    modules: Vec<crate::Module>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Visiting,
    Visited,
}

impl ModuleLoader {
    fn new() -> Self {
        Self {
            state: HashMap::new(),
            modules: Vec::new(),
        }
    }

    async fn load_module(&mut self, entry_path: &Path) -> Result<(), TszError> {
        let entry_path = canonicalize(entry_path).await?;

        match self.state.get(&entry_path).copied() {
            Some(VisitState::Visited) => return Ok(()),
            Some(VisitState::Visiting) => {
                return Err(TszError::Resolve {
                    message: format!("检测到循环依赖: {entry_path:?}"),
                });
            }
            None => {}
        }

        // 用显式栈做 DFS，避免 async 递归（Rust 不允许无限大小 Future）。
        struct Frame {
            path: PathBuf,
            module: crate::Module,
            next_import: usize,
        }

        let mut stack = Vec::new();
        self.state.insert(entry_path.clone(), VisitState::Visiting);
        stack.push(Frame {
            module: parse_module_file(&entry_path).await?,
            path: entry_path,
            next_import: 0,
        });

        while let Some(frame) = stack.last_mut() {
            if frame.next_import >= frame.module.imports.len() {
                let frame = stack.pop().expect("frame exists");
                self.state.insert(frame.path.clone(), VisitState::Visited);
                self.modules.push(frame.module);
                continue;
            }

            let import = &mut frame.module.imports[frame.next_import];
            frame.next_import += 1;

            let dep = resolve_import_specifier(&frame.path, &import.from).await?;
            let dep = canonicalize(&dep).await?;
            import.resolved_path = Some(dep.clone());

            match self.state.get(&dep).copied() {
                Some(VisitState::Visited) => continue,
                Some(VisitState::Visiting) => {
                    return Err(TszError::Resolve {
                        message: format!("检测到循环依赖: {dep:?}"),
                    });
                }
                None => {}
            }

            self.state.insert(dep.clone(), VisitState::Visiting);
            stack.push(Frame {
                module: parse_module_file(&dep).await?,
                path: dep,
                next_import: 0,
            });
        }

        Ok(())
    }

    fn finish(self) -> Vec<crate::Module> {
        // 依赖优先的后序；后续阶段可以自行根据需要重排。
        self.modules
    }
}

async fn parse_module_file(path: &Path) -> Result<crate::Module, TszError> {
    let source = tokio::fs::read_to_string(path).await.map_err(|e| TszError::Io {
        path: path.to_path_buf(),
        source: e,
    })?;
    parser::parse_module(path, &source)
}

async fn resolve_entry_path(entry: &Path) -> Result<PathBuf, TszError> {
    match tokio::fs::metadata(entry).await {
        Ok(meta) => {
            if meta.is_file() {
                return Ok(entry.to_path_buf());
            }
            if meta.is_dir() {
                return resolve_package_entry(entry).await;
            }
            Err(TszError::Resolve {
                message: format!("入口既不是文件也不是目录: {entry:?}"),
            })
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            if entry.extension().is_none() {
                // 允许省略扩展名：优先 `.ts`，再兼容 `.tsz`
                for ext in [DEFAULT_SOURCE_EXT, LEGACY_SOURCE_EXT] {
                    let candidate = entry.with_extension(ext);
                    if let Some(meta) = metadata_optional(&candidate).await? {
                        if meta.is_file() {
                            return Ok(candidate);
                        }
                        if meta.is_dir() {
                            return resolve_package_entry(&candidate).await;
                        }
                    }
                }
            }

            Err(TszError::Resolve {
                message: format!("入口不存在: {entry:?}"),
            })
        }
        Err(e) => Err(TszError::Io {
            path: entry.to_path_buf(),
            source: e,
        }),
    }
}

async fn resolve_import_specifier(current_file: &Path, specifier: &str) -> Result<PathBuf, TszError> {
    let current_dir = current_file.parent().ok_or_else(|| TszError::Resolve {
        message: format!("无法获取当前模块目录: {current_file:?}"),
    })?;

    if is_relative_specifier(specifier) {
        resolve_relative_specifier(current_dir, specifier).await
    } else {
        resolve_package_specifier(current_dir, specifier).await
    }
}

fn is_relative_specifier(specifier: &str) -> bool {
    specifier.starts_with("./") || specifier.starts_with("../")
}

async fn resolve_relative_specifier(base_dir: &Path, specifier: &str) -> Result<PathBuf, TszError> {
    let raw = base_dir.join(specifier);

    if let Some(meta) = metadata_optional(&raw).await? {
        if meta.is_file() {
            return Ok(raw);
        }
        if meta.is_dir() {
            return resolve_package_entry(&raw).await;
        }
        return Err(TszError::Resolve {
            message: format!("无效的导入路径（既不是文件也不是目录）: {raw:?}"),
        });
    }

    if raw.extension().is_some() {
        return Err(TszError::Resolve {
            message: format!("导入模块不存在: {raw:?}"),
        });
    }

    // 允许省略扩展名：优先 `.ts`，再兼容 `.tsz`
    for ext in [DEFAULT_SOURCE_EXT, LEGACY_SOURCE_EXT] {
        let candidate = raw.with_extension(ext);
        if metadata_optional(&candidate).await?.is_some() {
            return Ok(candidate);
        }
    }

    Err(TszError::Resolve {
        message: format!("导入模块不存在: {raw:?}（尝试补全扩展名也失败）"),
    })
}

async fn resolve_package_specifier(base_dir: &Path, specifier: &str) -> Result<PathBuf, TszError> {
    let pkg_parts = parse_pkg_root_parts(specifier)?;

    let mut dir = base_dir.to_path_buf();
    loop {
        let mut candidate = dir.join("node_modules");
        for part in &pkg_parts {
            candidate = candidate.join(part);
        }

        if let Some(meta) = metadata_optional(&candidate).await? {
            if meta.is_dir() {
                return resolve_package_entry(&candidate).await;
            }
        }

        let Some(parent) = dir.parent() else { break };
        dir = parent.to_path_buf();
    }

    Err(TszError::Resolve {
        message: format!("无法在 node_modules 中找到包: {specifier}"),
    })
}

fn parse_pkg_root_parts(specifier: &str) -> Result<Vec<&str>, TszError> {
    if specifier.is_empty() {
        return Err(TszError::Resolve {
            message: "包名不能为空".to_string(),
        });
    }

    let mut parts = specifier.split('/');
    let first = parts.next().unwrap_or_default();
    if first.starts_with('@') {
        let scope = first;
        let Some(name) = parts.next() else {
            return Err(TszError::Resolve {
                message: format!("无效的 scoped 包名: {specifier}（期望 @scope/name）"),
            });
        };
        if parts.next().is_some() {
            return Err(TszError::Resolve {
                message: format!("暂不支持包子路径导入: {specifier}（仅支持包根）"),
            });
        }
        Ok(vec![scope, name])
    } else {
        if parts.next().is_some() {
            return Err(TszError::Resolve {
                message: format!("暂不支持包子路径导入: {specifier}（仅支持包根）"),
            });
        }
        Ok(vec![first])
    }
}

async fn resolve_package_entry(package_dir: &Path) -> Result<PathBuf, TszError> {
    let package_json = package_dir.join("package.json");
    let source = tokio::fs::read_to_string(&package_json)
        .await
        .map_err(|e| TszError::Io {
            path: package_json.clone(),
            source: e,
        })?;

    let pkg: PackageJson = serde_json::from_str(&source).map_err(|e| TszError::Resolve {
        message: format!("解析 package.json 失败: {package_json:?}: {e}"),
    })?;

    let tsz = pkg.tsz.ok_or_else(|| TszError::Resolve {
        message: format!("package.json 缺少 tsz 字段: {package_json:?}"),
    })?;

    let entry = PathBuf::from(tsz.entry);
    if entry.is_absolute() {
        return Err(TszError::Resolve {
            message: format!("tsz.entry 必须是相对路径: {package_json:?}"),
        });
    }

    let entry_path = package_dir.join(entry);
    if !matches!(
        entry_path.extension().and_then(|s| s.to_str()),
        Some(DEFAULT_SOURCE_EXT) | Some(LEGACY_SOURCE_EXT)
    ) {
        return Err(TszError::Resolve {
            message: format!("tsz.entry 必须指向 .ts 或 .tsz 文件: {entry_path:?}"),
        });
    }

    match tokio::fs::metadata(&entry_path).await {
        Ok(meta) if meta.is_file() => Ok(entry_path),
        Ok(_) => Err(TszError::Resolve {
            message: format!("tsz.entry 不是文件: {entry_path:?}"),
        }),
        Err(e) => Err(TszError::Io {
            path: entry_path,
            source: e,
        }),
    }
}

#[derive(Debug, Deserialize)]
struct PackageJson {
    tsz: Option<TszConfig>,
}

#[derive(Debug, Deserialize)]
struct TszConfig {
    entry: String,
}

async fn canonicalize(path: &Path) -> Result<PathBuf, TszError> {
    tokio::fs::canonicalize(path).await.map_err(|e| TszError::Io {
        path: path.to_path_buf(),
        source: e,
    })
}

async fn metadata_optional(path: &Path) -> Result<Option<std::fs::Metadata>, TszError> {
    match tokio::fs::metadata(path).await {
        Ok(m) => Ok(Some(m)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(TszError::Io {
            path: path.to_path_buf(),
            source: e,
        }),
    }
}
