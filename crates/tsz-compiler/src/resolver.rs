use crate::{Program, TszError, parser};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

const DEFAULT_SOURCE_EXT: &str = "ts";
const LEGACY_SOURCE_EXT: &str = "tsz";

/// Recursively loads the entry module and its dependencies (`import { ... } from "<specifier>"`).
///
/// Design goals:
/// - Minimal, predictable rules
/// - Async I/O (tokio)
/// - Explicit errors (no complex Node compatibility)
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
                    message: format!("Cycle detected: {entry_path:?}"),
                });
            }
            None => {}
        }

        // Use an explicit DFS stack to avoid async recursion (Rust cannot represent infinitely-sized Futures).
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
                        message: format!("Cycle detected: {dep:?}"),
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
        // Postorder with dependencies first; later stages can reorder as needed.
        self.modules
    }
}

async fn parse_module_file(path: &Path) -> Result<crate::Module, TszError> {
    let source = tokio::fs::read_to_string(path)
        .await
        .map_err(|e| TszError::Io {
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
                message: format!("Entry is neither a file nor a directory: {entry:?}"),
            })
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            if entry.extension().is_none() {
                // Allow omitting the extension: prefer `.ts`, then try legacy `.tsz`.
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
                message: format!("Entry does not exist: {entry:?}"),
            })
        }
        Err(e) => Err(TszError::Io {
            path: entry.to_path_buf(),
            source: e,
        }),
    }
}

async fn resolve_import_specifier(
    current_file: &Path,
    specifier: &str,
) -> Result<PathBuf, TszError> {
    let current_dir = current_file.parent().ok_or_else(|| TszError::Resolve {
        message: format!("Failed to get current module directory: {current_file:?}"),
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
    resolve_path_like(&raw).await
}

async fn resolve_package_specifier(base_dir: &Path, specifier: &str) -> Result<PathBuf, TszError> {
    let pkg = parse_package_specifier(specifier)?;

    let mut dir = base_dir.to_path_buf();
    loop {
        let mut candidate = dir.join("node_modules");
        for part in &pkg.pkg_parts {
            candidate = candidate.join(part);
        }

        if let Some(meta) = metadata_optional(&candidate).await? {
            if meta.is_dir() {
                if pkg.subpath_parts.is_empty() {
                    return resolve_package_entry(&candidate).await;
                }

                // Require every node_modules import to point to a valid TSZ package (package.json + tsz.entry),
                // even if the import uses a subpath. This keeps rules simple and consistent.
                let _entry = resolve_package_entry(&candidate).await?;

                return resolve_package_subpath(&candidate, &pkg.subpath_parts).await;
            }
        }

        let Some(parent) = dir.parent() else { break };
        dir = parent.to_path_buf();
    }

    Err(TszError::Resolve {
        message: format!("Package not found in node_modules: {specifier}"),
    })
}

#[derive(Debug)]
struct PackageSpecifier<'a> {
    pkg_parts: Vec<&'a str>,
    subpath_parts: Vec<&'a str>,
}

fn parse_package_specifier(specifier: &str) -> Result<PackageSpecifier<'_>, TszError> {
    if specifier.is_empty() {
        return Err(TszError::Resolve {
            message: "Package specifier cannot be empty".to_string(),
        });
    }

    // Keep rules minimal/predictable:
    // - <pkg> or @scope/<pkg> is the package root name
    // - optional "/<subpath...>" is resolved as a path under the package root directory
    let mut parts = specifier.split('/');
    let first = parts.next().unwrap_or_default();
    if first.is_empty() {
        return Err(TszError::Resolve {
            message: format!("Invalid package specifier: {specifier}"),
        });
    }

    if first.starts_with('@') {
        if first.len() == 1 {
            return Err(TszError::Resolve {
                message: format!("Invalid scoped package name: {specifier} (expected @scope/name)"),
            });
        }

        let scope = first;
        let Some(name) = parts.next() else {
            return Err(TszError::Resolve {
                message: format!("Invalid scoped package name: {specifier} (expected @scope/name)"),
            });
        };
        if name.is_empty() {
            return Err(TszError::Resolve {
                message: format!("Invalid scoped package name: {specifier} (expected @scope/name)"),
            });
        }
        validate_package_name_parts(specifier, &[scope, name])?;

        let subpath_parts: Vec<&str> = parts.collect();
        validate_package_subpath_parts(specifier, &subpath_parts)?;

        Ok(PackageSpecifier {
            pkg_parts: vec![scope, name],
            subpath_parts,
        })
    } else {
        let name = first;
        validate_package_name_parts(specifier, &[name])?;
        let subpath_parts: Vec<&str> = parts.collect();
        validate_package_subpath_parts(specifier, &subpath_parts)?;

        Ok(PackageSpecifier {
            pkg_parts: vec![name],
            subpath_parts,
        })
    }
}

fn validate_package_name_parts(specifier: &str, parts: &[&str]) -> Result<(), TszError> {
    for part in parts {
        if part.is_empty() {
            return Err(TszError::Resolve {
                message: format!("Invalid package name: {specifier}"),
            });
        }
        if *part == "." || *part == ".." {
            return Err(TszError::Resolve {
                message: format!("Invalid package name: {specifier}"),
            });
        }
        if part.contains('\\') {
            return Err(TszError::Resolve {
                message: format!("Invalid package name: {specifier} (\"\\\\\" is not allowed)"),
            });
        }
        // Reject Windows path prefixes / absolute paths (e.g. "C:" / "\\\\server\\share") to avoid escaping node_modules.
        let mut comps = Path::new(part).components();
        match (comps.next(), comps.next()) {
            (Some(std::path::Component::Normal(_)), None) => {}
            _ => {
                return Err(TszError::Resolve {
                    message: format!("Invalid package name: {specifier}"),
                });
            }
        }
    }
    Ok(())
}

fn validate_package_subpath_parts(specifier: &str, parts: &[&str]) -> Result<(), TszError> {
    for part in parts {
        if part.is_empty() {
            return Err(TszError::Resolve {
                message: format!("Invalid package subpath import: {specifier} (empty segment)"),
            });
        }
        if *part == "." || *part == ".." {
            return Err(TszError::Resolve {
                message: format!(
                    "Invalid package subpath import: {specifier} (dot segments are not allowed)"
                ),
            });
        }

        // Keep specifiers consistent and avoid platform-specific path separators / prefixes.
        if part.contains('\\') {
            return Err(TszError::Resolve {
                message: format!(
                    "Invalid package subpath import: {specifier} (\"\\\\\" is not allowed)"
                ),
            });
        }

        // On Windows, a segment like "C:" is treated as a path prefix and would escape the package root.
        let mut comps = Path::new(part).components();
        match (comps.next(), comps.next()) {
            (Some(std::path::Component::Normal(_)), None) => {}
            _ => {
                return Err(TszError::Resolve {
                    message: format!(
                        "Invalid package subpath import: {specifier} (invalid segment: {part})"
                    ),
                });
            }
        }
    }
    Ok(())
}

async fn resolve_package_subpath(
    package_dir: &Path,
    subpath_parts: &[&str],
) -> Result<PathBuf, TszError> {
    let mut raw = package_dir.to_path_buf();
    for part in subpath_parts {
        raw = raw.join(part);
    }
    resolve_path_like(&raw).await
}

async fn resolve_path_like(raw: &Path) -> Result<PathBuf, TszError> {
    if let Some(meta) = metadata_optional(raw).await? {
        if meta.is_file() {
            return Ok(raw.to_path_buf());
        }
        if meta.is_dir() {
            return resolve_package_entry(raw).await;
        }
        return Err(TszError::Resolve {
            message: format!("Invalid import path (neither file nor directory): {raw:?}"),
        });
    }

    if raw.extension().is_some() {
        return Err(TszError::Resolve {
            message: format!("Imported module does not exist: {raw:?}"),
        });
    }

    // Allow omitting the extension: prefer `.ts`, then try legacy `.tsz`.
    for ext in [DEFAULT_SOURCE_EXT, LEGACY_SOURCE_EXT] {
        let candidate = raw.with_extension(ext);
        if let Some(meta) = metadata_optional(&candidate).await? {
            if meta.is_file() {
                return Ok(candidate);
            }
            if meta.is_dir() {
                return resolve_package_entry(&candidate).await;
            }
        }
    }

    Err(TszError::Resolve {
        message: format!(
            "Imported module does not exist: {raw:?} (extension completion also failed)"
        ),
    })
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
        message: format!("Failed to parse package.json: {package_json:?}: {e}"),
    })?;

    let tsz = pkg.tsz.ok_or_else(|| TszError::Resolve {
        message: format!("package.json is missing the tsz field: {package_json:?}"),
    })?;

    let entry = PathBuf::from(tsz.entry);
    if entry.is_absolute() {
        return Err(TszError::Resolve {
            message: format!("tsz.entry must be a relative path: {package_json:?}"),
        });
    }

    let entry_path = package_dir.join(entry);
    if !matches!(
        entry_path.extension().and_then(|s| s.to_str()),
        Some(DEFAULT_SOURCE_EXT) | Some(LEGACY_SOURCE_EXT)
    ) {
        return Err(TszError::Resolve {
            message: format!("tsz.entry must point to a .ts or .tsz file: {entry_path:?}"),
        });
    }

    match tokio::fs::metadata(&entry_path).await {
        Ok(meta) if meta.is_file() => Ok(entry_path),
        Ok(_) => Err(TszError::Resolve {
            message: format!("tsz.entry is not a file: {entry_path:?}"),
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
    tokio::fs::canonicalize(path)
        .await
        .map_err(|e| TszError::Io {
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
