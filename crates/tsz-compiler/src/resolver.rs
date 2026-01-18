use crate::diagnostics::{Diagnostics, SourceMap};
use crate::{parser, Program, TszError};
use serde::Deserialize;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use tokio::task::JoinSet;

const DEFAULT_SOURCE_EXT: &str = "ts";
const LEGACY_SOURCE_EXT: &str = "tsz";

/// Recursively loads the entry module and its dependencies (`import { ... } from "<specifier>"`).
///
/// Design goals:
/// - Minimal, predictable rules
/// - Async I/O (tokio)
/// - Explicit errors (no complex Node compatibility)
pub async fn load_program(entry: &Path, diags: &mut Diagnostics) -> Result<Program, TszError> {
    let entry_file = resolve_entry_path(entry).await?;
    let entry_file = canonicalize(&entry_file).await?;

    let mut loader = ModuleLoader::new(diags);
    loader.load_all(entry_file.clone()).await?;
    let (modules, sources) = loader.finish();
    Ok(Program {
        entry: entry_file,
        modules,
        sources,
    })
}

struct ModuleLoader<'d> {
    modules: HashMap<PathBuf, crate::Module>,
    edges: HashMap<PathBuf, Vec<PathBuf>>,
    sources: SourceMap,
    diags: &'d mut Diagnostics,
    max_errors: usize,
}

struct ModuleParseResult {
    module: crate::Module,
    source: String,
    diagnostics: Diagnostics,
}

struct ModuleParseOutcome {
    path: PathBuf,
    result: Result<ModuleParseResult, TszError>,
}

impl<'d> ModuleLoader<'d> {
    fn new(diags: &'d mut Diagnostics) -> Self {
        Self {
            modules: HashMap::new(),
            edges: HashMap::new(),
            sources: SourceMap::default(),
            max_errors: diags.max_errors(),
            diags,
        }
    }

    async fn load_all(&mut self, entry_path: PathBuf) -> Result<(), TszError> {
        self.run_load_queue(entry_path).await
    }

    async fn run_load_queue(&mut self, entry: PathBuf) -> Result<(), TszError> {
        let max_parallel = max_parallelism();
        let mut scheduled = HashSet::new();
        let mut pending = VecDeque::new();
        scheduled.insert(entry.clone());
        pending.push_back(entry.clone());

        let mut join_set = JoinSet::new();
        let mut in_flight = 0usize;

        while !pending.is_empty() || in_flight > 0 {
            while in_flight < max_parallel {
                let Some(path) = pending.pop_front() else { break };
                join_set.spawn(parse_module_task(path, self.max_errors));
                in_flight += 1;
            }

            let Some(result) = join_set.join_next().await else { break };
            in_flight = in_flight.saturating_sub(1);
            let outcome = result.map_err(|e| TszError::Resolve {
                message: format!("Failed to join module task: {e}"),
            })?;
            self.handle_outcome(outcome, &entry, &mut pending, &mut scheduled).await?;
        }

        Ok(())
    }

    async fn handle_outcome(
        &mut self,
        outcome: ModuleParseOutcome,
        entry: &Path,
        pending: &mut VecDeque<PathBuf>,
        scheduled: &mut HashSet<PathBuf>,
    ) -> Result<(), TszError> {
        let ModuleParseOutcome { path, result } = outcome;
        let parsed = match result {
            Ok(parsed) => parsed,
            Err(err) => return self.handle_parse_error(path, entry, err),
        };

        self.merge_diagnostics(parsed.diagnostics);
        self.sources.insert(path.clone(), parsed.source);

        let mut module = parsed.module;
        let deps = resolve_imports_for_module(&mut module, self.diags).await?;

        self.edges.insert(path.clone(), deps.clone());
        self.modules.insert(path.clone(), module);

        for dep in deps {
            if scheduled.insert(dep.clone()) {
                pending.push_back(dep);
            }
        }

        Ok(())
    }

    fn handle_parse_error(&mut self, path: PathBuf, entry: &Path, err: TszError) -> Result<(), TszError> {
        if path == entry {
            return Err(err);
        }

        match err {
            TszError::Io { path, source } => {
                self.diags.error(format!("I/O error: {path:?}: {source}"));
            }
            TszError::Resolve { message } => {
                self.diags.error(message);
            }
            other => return Err(other),
        }
        Ok(())
    }

    fn merge_diagnostics(&mut self, local: Diagnostics) {
        for diag in local.items() {
            self.diags.push(diag.clone());
        }
    }

    fn finish(mut self) -> (Vec<crate::Module>, SourceMap) {
        let order = topo_sort_modules(&self.modules, &self.edges, self.diags);
        let mut modules = Vec::with_capacity(order.len());
        for path in order {
            if let Some(module) = self.modules.remove(&path) {
                modules.push(module);
            }
        }
        (modules, self.sources)
    }
}

async fn parse_module_task(path: PathBuf, max_errors: usize) -> ModuleParseOutcome {
    let result = parse_module_file(&path, max_errors).await;
    ModuleParseOutcome { path, result }
}

async fn parse_module_file(path: &Path, max_errors: usize) -> Result<ModuleParseResult, TszError> {
    let source = tokio::fs::read_to_string(path)
        .await
        .map_err(|e| TszError::Io {
            path: path.to_path_buf(),
            source: e,
        })?;
    let mut diags = Diagnostics::new(max_errors);
    let module = parser::parse_module(path, &source, &mut diags);
    Ok(ModuleParseResult {
        module,
        source,
        diagnostics: diags,
    })
}

async fn resolve_imports_for_module(
    module: &mut crate::Module,
    diags: &mut Diagnostics,
) -> Result<Vec<PathBuf>, TszError> {
    let mut deps = BTreeSet::new();
    for import in &mut module.imports {
        let dep = match resolve_import_specifier(&module.path, &import.from).await {
            Ok(dep) => dep,
            Err(TszError::Resolve { message }) => {
                diags.error_at(&module.path, import.span, message);
                continue;
            }
            Err(TszError::Io { path, source }) => {
                diags.error(format!("I/O error: {path:?}: {source}"));
                continue;
            }
            Err(e) => return Err(e),
        };
        let dep = canonicalize(&dep).await?;
        import.resolved_path = Some(dep.clone());
        deps.insert(dep);
    }
    Ok(deps.into_iter().collect())
}

fn topo_sort_modules(
    modules: &HashMap<PathBuf, crate::Module>,
    edges: &HashMap<PathBuf, Vec<PathBuf>>,
    diags: &mut Diagnostics,
) -> Vec<PathBuf> {
    let mut indegree = HashMap::new();
    for path in modules.keys() {
        indegree.insert(path.clone(), 0usize);
    }
    for (src, deps) in edges {
        if !modules.contains_key(src) {
            continue;
        }
        for dep in deps {
            if let Some(count) = indegree.get_mut(dep) {
                *count += 1;
            }
        }
    }

    let mut ready = BTreeSet::new();
    for (path, count) in &indegree {
        if *count == 0 {
            ready.insert(path.clone());
        }
    }

    let mut order = Vec::with_capacity(modules.len());
    while let Some(path) = ready.iter().next().cloned() {
        ready.remove(&path);
        order.push(path.clone());
        if let Some(deps) = edges.get(&path) {
            for dep in deps {
                if let Some(count) = indegree.get_mut(dep) {
                    *count = count.saturating_sub(1);
                    if *count == 0 {
                        ready.insert(dep.clone());
                    }
                }
            }
        }
    }

    if order.len() < indegree.len() {
        let mut cyclic: Vec<_> = indegree
            .into_iter()
            .filter(|(_, count)| *count > 0)
            .map(|(path, _)| path)
            .collect();
        cyclic.sort();
        for path in &cyclic {
            diags.error(format!("Cycle detected: {path:?}"));
        }
        order.extend(cyclic);
    }

    order
}

fn max_parallelism() -> usize {
    // Keep concurrency bounded for predictable resource usage.
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .clamp(1, 8)
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
