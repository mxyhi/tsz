use crate::Span;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub path: Option<PathBuf>,
    pub span: Option<Span>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            path: None,
            span: None,
            notes: Vec::new(),
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Warning,
            message: message.into(),
            path: None,
            span: None,
            notes: Vec::new(),
        }
    }

    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.path = Some(path);
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostics {
    items: Vec<Diagnostic>,
    max_errors: usize,
    error_count: usize,
}

impl Diagnostics {
    pub fn new(max_errors: usize) -> Self {
        Self {
            items: Vec::new(),
            max_errors: max_errors.max(1),
            error_count: 0,
        }
    }

    pub fn push(&mut self, diag: Diagnostic) {
        if diag.severity == Severity::Error {
            if self.error_count >= self.max_errors {
                return;
            }
            self.error_count += 1;
        }
        self.items.push(diag);
    }

    pub fn error_at(&mut self, path: &Path, span: Span, message: impl Into<String>) {
        self.push(Diagnostic::error(message).with_path(path.to_path_buf()).with_span(span));
    }

    pub fn warn_at(&mut self, path: &Path, span: Span, message: impl Into<String>) {
        self.push(Diagnostic::warning(message).with_path(path.to_path_buf()).with_span(span));
    }

    pub fn error(&mut self, message: impl Into<String>) {
        self.push(Diagnostic::error(message));
    }

    pub fn warning(&mut self, message: impl Into<String>) {
        self.push(Diagnostic::warning(message));
    }

    pub fn is_full(&self) -> bool {
        self.error_count >= self.max_errors
    }

    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    pub fn items(&self) -> &[Diagnostic] {
        &self.items
    }

    pub fn error_count(&self) -> usize {
        self.error_count
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: PathBuf,
    pub source: String,
    line_starts: Vec<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpanLocation {
    pub start: LineCol,
    pub end: LineCol,
}

impl SourceFile {
    pub fn new(path: PathBuf, source: String) -> Self {
        let mut line_starts = vec![0];
        for (idx, b) in source.as_bytes().iter().enumerate() {
            if *b == b'\n' {
                line_starts.push(idx + 1);
            }
        }
        Self {
            path,
            source,
            line_starts,
        }
    }

    pub fn line_col(&self, offset: usize) -> Option<LineCol> {
        if offset > self.source.len() {
            return None;
        }
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        let line_start = *self.line_starts.get(line_idx)?;
        let col = self.source[line_start..offset].chars().count() + 1;
        Some(LineCol {
            line: line_idx + 1,
            col,
        })
    }

    pub fn span_location(&self, span: Span) -> Option<SpanLocation> {
        let start = self.line_col(span.start)?;
        let end = self.line_col(span.end)?;
        Some(SpanLocation { start, end })
    }

    pub fn line_text(&self, line_index: usize) -> Option<&str> {
        let start = *self.line_starts.get(line_index)?;
        let end = match self.line_starts.get(line_index + 1) {
            Some(next) => next.saturating_sub(1),
            None => self.source.len(),
        };
        self.source.get(start..end)
    }

    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

#[derive(Debug, Default, Clone)]
pub struct SourceMap {
    files: HashMap<PathBuf, SourceFile>,
}

impl SourceMap {
    pub fn insert(&mut self, path: PathBuf, source: String) {
        self.files.insert(path.clone(), SourceFile::new(path, source));
    }

    pub fn get(&self, path: &Path) -> Option<&SourceFile> {
        self.files.get(path)
    }
}

pub fn render_diagnostics(diags: &Diagnostics, sources: &SourceMap) -> String {
    let mut items: Vec<&Diagnostic> = diags.items().iter().collect();
    items.sort_by(|a, b| {
        let path_a = a.path.as_ref().map(|p| p.as_path());
        let path_b = b.path.as_ref().map(|p| p.as_path());
        match (path_a, path_b) {
            (Some(pa), Some(pb)) => {
                let by_path = pa.cmp(pb);
                if by_path != std::cmp::Ordering::Equal {
                    return by_path;
                }
            }
            (Some(_), None) => return std::cmp::Ordering::Less,
            (None, Some(_)) => return std::cmp::Ordering::Greater,
            (None, None) => {}
        }

        match (a.span, b.span) {
            (Some(sa), Some(sb)) => sa.start.cmp(&sb.start),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        }
    });

    let mut out = String::new();
    for (idx, diag) in items.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        render_single_diagnostic(&mut out, diag, sources);
    }
    out
}

fn render_single_diagnostic(out: &mut String, diag: &Diagnostic, sources: &SourceMap) {
    let sev = match diag.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    };

    match (diag.path.as_ref(), diag.span) {
        (Some(path), Some(span)) => {
            if let Some(src) = sources.get(path) {
                if let Some(loc) = src.span_location(span) {
                    let _ = std::fmt::Write::write_fmt(
                        out,
                        format_args!(
                            "{}:{}:{}: {}: {}\n",
                            path.display(),
                            loc.start.line,
                            loc.start.col,
                            sev,
                            diag.message
                        ),
                    );
                    render_source_snippet(out, src, loc);
                    render_notes(out, diag);
                    return;
                }
            }

            let _ = std::fmt::Write::write_fmt(
                out,
                format_args!("{}: {}: {}\n", path.display(), sev, diag.message),
            );
            render_notes(out, diag);
        }
        (Some(path), None) => {
            let _ = std::fmt::Write::write_fmt(
                out,
                format_args!("{}: {}: {}\n", path.display(), sev, diag.message),
            );
            render_notes(out, diag);
        }
        (None, _) => {
            let _ = std::fmt::Write::write_fmt(out, format_args!("{}: {}\n", sev, diag.message));
            render_notes(out, diag);
        }
    }
}

fn render_source_snippet(out: &mut String, src: &SourceFile, loc: SpanLocation) {
    let start_line_idx = loc.start.line.saturating_sub(1);
    let end_line_idx = loc.end.line.saturating_sub(1);
    let width = loc.start.line.max(loc.end.line).to_string().len();

    out.push_str("  |\n");

    if let Some(line) = src.line_text(start_line_idx) {
        let line_no = loc.start.line;
        let _ = std::fmt::Write::write_fmt(out, format_args!("{:>width$} | {}\n", line_no, line, width = width));
        let caret = build_caret_line(line, loc.start.col, if start_line_idx == end_line_idx { loc.end.col } else { line.chars().count() + 1 });
        let _ = std::fmt::Write::write_fmt(out, format_args!("{:>width$} | {}\n", "", caret, width = width));
    }

    if end_line_idx != start_line_idx {
        let _ = std::fmt::Write::write_fmt(out, format_args!("{:>width$} | ...\n", "", width = width));
        if let Some(line) = src.line_text(end_line_idx) {
            let line_no = loc.end.line;
            let _ = std::fmt::Write::write_fmt(out, format_args!("{:>width$} | {}\n", line_no, line, width = width));
            let caret = build_caret_line(line, 1, loc.end.col);
            let _ = std::fmt::Write::write_fmt(out, format_args!("{:>width$} | {}\n", "", caret, width = width));
        }
    }
}

fn build_caret_line(line: &str, start_col: usize, end_col: usize) -> String {
    let line_len = line.chars().count();
    let start = start_col.max(1).min(line_len.saturating_add(1));
    let end = end_col.max(start).min(line_len.saturating_add(1));
    let caret_len = end.saturating_sub(start).max(1);
    let mut s = String::new();
    s.push_str(&" ".repeat(start.saturating_sub(1)));
    s.push_str(&"^".repeat(caret_len));
    s
}

fn render_notes(out: &mut String, diag: &Diagnostic) {
    for note in &diag.notes {
        let _ = std::fmt::Write::write_fmt(out, format_args!("note: {}\n", note));
    }
}
