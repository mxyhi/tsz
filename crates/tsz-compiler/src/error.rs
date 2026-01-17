use crate::Span;
use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum TszError {
    #[error("I/O error: {path}: {source}")]
    Io { path: PathBuf, source: std::io::Error },

    #[error("Lex error: {message} ({span:?})")]
    Lex { message: String, span: Span },

    #[error("Parse error: {message} ({span:?})")]
    Parse { message: String, span: Span },

    #[error("Type error: {message} ({span:?})")]
    Type { message: String, span: Span },

    #[error("Codegen error: {message}")]
    Codegen { message: String },

    #[error("Link error: {message}")]
    Link { message: String },

    #[error("Runtime error: {message}")]
    Runtime { message: String },

    #[error("Module resolution error: {message}")]
    Resolve { message: String },
}
