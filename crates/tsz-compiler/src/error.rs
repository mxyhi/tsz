use crate::Span;
use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum TszError {
    #[error("I/O 失败: {path}: {source}")]
    Io { path: PathBuf, source: std::io::Error },

    #[error("词法错误: {message} ({span:?})")]
    Lex { message: String, span: Span },

    #[error("语法错误: {message} ({span:?})")]
    Parse { message: String, span: Span },

    #[error("类型错误: {message} ({span:?})")]
    Type { message: String, span: Span },

    #[error("代码生成失败: {message}")]
    Codegen { message: String },

    #[error("链接失败: {message}")]
    Link { message: String },

    #[error("运行时失败: {message}")]
    Runtime { message: String },

    #[error("模块解析失败: {message}")]
    Resolve { message: String },
}
