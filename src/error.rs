use thiserror::Error;

use crate::jam::Shortcut;

pub type ExecResult<T> = Result<T, ExecError>;

#[derive(Error, Debug, PartialEq)]
pub enum ExecError {
    #[error("given shortcut '{shortcut}' is ambiguous (one of: {conflicts:?}?)")]
    Ambiguous {
        shortcut: Shortcut,
        conflicts: Vec<String>,
    },
    #[error("no command for given shortcut '{shortcut}'")]
    NotFound { shortcut: Shortcut },
    #[error("target '{name}' has no executable function")]
    CannotExec { name: String },
    #[error("{description}")]
    Reconciliation { description: String },
    #[error("{description}")]
    Executor { description: String },
    #[error("failed to execute dependency ('{dep_name}'): {err}")]
    Dependency {
        dep_name: String,
        err: Box<ExecError>,
    },
}
