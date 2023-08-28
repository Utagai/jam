use thiserror::Error;

use crate::jam::Shortcut;

pub type ExecResult<T> = Result<T, ExecError>;

// TODO: Should Ambiguous case take a list of strings and it intersperses itself?
#[derive(Error, Debug, PartialEq)]
pub enum ExecError {
    #[error("given shortcut '{shortcut}' is ambiguous (i.e. is it {conflict_msg}?)")]
    Ambiguous {
        shortcut: Shortcut,
        conflict_msg: String,
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
