use core::fmt;
use std::collections::HashMap;

use anyhow::Result;
use serde::Deserialize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Eq, PartialEq, Hash, EnumIter, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum ExecuteKind {
    DryRun,
    Shell,
    #[cfg(test)]
    Mock,
}

impl fmt::Display for ExecuteKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stringified = match self {
            Self::DryRun => "dryrun",
            Self::Shell => "shell",
            #[cfg(test)]
            Self::Mock => "mock",
        };
        write!(f, "{}", stringified)
    }
}

type ExecutableMapping = HashMap<ExecuteKind, Box<dyn Executable>>;

pub struct Executor {
    // TODO: Right now, we initialize this once and only once at
    // startup. Perhaps this means we can have &'static Executable? I
    // don't know if rustc can do that.
    executables: ExecutableMapping,
}

impl Executor {
    pub fn new() -> Executor {
        // TODO: This initialization is more expensive than
        // necessary. For example, if the entire config only ever uses
        // a single kind of executable, then we'll construct a bunch
        // of executables we won't ever use.
        let mut mapping: ExecutableMapping = HashMap::new();
        mapping.insert(ExecuteKind::DryRun, Box::new(DryRunner::new()));
        mapping.insert(ExecuteKind::Shell, Box::new(Shell::new()));
        #[cfg(test)]
        mapping.insert(ExecuteKind::Mock, Box::new(Mock::new()));

        for kind in ExecuteKind::iter() {
            if !mapping.contains_key(&kind) {
                panic!("unregistered executable kind: '{}'", kind)
            }
        }

        Executor {
            executables: mapping,
        }
    }

    pub fn execute(&self, kind: ExecuteKind, cmd: &str) -> Result<bool> {
        let executable = self.executables.get(&kind);
        // Unwrapping should be safe due to our validation logic in the constructor.
        executable.unwrap().execute(cmd)
    }
}

pub trait Executable {
    // TODO: It is not clear to me if Result<u32> is the right return
    // value. Namely, is u32 sufficient for all possible
    // implementations of Executor?
    /// Execute executes the command given and returns a Result<u32>
    /// indicating the commands exit status.
    fn execute(&self, cmd: &str) -> Result<bool>;
}

pub struct DryRunner {}

impl DryRunner {
    pub fn new() -> DryRunner {
        DryRunner {}
    }
}

impl Executable for DryRunner {
    fn execute(&self, cmd: &str) -> Result<bool> {
        println!("Executing cmd: {}", cmd);
        Ok(true)
    }
}

pub struct Shell {}

impl Shell {
    pub fn new() -> Shell {
        Shell {}
    }
}

impl Executable for Shell {
    fn execute(&self, cmd: &str) -> Result<bool> {
        println!("Executing cmd: {}", cmd);
        Ok(true)
    }
}

// TODO: This should be moved to a mod tests.
pub struct Mock {}

impl Mock {
    pub fn new() -> Mock {
        Mock {}
    }
}

impl Executable for Mock {
    fn execute(&self, cmd: &str) -> Result<bool> {
        println!("Executing cmd: {}", cmd);
        Ok(true)
    }
}
