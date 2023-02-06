use core::fmt;
use std::collections::HashMap;

use anyhow::Result;
use serde::Deserialize;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Eq, Copy, Clone, PartialEq, Hash, EnumIter, Deserialize, Debug)]
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
    executables: ExecutableMapping,
}

impl Executor {
    pub fn new() -> Executor {
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
        println!("Dry running cmd: {}", cmd);
        Ok(true)
    }
}

pub struct Shell {
    shell: String,
    exec_flag: &'static str,
}

impl Shell {
    pub fn new() -> Shell {
        let shell = std::env::var("SHELL").map_or(String::from("bash"), |oss| oss.to_string());
        Shell {
            shell,
            exec_flag: "-c",
        }
    }
}

impl Executable for Shell {
    fn execute(&self, cmd: &str) -> Result<bool> {
        println!(
            "Executing cmd: {}",
            format!("{} {} '{}'", self.shell, self.exec_flag, cmd)
        );
        let exit_status = std::process::Command::new(&self.shell)
            .arg(self.exec_flag)
            .arg(cmd)
            .status()?;
        Ok(exit_status.code() == Some(0))
    }
}

pub struct Mock {}

impl Mock {
    pub fn new() -> Mock {
        Mock {}
    }
}

impl Executable for Mock {
    fn execute(&self, cmd: &str) -> Result<bool> {
        println!("Mocking cmd: {}", cmd);
        Ok(true)
    }
}
