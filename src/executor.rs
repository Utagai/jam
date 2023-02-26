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
}

impl fmt::Display for ExecuteKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stringified = match self {
            Self::DryRun => "dryrun",
            Self::Shell => "shell",
        };
        write!(f, "{stringified}")
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

        for kind in ExecuteKind::iter() {
            if !mapping.contains_key(&kind) {
                panic!("unregistered executable kind: '{kind}'")
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
        println!("dry running cmd: {cmd}");
        Ok(true)
    }
}

pub struct Shell {
    shell: String,
    exec_flag: &'static str,
}

impl Shell {
    pub fn new() -> Shell {
        let shell = std::env::var("SHELL").map_or(String::from("bash"), |oss| oss);
        Shell {
            shell,
            exec_flag: "-c",
        }
    }
}

impl Executable for Shell {
    fn execute(&self, cmd: &str) -> Result<bool> {
        let exit_status = std::process::Command::new(&self.shell)
            .arg(self.exec_flag)
            .arg(cmd)
            .status()?;
        Ok(exit_status.code() == Some(0))
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fmt::{Debug, Display},
        path::PathBuf,
    };
    use std::{fs::metadata, path::Path};

    use lazy_static::lazy_static;
    use rstest::rstest;
    use tempdir::TempDir;

    use super::*;

    fn check_file<P: AsRef<Path>>(path: P) -> bool {
        metadata(path).is_ok()
    }

    fn check_file_contents<P: AsRef<Path>>(path: P, expected_content: &str) {
        assert_eq!(
            std::fs::read_to_string(path)
                .expect("failed to read output file")
                .trim(),
            expected_content.trim(),
        );
    }

    static JAM_SHELL_DIR_NAME: &str = "jam_shell_tests";

    struct TmpFile(PathBuf);

    impl Drop for TmpFile {
        fn drop(&mut self) {
            if !self.0.pop() {
                panic!(
                    "temporary path {} does not have a directory?",
                    self.0.display(),
                );
            }
            println!("deleting!");
            std::fs::remove_dir_all(&self.0).expect("failed to clean up temporary directory")
        }
    }

    impl AsRef<Path> for TmpFile {
        fn as_ref(&self) -> &Path {
            &self.0
        }
    }

    impl Display for TmpFile {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.0.fmt(f)
        }
    }

    impl TmpFile {
        fn new() -> Self {
            let dir =
                TempDir::new(JAM_SHELL_DIR_NAME).expect("failed to create temporary directory");
            TmpFile(dir.into_path().join("test.txt"))
        }
    }

    mod shell {
        use super::*;

        fn exec_shell_cmd(cmd: &str) {
            let sh = Shell::new();
            assert!(sh.execute(cmd).expect("failed to execute shell command"))
        }

        #[test]
        fn basic_cmd() {
            let file_path = TmpFile::new();
            exec_shell_cmd(&format!("touch {file_path}"));
            assert!(check_file(file_path))
        }

        #[test]
        fn redirection() {
            let file_path = TmpFile::new();
            let expected_content = "haha";
            exec_shell_cmd(&format!("echo '{expected_content}' > {file_path}"));
            check_file_contents(file_path, expected_content);
        }

        #[test]
        fn env_var_subst() {
            let file_path = TmpFile::new();
            exec_shell_cmd(&format!("echo $PWD > {file_path}"));
            check_file_contents(
                file_path,
                &std::env::current_dir()
                    .expect("failed to get cwd")
                    .display()
                    .to_string(),
            );
        }

        #[test]
        fn pipe_works() {
            let file_path = TmpFile::new();
            let match_part = "baba";
            exec_shell_cmd(&format!(
                "echo 'haha baba' | grep -o '{match_part}' > {file_path}"
            ));
            check_file_contents(file_path, match_part);
        }
    }

    #[test]
    fn dryrun_does_nothing() {
        let dr = DryRunner::new();
        assert!(dr
            .execute("touch i_shouldnt_be_created")
            .expect("did not expect the dryrun execution to fail"),);
        assert!(!check_file("i_shouldnt_be_created"));
    }

    static EXECUTOR_EXPECTED_CONTENT: &str = "haha";
    lazy_static! {
        static ref TMP_FILEPATH: PathBuf = TempDir::new("jam_executor_tests")
            .expect("failed to create temp dir")
            .into_path()
            .join("test.txt");
    }

    #[rstest]
    #[case::shell(ExecuteKind::Shell, &format!("echo '{EXECUTOR_EXPECTED_CONTENT}' > {}", TMP_FILEPATH.display()))]
    fn executor(#[case] exec_kind: ExecuteKind, #[case] cmd: &str) {
        // Create the tmp dir & file if they do not already exist.
        std::fs::create_dir_all(
            TMP_FILEPATH
                .as_path()
                .parent()
                .expect("temp file path should have a temp parent directory"),
        )
        .expect("failed to create temp file directory");
        std::fs::File::create(TMP_FILEPATH.as_path()).expect("failed to create temp file");

        // Run the actual test.
        let executor = Executor::new();
        assert!(executor
            .execute(exec_kind, cmd)
            .expect("failed to execute command"));
        check_file_contents(TMP_FILEPATH.as_path(), EXECUTOR_EXPECTED_CONTENT);

        // Clean up after ourselves. Note that if this is the last
        // case we're testing, then we will have cleaned up for good
        // and left without leaking anything.
        std::fs::remove_dir_all(
            TMP_FILEPATH
                .parent()
                .expect("temp file should have a temp parent directory"),
        )
        .expect("failed to delete temp dir");
    }
}
