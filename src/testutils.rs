#[cfg(test)]
pub mod tmp {
    use std::{
        fmt::{Debug, Display, Formatter, Result},
        path::PathBuf,
    };
    use std::{fs::metadata, path::Path};

    use tempdir::TempDir;

    pub fn check_file<P: AsRef<Path>>(path: P) -> bool {
        metadata(path).is_ok()
    }

    pub fn check_file_contents<P: AsRef<Path>>(path: P, expected_content: &str) {
        assert_eq!(
            std::fs::read_to_string(path)
                .expect("failed to read output file")
                .trim(),
            expected_content.trim(),
        );
    }

    static JAM_SHELL_DIR_NAME: &str = "jam_shell_tests";

    pub struct TmpFile(PathBuf);

    impl Drop for TmpFile {
        fn drop(&mut self) {
            if !self.0.pop() {
                panic!(
                    "temporary path {} does not have a directory?",
                    self.0.display(),
                );
            }
            std::fs::remove_dir_all(&self.0).expect("failed to clean up temporary directory")
        }
    }

    impl AsRef<Path> for TmpFile {
        fn as_ref(&self) -> &Path {
            &self.0
        }
    }

    impl Display for TmpFile {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            self.0.fmt(f)
        }
    }

    impl TmpFile {
        pub fn new() -> Self {
            let dir =
                TempDir::new(JAM_SHELL_DIR_NAME).expect("failed to create temporary directory");
            TmpFile(dir.into_path().join("test.txt"))
        }
    }
}
