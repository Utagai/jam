use anyhow::Result;

const DEFAULT_SHELL: &str = "sh";
const DEFAULT_EXEC_FLAG: &str = "-c";

pub fn execute(cmd: &str) -> Result<bool> {
    execute_with_flag(cmd, DEFAULT_EXEC_FLAG)
}

pub fn execute_with_flag(cmd: &str, exec_flag: &str) -> Result<bool> {
    let shell = std::env::var("SHELL").map_or(String::from(DEFAULT_SHELL), |oss| oss);
    let status = std::process::Command::new(shell)
        .arg(exec_flag)
        .arg(cmd)
        .status()?;
    Ok(status.success())
}

pub struct Output {
    pub stdout: String,
    pub success: bool,
}

pub fn execute_with_output(cmd: &str) -> Result<Output> {
    execute_with_flag_and_output(cmd, DEFAULT_EXEC_FLAG)
}

fn execute_with_flag_and_output(cmd: &str, exec_flag: &str) -> Result<Output> {
    let shell = std::env::var("SHELL").map_or(String::from(DEFAULT_SHELL), |oss| oss);
    let output = std::process::Command::new(shell)
        .arg(exec_flag)
        .arg(cmd)
        .output()?;
    Ok(Output {
        stdout: String::from_utf8(output.stdout)?,
        success: output.status.success(),
    })
}
