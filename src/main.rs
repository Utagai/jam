#![feature(iter_intersperse)]
use std::fs::File;

use clap::Parser;
use config::Config;
use slog::{debug, info, Record, KV};

use executor::Executor;
use jam::Jam;
use store::Shortcut;

mod config;
mod executor;
mod jam;
mod log;
mod reconciler;
mod shell;
mod store;
mod tui;

// NOTE: I wasn't able to figure out how to not have this be
// here. Ideally I could just chuck this into a module whose tests
// rely on this or something to that effect... but IDK...
// I think this just looks ugly, but shouldn't affect much.
#[cfg(test)]
mod testutils;

#[derive(Parser, Debug)]
#[command(about = "Jam (isn't) Another Make. A task runner.")]
struct Cli {
    /// Show what jam _would_ do, but don't actually do it.
    #[clap(short, long, value_parser, default_value_t = false)]
    dry_run: bool,

    /// Adjusts the logging level.
    #[clap(short, long, value_enum)]
    log_level: Option<log::Level>,

    #[clap(long)]
    log_path: Option<String>,

    #[clap(long, conflicts_with_all(["target_to_exec", "shortcut"]), default_value_t = false)]
    dump_mappings: bool,

    #[clap(short, long)]
    config_file: Option<String>,

    /// A target name to execute directly. Overrides other execution modes.
    #[clap(short, long)]
    target_to_exec: Option<String>,

    /// A series of characters that map to a shortcut sequence, leading to a target to execute.
    shortcut: Option<String>,
}

impl KV for Cli {
    fn serialize(&self, _: &Record, serializer: &mut dyn slog::Serializer) -> slog::Result {
        serializer.emit_bool("dry_run", self.dry_run)?;
        match self.log_level {
            Some(log_level) => serializer.emit_str("log_level", &log_level.to_string()),
            _ => Ok(()),
        }
    }
}

fn get_nearest_config_file(current_dir: &std::path::Path) -> anyhow::Result<String> {
    let mut path = current_dir.to_path_buf();
    let home_dir = dirs::home_dir().ok_or_else(|| anyhow::anyhow!("no home directory found"))?;
    // In typical execution, this should terminate the home directory. However,
    // we cannot technically control from _where_ a user actually runs this, so
    // the more graceful thing is to also exit if we somehow reach the root
    // directory.
    while path != home_dir && path != std::path::Path::new("/") {
        let possible_file_names = ["jam.yaml", "jam.yml", "jamfile.yml", "jamfile.yaml"];
        for file_name in possible_file_names.iter() {
            let config_path = path.join(file_name);
            if config_path.exists() {
                return Ok(config_path.to_str().unwrap().to_string());
            }
        }
        path = path.parent().unwrap().to_path_buf();
    }
    Err(anyhow::anyhow!("no jam file found"))
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let current_dir = std::env::current_dir()?;
    let config_path = match cli.config_file {
        Some(path) => Ok(path),
        None => match get_nearest_config_file(&current_dir) {
            Ok(path) => Ok(path),
            Err(err) => Err(err),
        },
    }?;

    let cfg: Config = serde_yaml::from_reader(File::open(&config_path)?)?;

    let desugared_cfg = cfg.desugar();

    let logger = log::logger(
        cli.log_level
            .or(desugared_cfg.options.log_level)
            .unwrap_or(log::Level::Disabled),
        cli.log_path.unwrap_or(String::from(log::DEFAULT_LOG_PATH)),
        config_path,
    );
    debug!(logger, "desugared config"; &desugared_cfg);

    let jam = Jam::new(&logger, Executor::new(), &desugared_cfg)?;
    info!(logger, "finished startup");

    if cli.dump_mappings {
        jam.mappings()?.iter().for_each(|(shortcut, target_name)| {
            println!("{} -> {}", shortcut, target_name);
        });
        return Ok(());
    }

    if let Some(ref target_name) = cli.target_to_exec {
        jam.execute_by_target_name(&target_name)?;
        return Ok(());
    }

    let shortcut = if let Some(ref shortcut) = cli.shortcut {
        Shortcut(shortcut.chars().collect())
    } else {
        tui::core::render(logger, &jam)?
    };

    if shortcut.len() > 0 {
        jam.execute_by_shortcut(shortcut)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nearest_config_file_current_directory() {
        // Create a temporary directory to isolate our tests.
        let temp_dir = tempfile::tempdir().unwrap();
        let current_dir = temp_dir.path();

        // Test when a config file exists in the current directory
        let config_file = current_dir.join("jam.yaml");
        std::fs::write(&config_file, "").unwrap();
        assert_eq!(
            get_nearest_config_file(&current_dir).unwrap(),
            config_file.to_str().unwrap()
        );

        // Delete the temporary directory
        temp_dir.close().unwrap();
    }

    #[test]
    fn nearest_config_file_parent_directory() {
        // Create a temporary directory to isolate our tests.
        let temp_dir = tempfile::tempdir().unwrap();
        let current_dir = temp_dir.path().join("test_dir");
        std::fs::create_dir(&current_dir).unwrap();

        // Test when a config file exists in a parent directory
        let parent_dir = current_dir.parent().unwrap().to_path_buf();
        let config_file = parent_dir.join("jam.yml");
        std::fs::write(&config_file, "").unwrap();
        assert_eq!(
            get_nearest_config_file(&current_dir).unwrap(),
            config_file.to_str().unwrap()
        );

        // Delete the temporary directory
        temp_dir.close().unwrap();
    }

    #[test]
    fn nearest_config_file_no_file() {
        // Create a temporary directory to isolate our tests.
        let temp_dir = tempfile::tempdir().unwrap();
        let current_dir = temp_dir.path();

        // Test when no config file exists
        assert!(get_nearest_config_file(&current_dir).is_err());

        // Delete the temporary directory
        temp_dir.close().unwrap();
    }
}
