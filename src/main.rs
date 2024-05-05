#![feature(iter_intersperse)]
use std::fs::File;

use clap::Parser;
use config::Config;
use slog::{debug, info, Record, KV};

use executor::Executor;
use jam::{Jam, Shortcut};

mod config;
mod executor;
mod jam;
mod log;
mod reconciler;
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

    #[clap(long, conflicts_with = "exec_arg", default_value_t = false)]
    dump_mappings: bool,

    #[clap(short, long)]
    config_file: Option<String>,

    /// First execution argument. If using a shortcut, this is just the first character. Otherwise, it's the name of the target to execute.
    exec_arg: Option<String>,

    /// Individual keys that together (with EXEC_ARG) give a shortcut, uniquely identifying a jam command to execute.
    shortcut: Vec<char>,
}

impl KV for Cli {
    fn serialize(&self, _: &Record, serializer: &mut dyn slog::Serializer) -> slog::Result {
        serializer.emit_bool("dry_run", self.dry_run)?;
        serializer.emit_str(
            "shortcut",
            // NOTE: I think this causes an allocation, and I feel like theoretically it may not be necessary.
            // But I also don't think there's use in prematurely optimizing something like this.
            &self.shortcut.iter().intersperse(&'-').collect::<String>(),
        )?;
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
                eprintln!("Found config file: {}", config_path.to_str().unwrap());
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

    if let Some(ref target_name) = cli.exec_arg {
        if target_name.len() > 1 {
            jam.execute_by_target_name(&target_name)?;
            return Ok(());
        }
    }

    let shortcut = if let Some(ref target_name) = cli.exec_arg {
        Shortcut(cli.shortcut).prepend(&target_name.chars().next().unwrap())
    } else {
        tui::core::render(logger, &jam)?
    };

    if shortcut.len() > 0 {
        jam.execute_by_shortcut(shortcut)?;
    }

    Ok(())
}
