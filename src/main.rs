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

/// J̲am (isn't) A̲nother M̲ake.
/// A task runner.
#[derive(Parser, Debug)]
struct Cli {
    /// Show what jam _would_ do, but don't actually do it.
    #[clap(short, long, value_parser, default_value_t = false)]
    dry_run: bool,

    /// Adjusts the logging level.
    #[clap(short, long, value_enum)]
    log_level: Option<log::Level>,

    /// Individual keys that together give a shortcut, uniquely identifying a jam command to execute.
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

fn main() -> anyhow::Result<()> {
    let config_path = "./rsrc/simple.yaml";
    let cfg: Config = serde_yaml::from_reader(File::open(config_path)?)?;

    let desugared_cfg = cfg.desugar();

    let cli = Cli::parse();

    let logger = log::logger(
        cli.log_level
            .or(desugared_cfg.options.log_level)
            .unwrap_or(log::Level::Disabled),
        config_path,
    );
    debug!(logger, "desugared config"; &desugared_cfg);
    debug!(
        logger,
        "parsed CLI flags";
        &cli
    );

    let jam = Jam::new(&logger, Executor::new(), &desugared_cfg)?;
    info!(logger, "finished startup");

    let shortcut = tui::render(&jam)?;

    // TODO: We need to handle cli.shortcut vs. interactive shortcut,
    // right now we're ignoring CLI.
    jam.execute(shortcut)
}
