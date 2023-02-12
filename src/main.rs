#![feature(iter_intersperse)]
use std::fs::File;

use clap::Parser;

use config::Config;
use jam::{Jam, Shortcut};
use slog::{debug, info, Record, KV};

use crate::executor::Executor;

mod config;
mod executor;
mod jam;
mod log;
mod reconciler;

/// J̲am (isn't) A̲nother M̲ake.
/// A task runner.
#[derive(Parser, Debug)]
struct Cli {
    /// Show what jam _would_ do, but don't actually do it.
    #[clap(short, long, value_parser, default_value_t = false)]
    dry_run: bool,

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
        )
    }
}

fn main() -> anyhow::Result<()> {
    let config_path = "./rsrc/simple.yaml";
    let startup_logger = log::logger(log::Level::Trace, config_path);
    debug!(startup_logger, "jam says hi!");

    let cfg: Config = serde_yaml::from_reader(File::open(config_path)?)?;
    debug!(startup_logger, "parsed config");

    let desugared_cfg = cfg.desugar();
    debug!(startup_logger, "desugared config"; &desugared_cfg);

    let cli = Cli::parse();
    debug!(
        startup_logger,
        "parsed CLI flags";
        &cli
    );

    let jam = Jam::new(&startup_logger, Executor::new(), &desugared_cfg)?;
    info!(startup_logger, "finished startup");

    jam.execute(Shortcut(cli.shortcut))
}
