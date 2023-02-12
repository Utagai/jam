#![feature(iter_intersperse)]
use std::{fs::File, os};

use clap::Parser;
use slog::*;

use config::Config;
use jam::{Jam, Shortcut};

use crate::executor::Executor;

mod config;
mod executor;
mod jam;
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
    fn serialize(&self, _: &Record, serializer: &mut dyn slog::Serializer) -> Result {
        serializer.emit_bool("dry_run", self.dry_run)?;
        serializer.emit_str(
            "shortcut",
            // NOTE: I think this causes an allocation, and I feel like theoretically it may not be necessary.
            // But I also don't think there's use in prematurely optimizing something like this.
            &self.shortcut.iter().intersperse(&'-').collect::<String>(),
        )
    }
}

fn logger(config_path: &'static str) -> slog::Logger {
    let version = env!("CARGO_PKG_VERSION");
    let cwd = std::env::current_dir()
        .expect("failed to get current working directory")
        .into_os_string()
        .into_string()
        .expect("failed to convert cwd OS string to string");

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build();
    let drain = std::sync::Mutex::new(drain).fuse();
    slog::Logger::root(
        drain,
        o!("config" => config_path, "version" => version, "cwd" => cwd),
    )
}

fn main() -> anyhow::Result<()> {
    let config_path = "./rsrc/simple.yaml";
    let logger = logger(config_path);
    debug!(logger, "jam says hi!");

    let cfg: Config = serde_yaml::from_reader(File::open(config_path)?)?;
    debug!(logger, "parsed config");

    let desugared_cfg = cfg.desugar();
    debug!(logger, "desugared config"; &desugared_cfg);

    let cli = Cli::parse();
    debug!(
        logger,
        "parsed CLI flags";
        &cli
    );

    let jam = Jam::new(&logger, Executor::new(), &desugared_cfg)?;
    info!(logger, "finished startup");

    jam.execute(Shortcut(cli.shortcut))
}
