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

fn main() -> anyhow::Result<()> {
    let config_path = "./rsrc/simple.yaml";
    let version = env!("CARGO_PKG_VERSION");
    let cwd = std::env::current_dir()
        .expect("failed to get current working directory")
        .to_string_lossy()
        .to_string();
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::CompactFormat::new(decorator).build();
    let drain = std::sync::Mutex::new(drain).fuse();
    let logger = slog::Logger::root(
        drain,
        o!("config" => config_path, "version" => version, "cwd" => cwd),
    );

    let cfg_file = File::open(config_path)?;
    let cfg: Config = serde_yaml::from_reader(cfg_file)?;
    info!(logger, "parsed config");
    let desugared_cfg = cfg.desugar();
    info!(logger, "desugared config");

    let cfg_logger = logger.new(o!("reconciliation_strategy" => format!("{:#?}", desugared_cfg.options.reconciliation_strategy)));
    let cli = Cli::parse();
    let cli_logger = logger.new(o!("dry_run" => cli.dry_run, "shortcut" => "a-l"));
    info!(cli_logger, "parsed CLI flags");
    let executor = Executor::new();

    let jam = Jam::new(&cli_logger, executor, &desugared_cfg)?;
    info!(cli_logger, "fully initialized Jam structures");

    jam.execute(Shortcut(cli.shortcut))
}
