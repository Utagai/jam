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

fn main() -> anyhow::Result<()> {
    let config_path = "./rsrc/simple.yaml";
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
    debug!(logger, "desugared config");

    let cli = Cli::parse();
    debug!(logger, "parsed CLI flags");

    let jam = Jam::new(&logger, Executor::new(), &desugared_cfg)?;
    info!(logger, "finished startup");

    jam.execute(Shortcut(cli.shortcut))
}
