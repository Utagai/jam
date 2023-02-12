use std::fs::File;

use clap::Parser;

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
    let cfg_file = File::open("./rsrc/simple.yaml")?;
    let cfg: Config = serde_yaml::from_reader(cfg_file)?;
    let desugared_cfg = cfg.desugar();
    println!("{:#?}", desugared_cfg.options);
    // println!("{:#?}", desugared_cfg.targets);
    let cli = Cli::parse();
    let executor = Executor::new();

    let jam = Jam::new(executor, &desugared_cfg)?;

    jam.execute(Shortcut(cli.shortcut))
}
