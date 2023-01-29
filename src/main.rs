use std::fs::File;

use clap::Parser;

use config::Config;
use jam::Jam;

mod config;
mod jam;

// TODO: This clap help text is using unicode characters. Is there a
// way to programmatically determine if the host terminal supports
// them? If so, we should be able to detect that and then branch off
// to a different, non-unicode variant. Either through a parallel
// struct or maybe some method of overriding the help function via
// some setting in clap.

/// J̲am (isn't) A̲nother M̲ake.
/// A task runner.
#[derive(Parser, Debug)]
struct Cli {
    /// Show what jam _would_ do, but don't actually do it.
    #[clap(short, long, value_parser, default_value_t = false)]
    dry_run: bool,

    /// Individual identifiers that together give a chord, uniquely identifying a jam command to execute.
    notes: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    let cfg_file = File::open("./rsrc/simple.yaml")?;
    let cfg: Config = serde_yaml::from_reader(cfg_file)?;
    let cli = Cli::parse();
    println!("{:#?}", cli);
    println!("{:#?}", cfg);

    let jam = Jam::parse(&cfg);

    return Ok(());
}
