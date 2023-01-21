use std::fs::File;

use config::Config;
use jam::Jam;

mod config;
mod jam;

fn main() -> anyhow::Result<()> {
    let cfg_file = File::open("./rsrc/simple.yaml")?;
    let cfg: Config = serde_yaml::from_reader(cfg_file)?;
    println!("{:#?}", cfg);

    let jam = Jam::parse(cfg);

    return Ok(());
}
