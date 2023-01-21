use std::fs::File;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct TargetCfg {
    name: String,
    // TODO: I believe, if we define custom serialization for this
    // field, we can make this non-Optional as long as we know
    // name. There may be conflicts, but those can be reconciled at a
    // time after serialization.  Plus, the reconciliation logic
    // likely gets simpler if it can work with String rather than
    // Option<String>.
    shortname: Option<String>,
    help: Option<String>,
    cmd: Option<String>,
    deps: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct Options {}

#[derive(Debug, Deserialize)]
struct Config {
    #[serde(flatten)]
    options: Options,
    targets: Vec<TargetCfg>,
}

fn main() -> anyhow::Result<()> {
    let cfg_file = File::open("./rsrc/simple.yaml")?;
    let cfg: Config = serde_yaml::from_reader(cfg_file)?;
    println!("{:#?}", cfg);

    return Ok(());
}