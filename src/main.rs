use std::fs::File;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Target {
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
    targets: Option<Vec<Target>>,
}

#[derive(Debug, Deserialize)]
struct Options {}

#[derive(Debug, Deserialize)]
struct Config {
    #[serde(flatten)]
    options: Options,
    targets: Vec<Target>,
}

fn main() -> anyhow::Result<()> {
    println!("Hello, world!");
    let f = File::open("./rsrc/simple.yaml")?;
    let yaml_s: Config = serde_yaml::from_reader(f)?;
    println!("{:#?}", yaml_s);

    return Ok(());
}
