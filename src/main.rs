use std::fs::File;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Blah {
    foo: i32,
    bar: i32,
}

#[derive(Debug, Deserialize)]
struct SimpleYAML {
    doe: String,
    french_hens: i32,
    calling_birds: Vec<String>,
    blah: Blah,
}

fn main() -> anyhow::Result<()> {
    println!("Hello, world!");
    let f = File::open("./rsrc/simple.yaml")?;
    let yaml_s: SimpleYAML = serde_yaml::from_reader(f)?;
    println!("{:#?}", yaml_s);

    return Ok(());
}
