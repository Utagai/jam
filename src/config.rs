use serde::Deserialize;

// TODO: Exercise - can we use &str in any of these fields?
#[derive(Debug, Deserialize)]
pub struct TargetCfg {
    pub name: String,
    // TODO: I believe, if we define custom serialization for this
    // field, we can make this non-Optional as long as we know
    // name. There may be conflicts, but those can be reconciled at a
    // time after serialization.  Plus, the reconciliation logic
    // likely gets simpler if it can work with String rather than
    // Option<String>.
    pub chord: Option<String>,
    pub help: Option<String>,
    // TODO: Should this still be optional even if we have linear task
    // definitions?
    pub cmd: Option<String>,
    pub targets: Option<Vec<TargetCfg>>,
    pub deps: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
pub struct Options {}

#[derive(Debug, Deserialize)]
pub struct Config {
    #[serde(flatten)]
    pub options: Options,
    pub targets: Vec<TargetCfg>,
}
