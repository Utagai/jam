use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub(crate) struct TargetCfg {
    name: String,
    // TODO: I believe, if we define custom serialization for this
    // field, we can make this non-Optional as long as we know
    // name. There may be conflicts, but those can be reconciled at a
    // time after serialization.  Plus, the reconciliation logic
    // likely gets simpler if it can work with String rather than
    // Option<String>.
    shortname: Option<String>,
    help: Option<String>,
    // TODO: Should this still be optional even if we have linear task
    // definitions?
    cmd: Option<String>,
    targets: Option<Vec<TargetCfg>>,
    deps: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct Options {}

#[derive(Debug, Deserialize)]
pub(crate) struct Config {
    #[serde(flatten)]
    pub(crate) options: Options,
    pub(crate) targets: Vec<TargetCfg>,
}

// TODO: There is no validation of the config going on but there
// should be.
// e.g.:
// 1. task names must be alphanumeric + dashes + underscores, that's it?
