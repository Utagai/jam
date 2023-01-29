use anyhow::Result;
use serde::Deserialize;

/// DesugaredTargetCfg is basically just a TargetCfg, but it has been
/// re-written and simplified so that the parsing logic can be
/// significantly easier. TargetCfg by itself is sufficient, but
/// desugaring some of the things implicitly encoded in it makes our
/// job significantly easier.
/// In particular, desugaring leads to doing the following things:
///		- Autogenerating chords if not specified. This does not care
///		 about conflicts.
///		- Changing names of subtargets to be prefixed by their parent's
///		 names. This is done recursively (e.g. the parent itself may be
///		 prefixed).
///		- Subtarget sections are flattened (post-prefixing) into deps.
/// TODO: The process of desugaring effectively copies a lot of
/// TargetCfg into DesugaredTargetCfg. This could be inefficient, but
/// I don't know yet and don't intend to do anything that could be
/// considered premature optimization. At some point though, I think a
/// simple benchmark off of a gigantic (randomly generated) YAML would
/// be a good idea. If it ends up being particularly slow, we can
/// change to mutating TargetCfg in-place and letting the parsing
/// logic complexity increase.
#[derive(Debug)]
pub struct DesugaredTargetCfg {
    pub name: String,
    pub chord_str: String, // TODO: Maybe Vec<String> or maybe even Chord?
    pub help: String,
    pub cmd: Option<String>,
    pub deps: Vec<String>,
}

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
    #[serde(rename = "chord")]
    pub chord_str: Option<String>,
    pub help: Option<String>,
    // TODO: Should this still be optional even if we have linear task
    // definitions?
    pub cmd: Option<String>,
    pub targets: Option<Vec<TargetCfg>>,
    pub deps: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
pub struct Options {}

// TODO: Is 'desugaring' the word we are actually looking for?
#[derive(Debug)]
pub struct DesugaredConfig {
    pub options: Options,
    pub targets: Vec<DesugaredTargetCfg>,
}

#[derive(Debug, Deserialize)]
pub struct Config {
    #[serde(flatten)]
    pub options: Options,
    pub targets: Vec<TargetCfg>,
}

impl Config {
    // TODO: Do we even need Result? I don't think we ever fail?
    pub fn desugar(self) -> Result<DesugaredConfig> {
        Ok(DesugaredConfig {
            options: Options {},
            targets: self
                .targets
                .into_iter()
                .map(|t| Self::desugar_target(t, String::from(""))) // TODO: Avoid String::from()?
                .collect::<Result<Vec<Vec<DesugaredTargetCfg>>>>()?
                .into_iter()
                .flatten()
                .collect(),
        })
    }

    fn desugar_target(sugar: TargetCfg, prefix: String) -> Result<Vec<DesugaredTargetCfg>> {
        let realized_name = if prefix.len() > 0 {
            format!("{}-{}", prefix, sugar.name)
        } else {
            sugar.name.clone() // TODO: clone
        };
        let chord_str = Self::name_to_short(&realized_name);
        let desugared = DesugaredTargetCfg {
            name: realized_name,
            chord_str: sugar.chord_str.unwrap_or(chord_str),
            help: sugar.help.unwrap_or("no help provided".to_string()),
            cmd: sugar.cmd,
            deps: sugar.deps.unwrap_or(vec![]),
        };
        // This will hold all the subtargets underneath the current
        // target. We are effectively flattening the config here as
        // part of our desugaring process.
        let mut desugared_targets = vec![desugared];
        if let Some(targets) = sugar.targets {
            for target in targets {
                // TODO: Investigate the prefix.clone() call below.
                let desugared_subtargets =
                    Config::desugar_target(target, prefix.clone() + &sugar.name)?;
                // Note that the first element is always the root
                // element of the recursion, so the first element of
                // desugared_subtargets is going to be the desugared
                // target. So, update the deps of the current parent
                // as well.
                desugared_targets[0]
                    .deps
                    .push(desugared_subtargets[0].name.clone()); // TODO: clone
                                                                 // Extend the desugared targets.
                desugared_targets.extend(desugared_subtargets);
            }
        }
        return Ok(desugared_targets);
    }

    // TODO: This duplicates the logic in Chord.
    fn name_to_short<T: AsRef<str>>(name: T) -> String {
        // TODO: Eventually, this delimiter should be configured.
        name.as_ref()
            .split("-")
            .map(|segment| segment.chars().nth(0).unwrap().to_string())
            .collect::<Vec<String>>()
            .join("-")
    }

    fn get_all_deps(sugar: TargetCfg) -> Vec<String> {
        let mut all_deps = vec![];
        if let Some(deps) = sugar.deps {
            all_deps.extend(deps);
        }

        if let Some(targets) = sugar.targets {
            for target in targets {
                all_deps.push(target.name);
            }
        }

        return all_deps;
    }
}
