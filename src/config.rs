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
#[derive(Debug, PartialEq)]
pub struct DesugaredTargetCfg {
    pub name: String,
    pub chord_str: String,
    pub help: String,
    pub cmd: Option<String>,
    pub deps: Vec<String>,
}

// TODO: Exercise - can we use &str in any of these fields?
#[derive(Debug, Deserialize)]
pub struct TargetCfg {
    pub name: String,
    #[serde(rename = "chord")]
    pub chord_str: Option<String>,
    pub help: Option<String>,
    pub cmd: Option<String>,
    pub targets: Option<Vec<TargetCfg>>,
    pub deps: Option<Vec<String>>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Options {}

// NOTE: 'Desugaring' may not exactly be the right terminology here.
// But the overall idea is we are rewriting the config in a simpler
// but perhaps more verbose and/or less readable form. This form is
// better for machines, but the prior form is better for humans.
#[derive(Debug, PartialEq)]
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
    pub fn desugar<'a>(self) -> Result<DesugaredConfig> {
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

    fn desugar_target<'a, T: AsRef<str>>(
        sugar: TargetCfg,
        prefix: T,
    ) -> Result<Vec<DesugaredTargetCfg>> {
        let realized_name = if prefix.as_ref().len() > 0 {
            format!("{}-{}", prefix.as_ref(), sugar.name)
        } else {
            sugar.name
        };
        let chord_str = Self::name_to_short(&realized_name);
        let desugared = DesugaredTargetCfg {
            name: realized_name.clone(),
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
                let desugared_subtargets = Config::desugar_target(target, &realized_name)?;
                // Note that the first element is always the root
                // element of the recursion, so the first element of
                // desugared_subtargets is going to be the desugared
                // target. So, update the deps of the current parent
                // as well.
                desugared_targets[0]
                    .deps
                    .push(desugared_subtargets[0].name.to_string());
                desugared_targets.extend(desugared_subtargets);
            }
        }
        return Ok(desugared_targets);
    }

    // TODO: This duplicates the logic in Chord.
    fn name_to_short<T: AsRef<str> + std::fmt::Display>(name: T) -> String {
        if name.as_ref().is_empty() {
            // This should fail validation, but it isn't our problem
            // right now.
            return String::from("");
        }
        // TODO: Eventually, this delimiter should be configured.
        name.as_ref()
            .split("-")
            .map(|segment| segment.chars().nth(0).unwrap().to_string())
            .collect::<Vec<String>>()
            .join("-")
    }
}

#[cfg(test)]
pub mod target {
    use super::*;

    pub fn lone(name: &str) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            chord_str: None,
            help: None,
            cmd: Some(String::from("blah")),
            targets: None,
            deps: None,
        }
    }

    pub fn dep(name: &str, deps: Vec<&str>) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            chord_str: None,
            help: None,
            cmd: None,
            targets: None,
            deps: Some(deps.iter().map(|dep| String::from(*dep)).collect()),
        }
    }

    pub fn sub(name: &str, subs: Vec<TargetCfg>) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            chord_str: None,
            help: None,
            cmd: None,
            targets: Some(subs),
            deps: None,
        }
    }
}

#[cfg(test)]
impl Config {
    pub fn with_targets(targets: Vec<TargetCfg>) -> DesugaredConfig {
        Config {
            options: Options {},
            targets,
        }
        .desugar()
        .expect("failed to desugar test config")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    mod dstarget {
        use super::*;

        pub fn lone(name: &str, chord_str: &str) -> DesugaredTargetCfg {
            DesugaredTargetCfg {
                name: String::from(name),
                chord_str: String::from(chord_str),
                help: String::from("no help provided"),
                cmd: Some(String::from("blah")),
                deps: vec![],
            }
        }

        pub fn dep(name: &str, chord_str: &str, deps: Vec<&str>) -> DesugaredTargetCfg {
            DesugaredTargetCfg {
                name: String::from(name),
                chord_str: String::from(chord_str),
                help: String::from("no help provided"),
                cmd: None,
                deps: deps.iter().map(|s| s.to_string()).collect(),
            }
        }
    }

    impl DesugaredConfig {
        pub fn with_targets(targets: Vec<DesugaredTargetCfg>) -> DesugaredConfig {
            DesugaredConfig {
                options: Options {},
                targets,
            }
        }
    }

    #[test]
    fn no_op_for_sugargree_cfg() {
        let cfg = Config::with_targets(vec![target::lone("foo")]);
        assert_eq!(
            cfg,
            DesugaredConfig::with_targets(vec![dstarget::lone("foo", "f")]),
        )
    }

    #[test]
    fn flattens_subtargets() {
        let cfg = Config::with_targets(vec![target::sub(
            "foo",
            vec![target::sub("bar", vec![target::lone("quux")])],
        )]);

        assert_eq!(
            DesugaredConfig::with_targets(vec![
                dstarget::dep("foo", "f", vec!["foo-bar"]),
                dstarget::dep("foo-bar", "f-b", vec!["foo-bar-quux"]),
                dstarget::lone("foo-bar-quux", "f-b-q")
            ]),
            cfg,
        )
    }

    #[test]
    fn flattens_subtargets_deep_and_wide_and_deps() {
        let cfg = Config::with_targets(vec![target::sub(
            "foo",
            vec![
                target::sub(
                    "bar",
                    vec![
                        target::sub(
                            "quux",
                            vec![target::sub("corge", vec![target::lone("lol")])],
                        ),
                        target::dep("blah", vec!["foo-bar-deep"]),
                    ],
                ),
                target::sub(
                    "deep",
                    vec![target::sub("deeper", vec![target::lone("deepest")])],
                ),
            ],
        )]);

        assert_eq!(
            DesugaredConfig::with_targets(vec![
                dstarget::dep("foo", "f", vec!["foo-bar", "foo-deep"]),
                dstarget::dep("foo-bar", "f-b", vec!["foo-bar-quux", "foo-bar-blah"]),
                dstarget::dep("foo-bar-quux", "f-b-q", vec!["foo-bar-quux-corge"]),
                dstarget::dep(
                    "foo-bar-quux-corge",
                    "f-b-q-c",
                    vec!["foo-bar-quux-corge-lol"]
                ),
                dstarget::lone("foo-bar-quux-corge-lol", "f-b-q-c-l"),
                dstarget::dep("foo-bar-blah", "f-b-b", vec!["foo-bar-deep"]),
                dstarget::dep("foo-deep", "f-d", vec!["foo-deep-deeper"]),
                dstarget::dep("foo-deep-deeper", "f-d-d", vec!["foo-deep-deeper-deepest"]),
                dstarget::lone("foo-deep-deeper-deepest", "f-d-d-d"),
            ]),
            cfg,
        )
    }

    #[test]
    fn does_not_expand_deps() {
        let cfg = Config::with_targets(vec![target::dep("foo", vec!["bar"]), target::lone("bar")]);
        assert_eq!(
            cfg,
            DesugaredConfig::with_targets(vec![
                dstarget::dep("foo", "f", vec!["bar"]),
                dstarget::lone("bar", "b")
            ])
        )
    }

    mod chords {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn automatic_of_single_word_is_first_char() {
            let cfg = Config::with_targets(vec![target::lone("foo")]);
            assert_eq!(
                cfg,
                DesugaredConfig::with_targets(vec![dstarget::lone("foo", "f")])
            )
        }

        #[test]
        fn automatic_of_double_word_is_first_char() {
            let cfg = Config::with_targets(vec![target::lone("foo-bar")]);
            assert_eq!(
                cfg,
                DesugaredConfig::with_targets(vec![dstarget::lone("foo-bar", "f-b")]),
            );
        }

        #[test]
        fn automatic_of_multi_word_is_first_char() {
            let cfg = Config::with_targets(vec![target::lone("foo-bar-baz-quux")]);
            assert_eq!(
                cfg,
                DesugaredConfig::with_targets(vec![dstarget::lone("foo-bar-baz-quux", "f-b-b-q")]),
            );
        }

        #[test]
        fn automatic_of_multiple_non_conflicting_targets() {
            let cfg = Config::with_targets(vec![
                target::lone("foo"),
                target::lone("bar"),
                target::lone("quux"),
            ]);
            assert_eq!(
                cfg,
                DesugaredConfig::with_targets(vec![
                    dstarget::lone("foo", "f"),
                    dstarget::lone("bar", "b"),
                    dstarget::lone("quux", "q"),
                ])
            );
        }

        #[test]
        fn automatic_of_multiple_conflicting_targets_no_reconciliation() {
            let cfg = Config::with_targets(vec![
                target::lone("bar"),
                target::lone("baz"),
                target::lone("bam"),
                target::lone("barr"),
            ]);
            assert_eq!(
                cfg,
                DesugaredConfig::with_targets(vec![
                    dstarget::lone("bar", "b"),
                    dstarget::lone("baz", "b"),
                    dstarget::lone("bam", "b"),
                    dstarget::lone("barr", "b")
                ]),
            );
        }

        #[test]
        fn override_respected() {
            let cfg = Config::with_targets(vec![TargetCfg {
                name: String::from("foo"),
                chord_str: Some(String::from("x")),
                help: None,
                cmd: Some(String::from("blah")),
                targets: None,
                deps: None,
            }]);
            assert_eq!(
                cfg,
                DesugaredConfig::with_targets(vec![dstarget::lone("foo", "x")]),
            );
        }

        #[test]
        fn desugared_names() {
            let cfg = Config::with_targets(vec![target::sub("foo", vec![target::lone("bar")])]);
            assert_eq!(
                cfg,
                DesugaredConfig::with_targets(vec![
                    dstarget::dep("foo", "f", vec!["foo-bar"]),
                    dstarget::lone("foo-bar", "f-b")
                ])
            )
        }
    }
}
