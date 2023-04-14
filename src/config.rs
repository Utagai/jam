use serde::Deserialize;
use slog::KV;

use crate::{executor::ExecuteKind, log, reconciler};

/// DesugaredTargetCfg is basically just a TargetCfg, but it has been
/// re-written and simplified so that the parsing logic can be
/// significantly easier. TargetCfg by itself is sufficient, but
/// desugaring some of the things implicitly encoded in it makes our
/// job significantly easier.
/// In particular, desugaring leads to doing the following things:
/// * Autogenerating shortcuts if not specified. This does not care
///  about conflicts.
/// * Changing names of subtargets to be prefixed by their parent's
///  names. This is done recursively (e.g. the parent itself may be
///  prefixed).
/// * Subtarget sections are flattened (post-prefixing) into deps.
/// * Removing Option types and replacing them with guaranteed values
///  when possible.
#[derive(Debug, PartialEq)]
pub struct DesugaredTargetCfg {
    pub name: String,
    pub shortcut_str: String,
    pub help: String,
    pub cmd: Option<String>,
    pub deps: Vec<String>,
    pub execute_kind: ExecuteKind,
}

impl KV for DesugaredTargetCfg {
    fn serialize(&self, _: &slog::Record, serializer: &mut dyn slog::Serializer) -> slog::Result {
        serializer.emit_str("name", &self.name)?;
        serializer.emit_str("shortcut_str", &self.shortcut_str)?;
        serializer.emit_str("help", &self.help)?;
        match &self.cmd {
            Some(cmd_str) => serializer.emit_str("cmd", cmd_str)?,
            None => serializer.emit_unit("cmd")?,
        };
        let dep_str = format!("[{}]", self.deps.join(","));
        serializer.emit_str("deps", &dep_str)?;
        serializer.emit_str("execute_kind", &self.execute_kind.to_string())
    }
}

#[derive(Debug, Deserialize)]
pub struct TargetCfg {
    pub name: String,
    #[serde(rename = "shortcut")]
    pub shortcut_str: Option<String>,
    pub help: Option<String>,
    pub cmd: Option<String>,
    pub targets: Option<Vec<TargetCfg>>,
    pub deps: Option<Vec<String>>,
    pub execute_kind: Option<ExecuteKind>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Options {
    #[serde(default = "reconciler::Strategy::default")]
    pub reconciliation_strategy: reconciler::Strategy,

    pub log_level: Option<log::Level>,
}

impl KV for Options {
    fn serialize(&self, _: &slog::Record, serializer: &mut dyn slog::Serializer) -> slog::Result {
        serializer.emit_str(
            "reconciliation_strategy",
            &self.reconciliation_strategy.to_string(),
        )?;
        if let Some(log_level) = self.log_level {
            serializer.emit_str("log_level", &log_level.to_string())?;
        };

        Ok(())
    }
}

// NOTE: 'Desugaring' may not exactly be the right terminology here.
// But the overall idea is we are rewriting the config in a simpler
// but perhaps more verbose and/or less readable form. This form is
// better for machines, but the prior form is better for humans.
#[derive(Debug, PartialEq)]
pub struct DesugaredConfig {
    pub options: Options,
    pub targets: Vec<DesugaredTargetCfg>,
}

impl KV for DesugaredConfig {
    fn serialize(
        &self,
        record: &slog::Record,
        serializer: &mut dyn slog::Serializer,
    ) -> slog::Result {
        self.options.serialize(record, serializer)?;
        for target in &self.targets {
            target.serialize(record, serializer)?;
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
pub struct Config {
    pub options: Options,
    pub targets: Vec<TargetCfg>,
}

impl Config {
    pub fn desugar(self) -> DesugaredConfig {
        DesugaredConfig {
            options: self.options,
            targets: self
                .targets
                .into_iter()
                .map(|t| Self::desugar_target(t, ""))
                .collect::<Vec<Vec<DesugaredTargetCfg>>>()
                .into_iter()
                .flatten()
                .collect(),
        }
    }

    fn desugar_target<T: AsRef<str>>(sugar: TargetCfg, prefix: T) -> Vec<DesugaredTargetCfg> {
        let realized_name = if !prefix.as_ref().is_empty() {
            format!("{}-{}", prefix.as_ref(), sugar.name)
        } else {
            sugar.name
        };
        let shortcut_str = Self::name_to_short(&realized_name);
        let desugared = DesugaredTargetCfg {
            name: realized_name.clone(),
            shortcut_str: sugar.shortcut_str.unwrap_or(shortcut_str),
            help: sugar.help.unwrap_or("no help provided".to_string()),
            cmd: sugar.cmd,
            deps: sugar.deps.unwrap_or(vec![]),
            execute_kind: sugar.execute_kind.unwrap_or(ExecuteKind::Shell),
        };
        // This will hold all the subtargets underneath the current
        // target. We are effectively flattening the config here as
        // part of our desugaring process.
        let mut desugared_targets = vec![desugared];
        if let Some(targets) = sugar.targets {
            for target in targets {
                let desugared_subtargets = Config::desugar_target(target, &realized_name);
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
        desugared_targets
    }

    fn name_to_short<T: AsRef<str> + std::fmt::Display>(name: T) -> String {
        if name.as_ref().is_empty() {
            // This should fail validation, but it isn't our problem
            // right now.
            return String::from("");
        }
        name.as_ref()
            .split('-')
            .map(|segment| segment.chars().next().unwrap().to_string())
            .collect::<Vec<String>>()
            .join("-")
    }
}

#[cfg(test)]
pub mod target {
    use std::path::Path;

    use super::*;

    pub fn lone(name: &str) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            shortcut_str: None,
            help: None,
            cmd: Some(String::from("blah")),
            targets: None,
            deps: None,
            execute_kind: None,
        }
    }

    pub fn dep(name: &str, deps: Vec<&str>) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            shortcut_str: None,
            help: None,
            cmd: None,
            targets: None,
            deps: Some(deps.iter().map(|dep| String::from(*dep)).collect()),
            execute_kind: None,
        }
    }

    pub fn sub(name: &str, subs: Vec<TargetCfg>) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            shortcut_str: None,
            help: None,
            cmd: None,
            targets: Some(subs),
            deps: None,
            execute_kind: None,
        }
    }

    // Note that the given out_file will be effectively appended to
    // cmd as " > {out_file}". This is to aid in testing, by allowing
    // the tests to use out_file as a place where the output of the
    // command will go. As implied by the above description, this
    // means that this function will return a TargetCfg that has an
    // executes via shell.
    pub fn exec(name: &str, cmd: &str, out_file: &str) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            shortcut_str: None,
            help: None,
            cmd: Some(format!("{} > {}", cmd, out_file)),
            targets: None,
            deps: None,
            execute_kind: Some(ExecuteKind::Shell),
        }
    }
}

#[cfg(test)]
impl Config {
    pub fn with_targets(targets: Vec<TargetCfg>) -> DesugaredConfig {
        Config {
            options: Options {
                reconciliation_strategy: reconciler::Strategy::Error,
                log_level: Some(log::Level::Disabled),
            },
            targets,
        }
        .desugar()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    mod dstarget {
        use super::*;

        pub fn lone(name: &str, shortcut_str: &str) -> DesugaredTargetCfg {
            DesugaredTargetCfg {
                name: String::from(name),
                shortcut_str: String::from(shortcut_str),
                help: String::from("no help provided"),
                cmd: Some(String::from("blah")),
                deps: vec![],
                execute_kind: ExecuteKind::Shell,
            }
        }

        pub fn dep(name: &str, shortcut_str: &str, deps: Vec<&str>) -> DesugaredTargetCfg {
            DesugaredTargetCfg {
                name: String::from(name),
                shortcut_str: String::from(shortcut_str),
                help: String::from("no help provided"),
                cmd: None,
                deps: deps.iter().map(|s| s.to_string()).collect(),
                execute_kind: ExecuteKind::Shell,
            }
        }
    }

    impl DesugaredConfig {
        pub fn with_targets(targets: Vec<DesugaredTargetCfg>) -> DesugaredConfig {
            DesugaredConfig {
                options: Options {
                    reconciliation_strategy: reconciler::Strategy::Error,
                    log_level: Some(log::Level::Disabled),
                },
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

    mod shortcuts {
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
                shortcut_str: Some(String::from("x")),
                help: None,
                cmd: Some(String::from("blah")),
                targets: None,
                deps: None,
                execute_kind: None,
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
