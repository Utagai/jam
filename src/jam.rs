use std::collections::HashMap;

use anyhow::{anyhow, bail};
use daggy::{Dag, NodeIndex, Walker};
use radix_trie::{Trie, TrieCommon, TrieKey};
use slog::{debug, info, o};
use thiserror::Error;

use crate::{
    config::{DesugaredConfig, DesugaredTargetCfg, Options},
    executor::{ExecuteKind, Executor},
    reconciler::reconcile,
};

struct Target<'a> {
    name: &'a str,
    shortcut: Shortcut,
    help: &'a str,
    cmd: Option<&'a str>,
    execute_kind: ExecuteKind,
}

impl<'a> From<&'a DesugaredTargetCfg> for Target<'a> {
    fn from(cfg: &'a DesugaredTargetCfg) -> Self {
        return Target {
            name: &cfg.name,
            shortcut: Shortcut::from_shortcut_str(&cfg.shortcut_str),
            help: &cfg.help,
            cmd: cfg.cmd.as_deref(),
            execute_kind: cfg.execute_kind,
        };
    }
}

type IdxT = u32;
type NodeIdx = NodeIndex<IdxT>;
pub type ShortcutTrie = Trie<Shortcut, Vec<NodeIdx>>;

pub struct Jam<'a> {
    opts: &'a Options,
    executor: Executor,
    dag: Dag<Target<'a>, IdxT>,
    shortcuts: ShortcutTrie,
    logger: slog::Logger,
}

#[derive(Eq, Debug)]
pub struct Shortcut(pub Vec<char>);

impl Shortcut {
    pub fn empty() -> Shortcut {
        Shortcut(vec![])
    }

    fn from_shortcut_str(shortcut_str: &str) -> Shortcut {
        Shortcut(
            shortcut_str
                .split('-')
                .map(|s| s.chars().nth(0).unwrap())
                .collect(),
        )
    }

    // NOTE: This is not very efficient. It basically clones the shortcut
    // then appends a key.  A more efficient approach is to implement
    // ShortcutView or ShortcutChain or idek, but something that just takes
    // a &Shortcut and &String and pretends like its the concatenation of
    // the two without actually doing the concatenation.
    pub fn append(&self, key: &char) -> Shortcut {
        let mut new_vec = self.0.clone();
        new_vec.push(*key);
        Shortcut(new_vec)
    }

    // NOTE: Same thing as described for append() above is worth considering here.
    pub fn tail(&self) -> (Shortcut, Option<char>) {
        let mut new_vec = self.0.clone();
        let key = new_vec.pop();
        (Shortcut(new_vec), key)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, idx: usize) -> Option<&char> {
        self.0.get(idx)
    }
}

impl From<&Self> for Shortcut {
    fn from(value: &Self) -> Self {
        Shortcut(value.0.clone())
    }
}

impl PartialEq for Shortcut {
    fn eq(&self, other: &Self) -> bool {
        self.0
            .iter()
            .zip(other.0.iter())
            .fold(true, |acc, (s, o)| acc && s == o)
    }
}

impl std::fmt::Display for Shortcut {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().collect::<String>())
    }
}

impl Clone for Shortcut {
    fn clone(&self) -> Self {
        Shortcut(self.0.to_vec())
    }
}

impl TrieKey for Shortcut {
    fn encode_bytes(&self) -> Vec<u8> {
        self.0
            .iter()
            .flat_map(|ch| {
                let mut buf: [u8; 4] = [0; 4];
                let s = ch.encode_utf8(&mut buf);
                s.encode_bytes()
            })
            .collect()
    }
}

#[derive(Error, Debug)]
pub enum ExecError {
    #[error("given shortcut '{shortcut}' is ambiguous (i.e. is it {conflict_msg}?)")]
    Ambiguous {
        shortcut: Shortcut,
        conflict_msg: String,
    },
    #[error("no command for given shortcut '{shortcut}'")]
    NotFound { shortcut: Shortcut },
    #[error("target '{name}' has no executable function")]
    CannotExec { name: String },
    #[error("{description}")]
    Reconciliation { description: String },
    #[error("{description}")]
    Executor { description: String },
}

// ParseResult is not really very useful at the moment. However, it
// may be something that we wish to embellish further in the
// future. This type can allow us to do that and save a bit of
// keystrokes. More importantly though, it lets us split the Jam API
// into two distinct classes errors: parsing errors & execution
// errors.
type ParseResult<T> = anyhow::Result<T, anyhow::Error>;
type ExecResult<T> = Result<T, ExecError>;

impl<'a> Jam<'a> {
    pub fn new(
        logger: &slog::Logger,
        executor: Executor,
        cfg: &'a DesugaredConfig,
    ) -> ParseResult<Jam<'a>> {
        let mut dag: Dag<Target, IdxT> = Dag::new();
        let mut deps: Vec<(&str, &str)> = Vec::new();
        let mut node_idxes: HashMap<&str, NodeIdx> = HashMap::new();
        let mut trie: ShortcutTrie = Trie::new();

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        for target_cfg in &cfg.targets {
            Self::validate_target_cfg(target_cfg, &node_idxes)?;

            let target = Target::from(target_cfg);
            // NOTE: I don't remember why, but I recall finding that
            // this .clone() is mostly necessary if we want to avoid
            // losing readability. Also possible is of course that I'm
            // not good enough at rust to see how to avoid it.
            let target_shortcut = target.shortcut.clone();

            // Keep track of the dep links. Once all the targets have
            // been visited, we can then wire up their edges in the
            // DAG. We must do this separately because at the time of
            // visiting, we cannot guarantee that the target this
            // target is depending on has been visited yet.
            for dep in &target_cfg.deps {
                deps.push((target.name, dep));
            }

            // Add this target as a node to the DAG.
            let node_idx = dag.add_node(target);

            // And add it to the trie under its shortcut.
            // A natural question to ask here is about conflicts
            // of shortcuts. For example, if two targets both have
            // the shortcut `t-a`, how can we disambiguate them? The
            // approach we take here is actually laziness. The
            // trie actually maps `t-a` to _two_ different targets
            // in this case, and the idea is that at 'runtime',
            // when the user has keyed in `t-a`, we will detect a
            // vector of length > 1, and _at that time_ we will
            // find their common prefix and use the remaining
            // characters (or '.') to disambiguate. This is
            // actually going to be more efficient, because doing
            // the reconciliation here pays the cost on _every_
            // jam startup, but the lazy approach only does it if
            // the user is going to use an ambiguous shortcut, which
            // may only be for very rare targets!
            trie.map_with_default(
                target_shortcut,
                |idxes| idxes.push(node_idx),
                vec![node_idx],
            );

            // And at last, record the mapping of target name to
            // its node index.
            node_idxes.insert(&target_cfg.name, node_idx);
        }

        // With their dependencies recorded, now add edges to the DAG
        // to represent them.
        // While doing this, we will catch cases where the dep isn't
        // recorded in our node index map, which means it doesn't
        // exist, as well as cases where adding the dep to the DAG
        // triggers a cycle.
        for dep in &deps {
            let dependee_idx = node_idxes
                .get(dep.0)
                .ok_or(anyhow!("reference to nonexistent dep: {}", dep.0))?;
            let dependent_idx = node_idxes
                .get(dep.1)
                .ok_or(anyhow!("reference to nonexistent dep: {}", dep.1))?;
            dag.add_edge(*dependee_idx, *dependent_idx, 0)
                .map_err(|_| anyhow!("'{}' -> '{}' creates a cycle", dep.0, dep.1))?;
            debug!(logger, "linked dependency"; o!("from" => dep.0, "to" => dep.1));
        }

        debug!(logger, "finished initializing structures"; o!("num_targets" => node_idxes.len(), "dep_links" => deps.len()));
        Ok(Jam {
            // NOTE: If we ever end up having a distinction between
            // config-time options vs. run-time options, we may want
            // to create a separate type here for isolation.
            opts: &cfg.options,
            executor,
            dag,
            shortcuts: trie,
            logger: logger.new(o!()),
        })
    }

    fn validate_target_cfg(
        cfg: &DesugaredTargetCfg,
        node_idxes: &HashMap<&str, NodeIdx>,
    ) -> ParseResult<()> {
        if cfg.name.is_empty() {
            bail!("cannot have an empty target name")
        } else if cfg.name.contains('.') {
            bail!("cannot have a '.' in a target name: '{}'", cfg.name)
        } else if cfg.name.contains('?') {
            bail!("cannot have a '?' in a target name: '{}'", cfg.name)
        } else if node_idxes.contains_key(&cfg.name as &str) {
            bail!("duplicate target name: '{}'", cfg.name)
        } else if cfg.deps.is_empty() && cfg.cmd.is_none() {
            bail!("a command without an executable command must have dependencies or subtargets, but '{}' does not", cfg.name)
        }

        Ok(())
    }

    pub fn next_keys(&self, prefix: &Shortcut) -> Vec<char> {
        let mut keys: Vec<char> = self
            .shortcuts
            .subtrie(&prefix)
            .unwrap()
            .keys()
            .filter_map(|k| k.get(prefix.len()).copied())
            .collect();
        // Because the keys we return are individual _characters_ of
        // _full_ sequences, it is possible to return duplicates
        // here. For example, consider the following shortcuts to
        // exist:
        //   a
        //   ab
        // The first call with the prefix being the empty shortcut will return [a,a] unless we de-dupe things.
        keys.sort_unstable();
        keys.dedup();
        return keys;
    }

    pub fn lookup(&self, shortcut: &Shortcut) -> bool {
        if let Ok(idxes) = self.get_idxes(shortcut) {
            return idxes.len() == 1;
        }

        return false;
    }

    fn execute_target(&self, logger: &slog::Logger, nidx: NodeIdx, depth: usize) -> ExecResult<()> {
        let target = &self.dag[nidx];
        info!(
            logger,
            "{}", if depth <= 0 {"executing target"} else {"executing dependency"};
            o!("name" => target.name, "depth" => depth)
        );
        let deps = self.dag.children(nidx);
        let mut num_deps_execed = 0;
        for dep in deps.iter(&self.dag) {
            let dep_logger = logger.new(o!("parent" => target.name.to_string()));
            self.execute_target(&dep_logger, dep.1, depth + 1)?;
            num_deps_execed += 1;
        }
        if let Some(cmd) = target.cmd {
            info!(logger, "running executor"; o!("cmd" => cmd));
            match self.executor.execute(target.execute_kind, cmd) {
                Ok(_) => {
                    info!(logger, "successfully executed target");
                }
                Err(err) => {
                    info!(logger, "failed to execute target");
                    return Err(ExecError::Executor {
                        description: err.to_string(),
                    });
                }
            }
        } else if num_deps_execed <= 0 {
            return Err(ExecError::CannotExec {
                name: target.name.to_string(),
            });
        }
        Ok(())
    }

    fn get_idxes(&self, shortcut: &Shortcut) -> ExecResult<Vec<NodeIdx>> {
        let shortcuts_ref = &self.shortcuts;
        let target_idxes;
        match shortcuts_ref.get(shortcut) {
            Some(nidxes) => target_idxes = nidxes.to_vec(), // TODO: Avoid copy?
            None => {
                debug!(self.logger, "found ambiguity, attempting reconciliation");
                // In this case, it is possible that the user has
                // actually specified a reconciliation character,
                // which won't exist in the trie until we
                // reconcile. So let's do that.
                let (tail, key) = shortcut.tail();
                // To see if this is correct, first, we expect the tail to exist:
                match shortcuts_ref.get(&tail) {
                    Some(nidxes) => {
                        // Secondly, not only must it exist, but it
                        // must be referring to a shortcut that has
                        // conflicts. If it has none, then the user
                        // should just specify the tail.
                        if nidxes.len() <= 1 {
                            return Err(ExecError::NotFound {
                                shortcut: Shortcut::from(shortcut),
                            });
                        }

                        info!(self.logger, "reconciling with strategy"; o!("strategy" => self.opts.reconciliation_strategy.to_string()));

                        // If it does indeed have a conflict,
                        // then let's make sure that the specified
                        // reconciliation key is a valid one:
                        let reconciliation_keys = match reconcile(
                            self.opts.reconciliation_strategy,
                            &self.shortcuts,
                            &nidxes.iter().map(|nidx| self.dag[*nidx].name).collect(),
                            &shortcut,
                        ) {
                            Ok(keys) => keys,
                            Err(err) => {
                                return Err(ExecError::Reconciliation {
                                    description: err.to_string(),
                                });
                            }
                        };

                        // The returned reconciliation keys are 1:1
                        // with the node indexes. i.e., the
                        // reconciliation key at index i is the
                        // reconciliation key for the conflicting
                        // target at index i in nidxes.
                        let nidx = match reconciliation_keys
                            .iter()
                            .position(|reconciliation_key| Some(reconciliation_key) == key.as_ref())
                        {
                            // If it isn't there, then this reconciliation
                            // key is not a good one and we should error.
                            None => {
                                return Err(ExecError::NotFound {
                                    shortcut: Shortcut::from(shortcut),
                                });
                            }
                            // However, and finally, if it is good, then
                            // let's identify which nidx we are looking at
                            // and return just that one:
                            Some(pos) => nidxes[pos],
                        };

                        debug!(self.logger, "reconciled");
                        target_idxes = vec![nidx];
                    }
                    None => {
                        return Err(ExecError::NotFound {
                            shortcut: Shortcut::from(shortcut),
                        });
                    }
                }
            }
        };
        Ok(target_idxes)
    }

    pub fn execute(&self, shortcut: Shortcut) -> ExecResult<()> {
        info!(self.logger, "executing");
        let target_idxes = self.get_idxes(&shortcut)?;
        if target_idxes.len() > 1 {
            return Err(ExecError::Ambiguous {
                shortcut,
                conflict_msg: target_idxes
                    .iter()
                    .map(|idx| String::from(self.dag[*idx].name))
                    .collect::<Vec<String>>()
                    .join(" or "),
            });
        }
        if let Some(nidx) = target_idxes.first() {
            self.execute_target(&self.logger, *nidx, 0)
        } else {
            Err(ExecError::NotFound { shortcut })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::target;
    use radix_trie::TrieCommon;

    mod parse {
        use super::*;

        use crate::config::{Config, TargetCfg};
        use slog::*;
        use slog_term::TestStdoutWriter;

        use crate::config::Options;
        use crate::log::Level;

        impl<'a> Jam<'a> {
            fn node_has_target(&self, target_name: &str) -> Option<NodeIdx> {
                self.shortcuts
                    .iter()
                    .flat_map(|kv| kv.1.clone())
                    .find(|nidx| self.dag[*nidx].name == target_name)
            }

            fn get_target_by_name(&self, target_name: &str) -> Option<&Target> {
                self.shortcuts
                    .iter()
                    .flat_map(|kv| kv.1.clone())
                    .map(|nidx| &self.dag[nidx])
                    .find(|target| target.name == target_name)
            }

            fn get_targets_by_shortcut(&self, shortcut: Shortcut) -> Option<&Vec<NodeIdx>> {
                self.shortcuts.get(&shortcut)
            }

            fn has_target(&self, target_name: &str) -> bool {
                self.get_target_by_name(target_name).is_some()
            }

            fn has_dep(&self, dependee: &str, dependent: &str) -> bool {
                let depender_idx = self.node_has_target(dependee);
                let dependent_idx = self.node_has_target(dependent);
                if match depender_idx.zip(dependent_idx) {
                    Some((depender_idx, dependent_idx)) => {
                        self.dag.find_edge(depender_idx, dependent_idx).is_some()
                    }
                    None => false,
                } {
                    return true;
                }

                false
            }
        }

        fn test_logger() -> slog::Logger {
            let decorator = slog_term::PlainSyncDecorator::new(TestStdoutWriter);
            let drain = slog_term::CompactFormat::new(decorator).build();
            let drain = std::sync::Mutex::new(drain).fuse();
            slog::Logger::root(drain, o!())
        }

        fn get_jam(cfg: &DesugaredConfig) -> Jam {
            Jam::new(&test_logger(), Executor::new(), cfg).expect("expected no errors from parsing")
        }

        fn check_jam_err(targets: Vec<TargetCfg>, expected_err: &str) {
            let cfg = Config {
                options: Options {
                    reconciliation_strategy: crate::reconciler::Strategy::Error,
                    log_level: Some(Level::Disabled),
                },
                targets,
            }
            .desugar();
            if let Err(err) = Jam::new(&test_logger(), Executor::new(), &cfg) {
                assert_eq!(format!("{err}").trim(), expected_err)
            } else {
                panic!("expected an error from parsing, but got none")
            }
        }

        fn verify_jam_dag(jam: Jam, targets: &Vec<&str>, deps: &Vec<(&str, &str)>) {
            for target in targets {
                assert!(jam.has_target(target));
            }

            for dep in deps {
                assert!(jam.has_dep(dep.0, dep.1));
            }

            assert_eq!(jam.dag.node_count(), targets.len());
            assert_eq!(jam.dag.edge_count(), deps.len());
        }

        mod nodeps {
            use super::*;

            #[test]
            fn single_target() {
                let expected_target_name = "foo";
                let cfg = Config::with_targets(vec![target::lone(expected_target_name)]);
                let jam = get_jam(&cfg);
                verify_jam_dag(jam, &vec![expected_target_name], &vec![]);
            }

            #[test]
            fn zero_targets() {
                let cfg = Config::with_targets(vec![]);
                let jam = get_jam(&cfg);
                verify_jam_dag(jam, &vec![], &vec![]);
            }

            #[test]
            fn multiple_targets() {
                let expected_target_names = vec!["foo", "bar", "quux"];
                let cfg = Config::with_targets(
                    expected_target_names
                        .iter()
                        .map(|name| target::lone(name))
                        .collect(),
                );
                let jam = get_jam(&cfg);
                expected_target_names
                    .iter()
                    .for_each(|name| assert!(jam.has_target(name)));
                verify_jam_dag(jam, &expected_target_names, &vec![]);
            }
        }

        mod deps {
            use super::*;

            #[test]
            fn single_dependency() {
                let depcfg = Config::with_targets(vec![
                    target::lone("bar"),
                    target::dep("foo", vec!["bar"]),
                ]);
                let depjam = get_jam(&depcfg);
                let subcfg =
                    Config::with_targets(vec![target::sub("foo", vec![target::lone("bar")])]);
                let subjam = get_jam(&subcfg);
                verify_jam_dag(depjam, &vec!["foo", "bar"], &vec![("foo", "bar")]);
                verify_jam_dag(subjam, &vec!["foo", "foo-bar"], &vec![("foo", "foo-bar")]);
            }

            #[test]
            fn one_target_two_dependents() {
                let depcfg = Config::with_targets(vec![
                    target::lone("bar"),
                    target::lone("quux"),
                    target::dep("foo", vec!["bar", "quux"]),
                ]);
                let depjam = get_jam(&depcfg);

                let subcfg = Config::with_targets(vec![target::sub(
                    "foo",
                    vec![target::lone("bar"), target::lone("quux")],
                )]);
                let subjam = get_jam(&subcfg);
                verify_jam_dag(
                    depjam,
                    &vec!["foo", "bar", "quux"],
                    &vec![("foo", "bar"), ("foo", "quux")],
                );
                verify_jam_dag(
                    subjam,
                    &vec!["foo", "foo-bar", "foo-quux"],
                    &vec![("foo", "foo-bar"), ("foo", "foo-quux")],
                );
            }

            #[test]
            fn single_dependency_but_dependee_defined_first() {
                let cfg = Config::with_targets(vec![
                    target::dep("foo", vec!["bar"]),
                    target::lone("bar"),
                ]);
                let jam = get_jam(&cfg);
                verify_jam_dag(jam, &vec!["foo", "bar"], &vec![("foo", "bar")]);
            }

            #[test]
            fn two_targets_one_dep_each() {
                let depcfg = Config::with_targets(vec![
                    target::lone("c"),
                    target::lone("d"),
                    target::dep("a", vec!["c"]),
                    target::dep("b", vec!["d"]),
                ]);
                let depjam = get_jam(&depcfg);

                let subcfg = Config::with_targets(vec![
                    target::sub("a", vec![target::lone("c")]),
                    target::sub("b", vec![target::lone("d")]),
                ]);
                let subjam = get_jam(&subcfg);
                verify_jam_dag(
                    depjam,
                    &vec!["a", "b", "c", "d"],
                    &vec![("a", "c"), ("b", "d")],
                );
                verify_jam_dag(
                    subjam,
                    &vec!["a", "b", "a-c", "b-d"],
                    &vec![("a", "a-c"), ("b", "b-d")],
                );
            }

            #[test]
            fn two_targets_share_a_dep() {
                let depcfg = Config::with_targets(vec![
                    target::lone("c"),
                    target::dep("a", vec!["c"]),
                    target::dep("b", vec!["c"]),
                ]);
                let depjam = get_jam(&depcfg);

                let subcfg = Config::with_targets(vec![
                    target::sub("a", vec![target::lone("c")]),
                    target::dep("b", vec!["a-c"]),
                ]);
                let subjam = get_jam(&subcfg);
                verify_jam_dag(depjam, &vec!["a", "b", "c"], &vec![("a", "c"), ("b", "c")]);
                verify_jam_dag(
                    subjam,
                    &vec!["a", "b", "a-c"],
                    &vec![("a", "a-c"), ("b", "a-c")],
                );
            }
        }

        mod errors {
            use super::*;

            #[test]
            fn immediate_cycle() {
                check_jam_err(
                    vec![
                        target::dep("baz", vec!["foo"]),
                        target::dep("foo", vec!["baz"]),
                    ],
                    "'foo' -> 'baz' creates a cycle",
                );
            }

            #[test]
            fn long_cycle() {
                check_jam_err(
                    vec![
                        target::dep("baz", vec!["foo"]),
                        target::dep("foo", vec!["corge"]),
                        target::dep("corge", vec!["quux"]),
                        target::dep("quux", vec!["baz"]),
                    ],
                    "'quux' -> 'baz' creates a cycle",
                );
            }

            #[test]
            fn dep_that_dne() {
                check_jam_err(
                    vec![
                        target::dep("baz", vec!["foo"]),
                        target::dep("foo", vec!["I DO NOT EXIST"]),
                    ],
                    "reference to nonexistent dep: I DO NOT EXIST",
                );
            }

            #[test]
            fn no_sub_or_deps_or_cmd() {
                check_jam_err(
                        vec![TargetCfg {
                            name: String::from("foo"),
                            shortcut_str: None,
                            help: None,
                            cmd: None,
                            targets: None,
                            deps: None,
														execute_kind: None,
                        }],
                        "a command without an executable command must have dependencies or subtargets, but 'foo' does not",
                    )
            }

            mod duped_target_name {
                use super::*;

                #[test]
                fn same_level() {
                    check_jam_err(
                        vec![target::lone("foo"), target::lone("foo")],
                        "duplicate target name: 'foo'",
                    );
                }

                #[test]
                fn diff_level() {
                    check_jam_err(
                        vec![
                            target::sub("foo", vec![target::lone("bar")]),
                            target::lone("foo-bar"),
                        ],
                        "duplicate target name: 'foo-bar'",
                    );
                }
            }

            mod target_name_validation {
                use super::*;

                #[test]
                fn empty_target_name() {
                    check_jam_err(vec![target::lone("")], "cannot have an empty target name");
                }

                #[test]
                fn target_name_with_period() {
                    check_jam_err(
                        vec![target::lone("pow.")],
                        "cannot have a '.' in a target name: 'pow.'",
                    );
                }

                #[test]
                fn target_name_with_question() {
                    check_jam_err(
                        vec![target::lone("pow?")],
                        "cannot have a '?' in a target name: 'pow?'",
                    );
                }
            }
        }
    }
}
