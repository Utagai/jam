use std::{collections::HashMap, slice::Iter};

use anyhow::{anyhow, bail};
use daggy::{Dag, NodeIndex, Walker};
use sequence_trie::SequenceTrie;
use slog::{debug, info, o};

use crate::{
    config::{DesugaredConfig, DesugaredTargetCfg, Options},
    error::{ExecError, ExecResult},
    executor::{ExecuteKind, Executor},
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
pub type NodeIdx = NodeIndex<IdxT>;
// NOTE: I wonder if we actually really need a trie. I think the more general
// N-ary tree would work just as well given how we use this trie. We're not
// actually making good use of the strengths of a trie, I think. In either case,
// it shouldn't really change much in the code, even in terms of complexity, so
// I'm just going to keep it here until I find a good enough reason to refactor.
pub type ShortcutTrie = SequenceTrie<char, Vec<NodeIdx>>;

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
    // TODO: Why is this pub?
    pub fn empty() -> Shortcut {
        Shortcut(vec![])
    }

    pub fn from_shortcut_str(shortcut_str: &str) -> Shortcut {
        Shortcut(
            shortcut_str
                .split('-')
                .map(|s| s.chars().next().unwrap())
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

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn iter(&self) -> Iter<'_, char> {
        self.0.iter()
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
        write!(f, "{}", self.0.iter().intersperse(&'-').collect::<String>())
    }
}

impl Clone for Shortcut {
    fn clone(&self) -> Self {
        Shortcut(self.0.to_vec())
    }
}

// ParseResult is not really very useful at the moment. However, it
// may be something that we wish to embellish further in the
// future. This type can allow us to do that and save a bit of
// keystrokes. More importantly though, it lets us split the Jam API
// into two distinct classes errors: parsing errors & execution
// errors.
type ParseResult<T> = anyhow::Result<T, anyhow::Error>;

#[derive(Debug, PartialEq)]
pub enum Lookup {
    NotFound,
    Found,
    ReconciliationFailure(String),
    Conflict,
}

impl<'a> Jam<'a> {
    pub fn new(
        logger: &slog::Logger,
        executor: Executor,
        cfg: &'a DesugaredConfig,
    ) -> ParseResult<Jam<'a>> {
        let mut dag: Dag<Target, IdxT> = Dag::new();
        let mut deps: Vec<(&str, &str)> = Vec::new();
        let mut node_idxes: HashMap<&str, NodeIdx> = HashMap::new();
        let mut trie: ShortcutTrie = SequenceTrie::new();

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        for target_cfg in &cfg.targets {
            validate_target_cfg(target_cfg, &node_idxes)?;

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
            if let Some(idxes) = trie.get_mut(target_shortcut.iter()) {
                idxes.push(node_idx);
            } else {
                trie.insert(target_shortcut.iter(), vec![node_idx]);
            }

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

    /// Takes a shortcut and determines what all the possible subsequent
    /// characters could be. Note that reconciliation is _not_ done here, and
    /// this function can return a next-character that leads to an ambiguous
    /// shortcut. Callers must handle this themselves. See tui.rs.
    pub fn next_keys(&self, prefix: &Shortcut) -> Vec<char> {
        let subtrie = self.shortcuts.get_node(prefix.iter());
        if let Some(subtrie) = subtrie {
            let mut keys: Vec<char> = subtrie
                .keys()
                .filter(|k| !k.is_empty())
                .filter_map(|k| k.get(0).copied())
                .cloned()
                .collect();
            // Because the keys we return are individual _characters_ of
            // _full_ sequences, it is possible to return duplicates
            // here. For example, consider the following shortcuts to
            // exist:
            //   a
            //   ab
            // The first call with the prefix being the empty shortcut will
            // return [a,a] unless we de-dupe things.
            keys.sort_unstable();
            keys.dedup();
            keys
        } else {
            // If there is no subtrie for this prefix, then there
            // should be no next_keys, there's nothing to extend it
            // with.
            vec![]
        }
    }

    /// Takes a shortcut and returns an existence result. The result includes
    /// the case where the shortcut leads to an ambiguity that must be
    /// reconciled.
    pub fn lookup(&self, shortcut: &Shortcut) -> Lookup {
        match self.get_idxes(shortcut) {
            Ok(idxes) => {
                if idxes.is_empty() {
                    Lookup::NotFound
                } else if idxes.len() == 1 {
                    Lookup::Found
                } else {
                    // Multiple.
                    Lookup::Conflict
                }
            }
            Err(ExecError::Ambiguous {
                shortcut: _,
                conflict_msg: _,
            }) => Lookup::Conflict,
            Err(ExecError::Reconciliation { description }) => {
                Lookup::ReconciliationFailure(description)
            }
            Err(_) => Lookup::NotFound,
        }
    }

    /// Takes a shortcut and returns the names of the potential targets it may
    /// lead to if it were to be extended by a single character. This is similar
    /// to next_keys(), but returns the names of the targets it leads to rather
    /// than what the next characters to get to those targets are.
    /// NOTE: This method is different from get_idxes() as well.  get_idxes
    /// tries to find the nodes that a given shortcut maps to.  We don't
    /// actually want that here. We want the things that this shortcut can
    /// _eventually_ lead to.  AKA, we don't want just the shortcut itself, we
    /// also want its children.
    pub fn get_children_names(&self, shortcut: &Shortcut) -> ExecResult<Vec<&'a str>> {
        Ok(
            if let Some(subtrie) = self.shortcuts.get_node(shortcut.iter()) {
                subtrie
                    .values() // Gets the key values contained under this subtrie, which means only the super-strings.
                    .flatten() // Flatten them into a stream of node indexes, since a single key can map to multiple targets.
                    .map(|nidx| self.dag[*nidx].name) // Look up the node index to get the proper target name.
                    .collect()
            } else {
                // If the shortcut isn't found in the trie, it is likely a conflict.
                self.get_idxes(shortcut)? // Run get_idxes to see if reconciliation gets us anything.
                    .iter() // Iterate them.
                    .map(|idx| self.dag[*idx].name) // And map them to their names.
                    .collect()
            },
        )
    }

    /// Takes the given shortcut and determines what the reconciled subsequent
    /// characters should be.
    pub fn reconcile(&self, _: &Shortcut) -> ExecResult<Vec<char>> {
        Err(ExecError::Reconciliation {
            description: String::from("not implemented"),
        })
    }

    pub fn execute(&self, shortcut: Shortcut) -> ExecResult<()> {
        info!(self.logger, "executing");
        let target_idxes = self.get_idxes(&shortcut)?;
        if target_idxes.len() > 1 {
            return Err(ExecError::Ambiguous {
                shortcut,
                conflict_msg: target_idxes
                    .iter()
                    .map(|idx| format!("'{}'", self.dag[*idx].name))
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

    // NOTE: We take a logger explicitly here instead of relying on self.logger
    // because this function can execute recursively, and we want to encode that
    // recursion into the logger's context as it goes on.
    fn execute_target(&self, logger: &slog::Logger, nidx: NodeIdx, depth: usize) -> ExecResult<()> {
        let target = &self.dag[nidx];
        info!(
            logger,
            "{}", if depth == 0 {"executing target"} else {"executing dependency"};
            o!("name" => target.name, "depth" => depth)
        );
        let deps = self.dag.children(nidx);
        let mut num_deps_execed = 0;
        for dep in deps.iter(&self.dag) {
            let dep_logger = logger.new(o!("parent" => target.name.to_string()));
            self.execute_target(&dep_logger, dep.1, depth + 1)
                .map_err(|err| ExecError::Dependency {
                    dep_name: target.name.to_string(),
                    err: Box::new(err),
                })?;
            num_deps_execed += 1;
        }
        if let Some(cmd) = target.cmd {
            info!(logger, "running executor"; o!("cmd" => cmd));
            match self.executor.execute(target.execute_kind, cmd) {
                Ok(true) => {
                    info!(logger, "successfully executed target");
                }
                Ok(false) => {
                    return Err(ExecError::Executor {
                        description: String::from("command failed to execute"),
                    })
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
        match shortcuts_ref.get(shortcut.iter()) {
            Some(nidxes) => target_idxes = nidxes.to_vec(),
            None => {
                if let Some(node) = shortcuts_ref.get_node(shortcut.iter()) {
                    eprintln!("Child count: {}", node.children().iter().count());
                    // If the shortcut is found as a subtrie trie, then it is an
                    // ambiguity.
                    return Err(ExecError::Ambiguous {
                        shortcut: shortcut.clone(),
                        conflict_msg: node
                            .children()
                            .iter()
                            .filter(|node| node.value().is_some())
                            .map(|node| format!("'{}'", self.dag[node.value().unwrap()[0]].name))
                            .intersperse(String::from(" or "))
                            .collect(),
                    });
                }
                panic!("reconciliation not implemented");
            }
        };
        Ok(target_idxes)
    }

    fn reconcile_with_nidxes(
        &self,
        shortcut: &Shortcut,
        nidxes: &[NodeIdx],
    ) -> ExecResult<Vec<char>> {
        Err(ExecError::Reconciliation {
            description: String::from("not implemented"),
        })
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::target;
    use crate::config::{Config, TargetCfg};

    use slog::*;
    use slog_term::TestStdoutWriter;

    fn test_logger() -> slog::Logger {
        let decorator = slog_term::PlainSyncDecorator::new(TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build();
        let drain = std::sync::Mutex::new(drain).fuse();
        slog::Logger::root(drain, o!())
    }

    fn get_jam(cfg: &DesugaredConfig) -> Jam {
        Jam::new(&test_logger(), Executor::new(), cfg).expect("expected no errors from parsing")
    }

    mod parse {
        use super::*;

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

        fn check_jam_err(targets: Vec<TargetCfg>, expected_err: &str) {
            let cfg = Config {
                options: Options {
                    reconciliation_strategy: crate::reconciler::Strategy::Error,
                    log_level: Some(Level::Disabled),
                },
                targets,
            }
            .desugar()
            .unwrap();
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

    // The tests in this module involve the execution of shell commands.
    // In the ideal case, we would not need to invoke the shell and
    // use some mocking, however, the way ExecuteKind is right now
    // makes that annoying.
    mod execution {
        use crate::{reconciler::Strategy, testutils::tmp::*};

        use super::*;

        #[test]
        fn executes_simple_target() {
            let expected_message = "hello world";
            let cmd = &format!("echo {expected_message}");
            let out_file = TmpFile::new();
            let cfg = Config::with_targets(vec![target::exec("blah", cmd, &out_file)]);
            let jam = get_jam(&cfg);
            jam.execute(Shortcut::from_shortcut_str("b"))
                .expect("expected execution of the command to pass");
            check_file_contents(out_file, expected_message);
        }

        #[test]
        fn executes_dep() {
            let foo_cmd = "echo 'foo'";
            let bar_cmd = "echo 'bar'";
            let foo_file = TmpFile::new();
            let bar_file = TmpFile::new();
            let cfg = Config::with_targets(vec![
                target::exec("bar", bar_cmd, &bar_file),
                target::exec_deps("foo", foo_cmd, &foo_file, vec!["bar"]),
            ]);
            let jam = get_jam(&cfg);
            jam.execute(Shortcut::from_shortcut_str("f"))
                .expect("expected execution of the command to pass");
            check_file_contents(foo_file, "foo");
            check_file_contents(bar_file, "bar");
        }

        #[test]
        fn executes_dep_only_when_ran_explicitly() {
            let foo_cmd = "echo 'foo'";
            let bar_cmd = "echo 'bar'";
            let foo_file = TmpFile::new();
            let bar_file = TmpFile::new();
            let cfg = Config::with_targets(vec![
                target::exec("bar", bar_cmd, &bar_file),
                target::exec_deps("foo", foo_cmd, &foo_file, vec!["bar"]),
            ]);
            let jam = get_jam(&cfg);
            jam.execute(Shortcut::from_shortcut_str("b"))
                .expect("expected execution of the command to pass");
            // Foo should not have been executed, since we executed 'b', or bar, only.
            assert!(!check_file(foo_file));
            check_file_contents(bar_file, "bar");
        }

        #[test]
        fn honors_execute_kind() {
            // This is a bit bleh, cause if we had a bug and we did
            // try to write to this file, it is very possible we'd
            // fail from a permissions issue, etc. This would still
            // fail the test, so we're considering that acceptable,
            // but it would be cleaner to avoid that since a failure
            // about permissions does not directly indicate that the
            // problem is a lack of honoring of execute kinds.
            let out_file = "/tmp/blah.txt";
            let cfg = Config::with_targets(vec![TargetCfg {
                name: String::from("blah"),
                shortcut_str: None,
                help: None,
                cmd: Some(String::from("echo 'should not be ran!' > {out_file}")),
                targets: None,
                deps: None,
                execute_kind: Some(ExecuteKind::DryRun),
            }]);
            let jam = get_jam(&cfg);
            jam.execute(Shortcut::from_shortcut_str("b"))
                .expect("expected execution of the command to pass");
            // Since we are executing via dry run, no command and
            // therefore no file should have been created.
            assert!(!check_file(out_file));
        }

        #[test]
        fn handles_reconciliation() {
            let expected_message = "hello world";
            let cmd = &format!("echo {expected_message}");
            let breh_file = TmpFile::new();
            let bruh_file = TmpFile::new();
            let mut cfg = Config::with_targets(vec![
                target::exec("breh", cmd, &breh_file),
                target::exec("bruh", cmd, &bruh_file),
            ]);
            cfg.options.reconciliation_strategy = Strategy::FirstNonMatch;
            let jam = get_jam(&cfg);
            // When we attempt to execute 'b-u', 'b' by itself would
            // be ambiguous.
            // 'b-u' however distinguishes 'bruh' from 'breh'. We
            // should therefore see bruh_file with the expected
            // contents, but not breh_file.
            jam.execute(Shortcut::from_shortcut_str("b-u"))
                .expect("expected execution of the command to pass");
            check_file_contents(bruh_file, expected_message);
            assert!(!check_file(breh_file))
        }

        mod errors {
            use super::*;

            fn check_err(res: ExecResult<()>, expected_err: &str) {
                assert_eq!(
                    res.expect_err("expected execution of the command to fail")
                        .to_string(),
                    expected_err
                );
            }

            #[test]
            fn failed_reconciliation() {
                let cmd = "blah";
                let out_file_bar = TmpFile::new();
                let out_file_baz = TmpFile::new();
                let mut cfg = Config::with_targets(vec![
                    target::exec("conflict-bar", cmd, &out_file_bar),
                    target::exec("conflict-baz", cmd, &out_file_baz),
                ]);
                cfg.options.reconciliation_strategy = Strategy::Error;
                let jam = get_jam(&cfg);
                // When we attempt to execute 'b-u', 'b' by itself would
                // be ambiguous.
                // 'b-u' however distinguishes 'bruh' from 'conflict1'. We
                // should therefore see bruh_file with the expected
                // contents, but not conflict1_file.
                check_err(
                    jam.execute(Shortcut::from_shortcut_str("c-b")),
                    "given shortcut 'c-b' is ambiguous (i.e. is it 'conflict-bar' or 'conflict-baz'?)",
                );
                assert!(!check_file(out_file_bar));
                assert!(!check_file(out_file_baz));
            }

            #[test]
            fn failed_execution() {
                let cmd = "idontexist";
                let out_file = TmpFile::new();
                let cfg = Config::with_targets(vec![target::exec("idontexist", cmd, &out_file)]);
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute(Shortcut::from_shortcut_str("i")),
                    "command failed to execute",
                );
                assert!(!check_file(out_file));
            }

            #[test]
            fn executing_cmd_that_dne() {
                let cmd = "echo 'blah'";
                let out_file = TmpFile::new();
                let cfg = Config::with_targets(vec![target::exec("idontexist", cmd, &out_file)]);
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute(Shortcut::from_shortcut_str("z-z-z-z-z")),
                    "no command for given shortcut 'z-z-z-z-z'",
                );
                assert!(!check_file(out_file));
            }

            #[test]
            fn execution_failure_from_dep() {
                let foo_cmd = "echo 'foo'";
                let bar_cmd = "idontexist";
                let foo_file = TmpFile::new();
                let bar_file = TmpFile::new();
                let cfg = Config::with_targets(vec![
                    target::exec("bar", bar_cmd, &bar_file),
                    target::exec_deps("foo", foo_cmd, &foo_file, vec!["bar"]),
                ]);
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute(Shortcut::from_shortcut_str("f")),
                    "failed to execute dependency ('foo'): command failed to execute",
                );
                // Neither file should be created. The dependency will
                // fail, which should make the dependent not even run.
                assert!(!check_file(foo_file));
                assert!(!check_file(bar_file));
            }

            #[test]
            fn shortcut_without_reconciliation_suffix() {
                let expected_message = "hello world";
                let cmd = &format!("echo {expected_message}");
                let breh_file = TmpFile::new();
                let bruh_file = TmpFile::new();
                let mut cfg = Config::with_targets(vec![
                    target::exec("breh", cmd, &breh_file),
                    target::exec("bruh", cmd, &bruh_file),
                ]);
                cfg.options.reconciliation_strategy = Strategy::FirstNonMatch;
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute(Shortcut::from_shortcut_str("b")),
                    "given shortcut 'b' is ambiguous (i.e. is it 'breh' or 'bruh'?)",
                );
                assert!(!check_file(bruh_file));
                assert!(!check_file(breh_file));
            }

            #[test]
            fn shortcut_without_reconciliation_suffix_three_targets() {
                let expected_message = "hello world";
                let cmd = &format!("echo {expected_message}");
                let breh_file = TmpFile::new();
                let bruh_file = TmpFile::new();
                let broh_file = TmpFile::new();
                let mut cfg = Config::with_targets(vec![
                    target::exec("breh", cmd, &breh_file),
                    target::exec("bruh", cmd, &bruh_file),
                    target::exec("broh", cmd, &broh_file),
                ]);
                cfg.options.reconciliation_strategy = Strategy::FirstNonMatch;
                let jam = get_jam(&cfg);
                check_err(
                    jam.execute(Shortcut::from_shortcut_str("b")),
                    "given shortcut 'b' is ambiguous (i.e. is it 'broh' or 'breh' or 'broh'?)",
                );
                assert!(!check_file(bruh_file));
                assert!(!check_file(breh_file));
                assert!(!check_file(broh_file));
            }
        }
    }
}
