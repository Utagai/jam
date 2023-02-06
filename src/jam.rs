use std::collections::HashMap;

use anyhow::{anyhow, bail, Result};
use daggy::{Dag, NodeIndex, Walker};
use radix_trie::{Trie, TrieKey};

use crate::{
    config::{DesugaredConfig, DesugaredTargetCfg, Options},
    executor::{ExecuteKind, Executor},
};

struct Target<'a> {
    name: &'a str,
    chord: Chord,
    help: &'a str,
    cmd: Option<&'a str>,
    execute_kind: ExecuteKind,
}

impl<'a> From<&'a DesugaredTargetCfg> for Target<'a> {
    fn from(cfg: &'a DesugaredTargetCfg) -> Self {
        return Target {
            name: &cfg.name,
            chord: Chord::from_shortname(&cfg.chord_str),
            help: &cfg.help,
            cmd: cfg.cmd.as_deref(),
            execute_kind: cfg.execute_kind,
        };
    }
}

type IdxT = u32;
type NodeIdx = NodeIndex<IdxT>;
pub type ChordTrie = Trie<Chord, Vec<NodeIdx>>;

pub struct Jam<'a> {
    opts: &'a Options,
    executor: Executor,
    dag: Dag<Target<'a>, IdxT>,
    chords: ChordTrie,
}

#[derive(Eq)]
pub struct Chord(pub Vec<String>);

impl Chord {
    fn from_shortname(shortname: &str) -> Chord {
        Chord(shortname.split('-').map(String::from).collect())
    }
}

impl PartialEq for Chord {
    fn eq(&self, other: &Self) -> bool {
        self.0
            .iter()
            .zip(other.0.iter())
            .fold(true, |acc, (s, o)| acc && s == o)
    }
}

impl std::fmt::Display for Chord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("-"))
    }
}

impl Clone for Chord {
    fn clone(&self) -> Self {
        Chord(self.0.to_vec())
    }
}

impl TrieKey for Chord {
    fn encode_bytes(&self) -> Vec<u8> {
        self.0.iter().flat_map(|s| s.encode_bytes()).collect()
    }
}

impl<'a> Jam<'a> {
    pub fn new(executor: Executor, cfg: &'a DesugaredConfig) -> Result<Jam<'a>> {
        let mut dag: Dag<Target, IdxT> = Dag::new();
        let mut deps: Vec<(&str, &str)> = Vec::new();
        let mut node_idxes: HashMap<&str, NodeIdx> = HashMap::new();
        // TODO: We should flip it so that the Trie stores the Target,
        // and the Dag stores the target chord to index into the trie.
        // This would be more efficient and we won't need the helpers
        // we currently have for finding a Target given a target_name.
        let mut trie: ChordTrie = Trie::new();

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        for target_cfg in &cfg.targets {
            // TODO: Maybe move the loop body into a function.
            // The while loop condition guarantees this.
            Self::validate_target_cfg(target_cfg, &node_idxes)?;

            let target = Target::from(target_cfg);
            let target_chord = target.chord.clone();

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

            // And add it to the trie under its chord.
            // A natural question to ask here is about conflicts
            // of chords. For example, if two targets both have
            // the chord `t-a`, how can we disambiguate them? The
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
            // the user is going to use an ambiguous chord, which
            // may only be for very rare targets!
            trie.map_with_default(target_chord, |idxes| idxes.push(node_idx), vec![node_idx]);

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
        for dep in deps {
            let dependee_idx = node_idxes
                .get(dep.0)
                .ok_or(Jam::nonexistent_dep_err(dep.0))?;
            let dependent_idx = node_idxes
                .get(dep.1)
                .ok_or(Jam::nonexistent_dep_err(dep.1))?;
            dag.add_edge(*dependee_idx, *dependent_idx, 0)
                .map_err(|_| anyhow!("'{}' -> '{}' creates a cycle", dep.0, dep.1))?;
        }

        Ok(Jam {
            // TODO: If we ever end up having a distinction between
            // config-time options vs. run-time options, we may want
            // to create a separate type here for isolation.
            opts: &cfg.options,
            executor,
            dag,
            chords: trie,
        })
    }

    fn validate_target_cfg(
        cfg: &DesugaredTargetCfg,
        node_idxes: &HashMap<&str, NodeIdx>,
    ) -> Result<()> {
        // TODO: This should be validating short names too.
        if cfg.name.is_empty() {
            bail!("cannot have an empty target name")
        } else if cfg.name.contains('.') {
            // TODO: This and its sibling, '?', should have
            // its checks changed to be more permissive --
            // they're only not allowed _after_ a delimiter,
            // as that's when they cause ambiguity. Anywhere
            // else is actually fine. e.g. foo.bar-baz is
            // actually fine!
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

    // TODO: Should these functions be replaced with an Error enum?
    fn nonexistent_dep_err<T: AsRef<str> + std::fmt::Display>(dep_name: T) -> anyhow::Error {
        anyhow!("reference to nonexistent dep: {}", dep_name)
    }

    fn no_cmd_for_chord(chord: &Chord) -> anyhow::Error {
        anyhow!("no command for given chord: '{}'", chord)
    }

    fn ambiguous_chord(&self, chord: &Chord, nidxes: &[NodeIdx]) -> anyhow::Error {
        anyhow!(
            "given chord '{}' is ambiguous (i.e. is it {}?)",
            chord,
            nidxes
                .iter()
                .map(|nidx| format!("'{}'", self.dag[*nidx].name))
                .collect::<Vec<String>>()
                .join(" or "),
        )
    }

    fn cannot_exec_cmd(target: &Target) -> anyhow::Error {
        anyhow!("target '{}' has no executable function", target.name)
    }

    fn execute_target(&self, nidx: NodeIdx) -> Result<()> {
        let deps = self.dag.children(nidx);
        let mut num_deps_execed = 0;
        for dep in deps.iter(&self.dag) {
            println!("\tExecuting dependency: {:?}", self.dag[dep.1].name);
            self.execute_target(dep.1)?;
            num_deps_execed += 1;
        }
        let target = &self.dag[nidx];
        println!("found target: '{}'", target.name);
        if let Some(cmd) = target.cmd {
            if self.executor.execute(target.execute_kind, cmd)? {
                println!("\tSuccessfully executed!")
            } else {
                println!("\tFailed to execute!")
            }
        } else if num_deps_execed <= 0 {
            bail!(Jam::cannot_exec_cmd(target))
        }
        Ok(())
    }

    // TODO: I think rename play, chord, notes, etc terminology?
    // Namely, I think a chord is for _simultaneous_ notes, not in sequence.
    // That is a chord _progression_... and that doesn't ring off the tongue very well, now does it?
    pub fn play(&self, chord: Chord) -> Result<()> {
        let nidxes = self
            .chords
            .get(&chord)
            .ok_or(Jam::no_cmd_for_chord(&chord))?; // TODO: We can do better with the error message here.
        if nidxes.len() > 1 {
            return Err(self.ambiguous_chord(&chord, nidxes));
        }
        if let Some(nidx) = nidxes.first() {
            self.execute_target(*nidx)
        } else {
            bail!(Jam::no_cmd_for_chord(&chord))
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

        use crate::config::Options;

        impl<'a> Jam<'a> {
            fn node_has_target(&self, target_name: &str) -> Option<NodeIdx> {
                self.chords
                    .iter()
                    .flat_map(|kv| kv.1.clone())
                    .find(|nidx| self.dag[*nidx].name == target_name)
            }

            fn get_target_by_name(&self, target_name: &str) -> Option<&Target> {
                self.chords
                    .iter()
                    .flat_map(|kv| kv.1.clone())
                    .map(|nidx| &self.dag[nidx])
                    .find(|target| target.name == target_name)
            }

            fn get_targets_by_chord(&self, chord: Chord) -> Option<&Vec<NodeIdx>> {
                self.chords.get(&chord)
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

        fn get_jam(cfg: &DesugaredConfig) -> Jam {
            Jam::new(Executor::new(), cfg).expect("expected no errors from parsing")
        }

        fn check_jam_err(targets: Vec<TargetCfg>, expected_err: &str) {
            let cfg = Config {
                options: Options {
                    reconciliation_strategy: crate::reconciler::Strategy::Error,
                },
                targets,
            }
            .desugar();
            if let Err(err) = Jam::new(Executor::new(), &cfg) {
                assert_eq!(format!("{err}").trim(), expected_err)
            } else {
                panic!("expected an error from parsing, but got none")
            }
        }

        // TODO: We can go a step further and actually construct the
        // Jam instance here ourselves based on targets + deps, but it
        // would require more complex logic to derive a DAG from the
        // two arguments and could be error prone... maybe later.
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
                // TODO: With the fixes to how we rename subtargets, this method of asserting is no longer correct.
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
                            chord_str: None,
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
