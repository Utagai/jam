use std::collections::{HashMap, VecDeque};

use anyhow::{anyhow, bail, Result};
use daggy::Walker;
use daggy::{Dag, NodeIndex};
use radix_trie::{Trie, TrieKey};

use crate::config::{Config, TargetCfg};

// TODO: Exercise - can we use &str in any of these fields?
struct Target {
    name: String,
    chord: Chord,
    help: String,
    cmd: Option<String>,
}

impl From<&TargetCfg> for Target {
    fn from(value: &TargetCfg) -> Self {
        // TODO: The clone() calls here may go away if we manage to
        // use &str in the TargetCfg instead...  Right now they are all
        // String, which we can't take without moving or
        // cloning. Moving can't be done here since we are From'ing
        // from a shared reference, so we have to clone.
        return Target {
            name: value.name.clone(),
            chord: value
                .chord_str
                .clone()
                .map_or(Chord::from_name(&value.name), |chord_str| {
                    Chord::from_shortname(&chord_str)
                }),
            help: value
                .help
                .clone()
                .unwrap_or(String::from("no help provided")),
            cmd: value.cmd.clone(),
        };
    }
}

pub struct Jam {
    // TODO: We should be able to bind the type parameter of NodeIndex
    // to the corresponding type parameter in Dag.
    root_targets: Vec<NodeIndex<u32>>,
    dag: Dag<Target, u32>,
    chords: Trie<Chord, Vec<NodeIndex<u32>>>,
}

#[derive(Eq)]
struct Chord(Vec<String>);

impl Chord {
    fn from_name(name: &str) -> Chord {
        Self::from_shortname(&Self::name_to_short(name))
    }

    fn from_shortname(shortname: &str) -> Chord {
        Chord(
            shortname
                .split("-")
                .map(|note| String::from(note))
                .collect(),
        )
    }

    fn name_to_short<T: AsRef<str>>(name: T) -> String {
        // TODO: Eventually, this delimiter should be configured.
        name.as_ref()
            .split("-")
            .map(|segment| segment.chars().nth(0).unwrap().to_string())
            .collect::<Vec<String>>()
            .join("-")
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
        Chord(self.0.iter().map(|note| note.clone()).collect())
    }
}

impl TrieKey for Chord {
    fn encode_bytes(&self) -> Vec<u8> {
        self.0.iter().flat_map(|s| s.encode_bytes()).collect()
    }
}

impl Jam {
    pub fn parse(cfg: Config) -> Result<Jam> {
        let mut exec_dag: Dag<Target, u32> = Dag::new();
        let mut node_idxes: HashMap<String, NodeIndex<u32>> = HashMap::new();
        let mut target_queue: VecDeque<&TargetCfg> = VecDeque::new();
        let mut deps: Vec<(String, String)> = Vec::new();
        let mut root_targets: Vec<NodeIndex<u32>> = Vec::new();
        // TODO: We should flip it so that the Trie stores the Target,
        // and the Dag stores the target chord to index into the trie.
        // This would be more efficient and we won't need the helpers
        // we currently have for finding a Target given a target_name.
        let mut trie: Trie<Chord, Vec<NodeIndex<u32>>> = Trie::new();

        target_queue.extend(cfg.targets.iter());

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        while !target_queue.is_empty() {
            let queue_len = target_queue.len();
            for _ in 0..queue_len {
                let target_cfg: &TargetCfg = target_queue.pop_front().unwrap(); // The while loop condition guarantees this.
                println!("Looking at target: {}", target_cfg.name);

                // Target validations:
                // TODO: This should be validating short names too.
                if target_cfg.name.len() == 0 {
                    bail!("cannot have an empty target name")
                } else if target_cfg.name.contains(".") {
                    // TODO: This and its sibling, '?', should have
                    // its checks changed to be more permissive --
                    // they're only not allowed _after_ a delimiter,
                    // as that's when they cause ambiguity. Anywhere
                    // else is actually fine. e.g. foo.bar-baz is
                    // actually fine!
                    bail!("cannot have a '.' in a target name: '{}'", target_cfg.name)
                } else if target_cfg.name.contains("?") {
                    bail!("cannot have a '?' in a target name: '{}'", target_cfg.name)
                } else if node_idxes.contains_key(&target_cfg.name) {
                    bail!("duplicate target name: '{}'", target_cfg.name)
                } else if target_cfg.deps.is_none()
                    && target_cfg.targets.is_none()
                    && target_cfg.cmd.is_none()
                {
                    bail!("a command without an executable command must have dependencies or subtargets, but '{}' does not", target_cfg.name)
                }

                let target = Target::from(target_cfg);
                let target_chord = target.chord.clone();

                let node_idx = exec_dag.add_node(target);
                trie.map_with_default(target_chord, |idxes| idxes.push(node_idx), vec![node_idx]);
                // This will only fill up to the amount of the _first_
                // value of queue_len, which is going to be the number
                // of nodes at the top level of the BFS -- this is
                // precisely the set of root nodes, aka, the top-level
                // targets.
                if root_targets.len() < queue_len {
                    root_targets.push(node_idx.clone());
                }
                if let Some(targets) = &target_cfg.targets {
                    for subtarget in targets {
                        target_queue.push_back(subtarget);
                        deps.push((target_cfg.name.clone(), subtarget.name.clone()))
                    }
                }

                if let Some(target_deps) = &target_cfg.deps {
                    for dep in target_deps {
                        deps.push((target_cfg.name.clone(), dep.clone()));
                    }
                }
                node_idxes.insert(target_cfg.name.clone(), node_idx);
            }
        }

        // With their dependencies recorded, now add edges to the DAG to represent them:
        for dep in deps {
            let dependee_idx = node_idxes
                .get(&dep.0)
                .ok_or(Jam::nonexistent_dep_err(dep.0.clone()))?;
            let dependent_idx = node_idxes
                .get(&dep.1)
                .ok_or(Jam::nonexistent_dep_err(dep.1.clone()))?;
            exec_dag
                .add_edge(*dependee_idx, *dependent_idx, 0)
                .map_err(|_| anyhow!("'{}' -> '{}' creates a cycle", dep.0, dep.1))?;
        }

        Ok(Jam {
            root_targets,
            dag: exec_dag,
            chords: trie,
        })
    }

    fn nonexistent_dep_err<T: AsRef<str> + std::fmt::Display>(dep_name: T) -> anyhow::Error {
        anyhow!("reference to nonexistent dep: {}", dep_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parse {
        use super::*;

        use crate::config::Options;

        impl Jam {
            fn node_has_target(
                &self,
                nidx: &NodeIndex<u32>,
                target_name: &str,
            ) -> Option<NodeIndex<u32>> {
                if self.dag[*nidx].name == target_name {
                    return Some(*nidx);
                }

                self.dag
                    .children(*nidx)
                    .iter(&self.dag)
                    .map(|(_, n)| self.node_has_target(&n, target_name))
                    .find(|n| n.is_some())
                    .flatten()
            }

            fn get_target_by_name(&self, target_name: &str) -> Option<&Target> {
                self.root_targets
                    .iter()
                    .map(|root| self.node_has_target(root, target_name))
                    .find(|found_node| found_node.is_some())
                    .flatten()
                    .map(|node_idx| &self.dag[node_idx])
            }

            fn get_targets_by_chord(&self, chord: Chord) -> Option<&Vec<NodeIndex<u32>>> {
                self.chords.get(&chord)
            }

            fn has_target(&self, target_name: &str) -> bool {
                self.get_target_by_name(target_name).is_some()
            }

            fn has_dep(&self, dependee: &str, dependent: &str) -> bool {
                for root in &self.root_targets {
                    let depender_idx = self.node_has_target(root, dependee);
                    let dependent_idx = self.node_has_target(root, dependent);
                    if match depender_idx.zip(dependent_idx) {
                        Some((depender_idx, dependent_idx)) => self
                            .dag
                            .find_edge(depender_idx, dependent_idx)
                            .is_some(),
                        None => false,
                    } {
                        return true;
                    }
                }

                return false;
            }
        }

        mod target {
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

        fn get_jam(targets: Vec<TargetCfg>) -> Jam {
            let cfg = Config {
                options: Options {},
                targets,
            };
            Jam::parse(cfg).expect("expected no errors from parsing")
        }

        fn check_jam_err(targets: Vec<TargetCfg>, expected_err: &str) {
            let cfg = Config {
                options: Options {},
                targets,
            };
            if let Err(err) = Jam::parse(cfg) {
                assert_eq!(format!("{:?}", err).trim(), expected_err)
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

        fn verify_jam_dags(jams: Vec<Jam>, targets: &Vec<&str>, deps: &Vec<(&str, &str)>) {
            for jam in jams {
                verify_jam_dag(jam, targets, deps);
            }
        }

        mod nodeps {
            use super::*;

            #[test]
            fn single_target() {
                let expected_target_name = "foo";
                let jam = get_jam(vec![target::lone(expected_target_name)]);
                verify_jam_dag(jam, &vec![expected_target_name], &vec![]);
            }

            #[test]
            fn zero_targets() {
                let jam = get_jam(vec![]);
                verify_jam_dag(jam, &vec![], &vec![]);
            }

            #[test]
            fn multiple_targets() {
                let expected_target_names = vec!["foo", "bar", "quux"];
                let jam = get_jam(
                    expected_target_names
                        .iter()
                        .map(|name| target::lone(name))
                        .collect(),
                );
                expected_target_names
                    .iter()
                    .for_each(|name| assert!(jam.has_target(&name)));
                verify_jam_dag(jam, &expected_target_names, &vec![]);
            }
        }

        mod deps {
            use super::*;

            #[test]
            fn single_dependency() {
                let depjam = get_jam(vec![target::lone("bar"), target::dep("foo", vec!["bar"])]);
                let subjam = get_jam(vec![target::sub("foo", vec![target::lone("bar")])]);
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["foo", "bar"],
                    &vec![("foo", "bar")],
                );
            }

            #[test]
            fn one_target_two_dependents() {
                let depjam = get_jam(vec![
                    target::lone("bar"),
                    target::lone("quux"),
                    target::dep("foo", vec!["bar", "quux"]),
                ]);

                let subjam = get_jam(vec![target::sub(
                    "foo",
                    vec![target::lone("bar"), target::lone("quux")],
                )]);
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["foo", "bar", "quux"],
                    &vec![("foo", "bar"), ("foo", "quux")],
                );
            }

            #[test]
            fn single_dependency_but_dependee_defined_first() {
                let jam = get_jam(vec![target::dep("foo", vec!["bar"]), target::lone("bar")]);
                verify_jam_dag(jam, &vec!["foo", "bar"], &vec![("foo", "bar")]);
            }

            #[test]
            fn two_targets_one_dep_each() {
                let depjam = get_jam(vec![
                    target::lone("c"),
                    target::lone("d"),
                    target::dep("a", vec!["c"]),
                    target::dep("b", vec!["d"]),
                ]);

                let subjam = get_jam(vec![
                    target::sub("a", vec![target::lone("c")]),
                    target::sub("b", vec![target::lone("d")]),
                ]);
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["a", "b", "c", "d"],
                    &vec![("a", "c"), ("b", "d")],
                );
            }

            #[test]
            fn two_targets_share_a_dep() {
                let depjam = get_jam(vec![
                    target::lone("c"),
                    target::dep("a", vec!["c"]),
                    target::dep("b", vec!["c"]),
                ]);

                let subjam = get_jam(vec![
                    target::sub("a", vec![target::lone("c")]),
                    target::dep("b", vec!["c"]),
                ]);
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["a", "b", "c"],
                    &vec![("a", "c"), ("b", "c")],
                );
            }
        }

        mod shortnames {
            use super::*;

            impl Chord {
                fn new(notes: Vec<&str>) -> Chord {
                    Chord(notes.into_iter().map(|note| String::from(note)).collect())
                }
            }

            #[test]
            fn automatic_of_single_word_is_first_char() {
                let jam = get_jam(vec![target::lone("foo")]);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["f"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                )
            }

            #[test]
            fn automatic_of_double_word_is_first_char() {
                let jam = get_jam(vec![target::lone("foo-bar")]);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["f", "b"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                );
            }

            #[test]
            fn automatic_of_multi_word_is_first_char() {
                let jam = get_jam(vec![target::lone("foo-bar-baz-quux")]);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["f", "b", "b", "q"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                );
            }

            #[test]
            fn automatic_of_multiple_non_conflicting_targets() {
                let jam = get_jam(vec![
                    target::lone("foo"),
                    target::lone("bar"),
                    target::lone("quux"),
                ]);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["f"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                );
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["b"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                );
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["q"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                );
            }

            #[test]
            fn automatic_of_multiple_conflicting_targets_no_reconciliation() {
                let jam = get_jam(vec![
                    target::lone("bar"),
                    target::lone("baz"),
                    target::lone("bam"),
                    target::lone("barr"),
                ]);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["b"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    4,
                );
            }

            #[test]
            fn override_respected() {
                let jam = get_jam(vec![TargetCfg {
                    name: String::from("foo"),
                    chord_str: Some(String::from("x")),
                    help: None,
                    cmd: Some(String::from("blah")),
                    targets: None,
                    deps: None,
                }]);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["x"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
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
                            target::lone("bar"),
                        ],
                        "duplicate target name: 'bar'",
                    );
                }

                #[test]
                fn child_parent() {
                    check_jam_err(
                        vec![target::sub("foo", vec![target::lone("foo")])],
                        "duplicate target name: 'foo'",
                    );
                }

                #[test]
                fn grand_child_parent() {
                    check_jam_err(
                        vec![target::sub(
                            "foo",
                            vec![target::sub("bar", vec![target::lone("foo")])],
                        )],
                        "duplicate target name: 'foo'",
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
