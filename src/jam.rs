use std::collections::{HashMap, VecDeque};

use anyhow::{anyhow, bail, Result};
use daggy::{Dag, NodeIndex};
use radix_trie::{Trie, TrieKey};

use crate::config::{Config, TargetCfg};

struct Target<'a> {
    name: &'a str,
    chord: Chord,
    help: &'a str,
    cmd: Option<&'a str>,
}

impl<'a> From<&'a TargetCfg> for Target<'a> {
    fn from(cfg: &'a TargetCfg) -> Self {
        return Target {
            name: &cfg.name,
            chord: cfg
                .chord_str
                .as_deref()
                .map_or(Chord::from_name(&cfg.name), |chord_str| {
                    Chord::from_shortname(&chord_str)
                }),
            help: cfg.help.as_deref().unwrap_or("no help provided"),
            cmd: cfg.cmd.as_deref(),
        };
    }
}

type NIdx = u32;

pub struct Jam<'a> {
    root_targets: Vec<NodeIndex<NIdx>>,
    dag: Dag<Target<'a>, NIdx>,
    chords: Trie<Chord, Vec<NodeIndex<NIdx>>>,
}

#[derive(Eq)]
pub struct Chord(pub Vec<String>);

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

impl<'a> Jam<'a> {
    pub fn parse(cfg: &'a Config) -> Result<Jam<'a>> {
        let mut dag: Dag<Target, NIdx> = Dag::new();
        let mut deps: Vec<(&str, &str)> = Vec::new();
        let mut node_idxes: HashMap<&str, NodeIndex<NIdx>> = HashMap::new();
        let mut target_queue: VecDeque<&TargetCfg> = VecDeque::new();
        let mut root_targets: Vec<NodeIndex<NIdx>> = Vec::new();
        // TODO: We should flip it so that the Trie stores the Target,
        // and the Dag stores the target chord to index into the trie.
        // This would be more efficient and we won't need the helpers
        // we currently have for finding a Target given a target_name.
        let mut trie: Trie<Chord, Vec<NodeIndex<NIdx>>> = Trie::new();

        // Seed the queue with the root target cfgs:
        target_queue.extend(cfg.targets.iter());

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        while !target_queue.is_empty() {
            let queue_len = target_queue.len();
            for _ in 0..queue_len {
                // TODO: Maybe move the loop body into a function.
                // The while loop condition guarantees this.
                let target_cfg: &TargetCfg = target_queue.pop_front().unwrap();
                Self::validate_target_cfg(target_cfg, &node_idxes)?;

                let target = Target::from(target_cfg);
                let target_chord = target.chord.clone();

                // Walk the dependencies.

                // This is for recording the deps for edge
                // construction later...
                // TODO: The subtarget should have its name auto-prepended with its parent target.
                if let Some(targets) = &target_cfg.targets {
                    for subtarget in targets.into_iter() {
                        deps.push((target.name, &subtarget.name));
                        target_queue.push_back(subtarget);
                    }
                }

                // ...but also to add subtargets to
                // the queue so we can exhaustively visit all the
                // targets in the config. Subtargets are also added as
                // deps too, since they implicitly represent
                // dependencies.
                if let Some(target_deps) = &target_cfg.deps {
                    for dep in target_deps {
                        deps.push((target.name, dep));
                    }
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

                // Record the root node indexes.
                // This will only fill up to the amount of the _first_
                // value of queue_len, which is going to be the number
                // of nodes at the top level of the BFS -- this is
                // precisely the set of root nodes, aka, the top-level
                // targets.
                if root_targets.len() < queue_len {
                    root_targets.push(node_idx);
                }

                // And at last, record the mapping of target name to
                // its node index.
                node_idxes.insert(&target_cfg.name, node_idx);
            }
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
            root_targets,
            dag,
            chords: trie,
        })
    }

    fn validate_target_cfg(
        cfg: &TargetCfg,
        node_idxes: &HashMap<&str, NodeIndex<NIdx>>,
    ) -> Result<()> {
        // TODO: This should be validating short names too.
        if cfg.name.len() == 0 {
            bail!("cannot have an empty target name")
        } else if cfg.name.contains(".") {
            // TODO: This and its sibling, '?', should have
            // its checks changed to be more permissive --
            // they're only not allowed _after_ a delimiter,
            // as that's when they cause ambiguity. Anywhere
            // else is actually fine. e.g. foo.bar-baz is
            // actually fine!
            bail!("cannot have a '.' in a target name: '{}'", cfg.name)
        } else if cfg.name.contains("?") {
            bail!("cannot have a '?' in a target name: '{}'", cfg.name)
        } else if node_idxes.contains_key(&cfg.name as &str) {
            bail!("duplicate target name: '{}'", cfg.name)
        } else if cfg.deps.is_none() && cfg.targets.is_none() && cfg.cmd.is_none() {
            bail!("a command without an executable command must have dependencies or subtargets, but '{}' does not", cfg.name)
        }

        Ok(())
    }

    fn nonexistent_dep_err<T: AsRef<str> + std::fmt::Display>(dep_name: T) -> anyhow::Error {
        anyhow!("reference to nonexistent dep: {}", dep_name)
    }

    fn no_cmd_for_chord(chord: &Chord) -> anyhow::Error {
        anyhow!("no command for given chord: '{}'", chord)
    }

    fn ambiguous_chord(&self, chord: &Chord, nidxes: &Vec<NodeIndex<NIdx>>) -> anyhow::Error {
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

    pub fn play(&self, chord: Chord) -> Result<()> {
        let nidxes = self
            .chords
            .get(&chord)
            .ok_or(Jam::no_cmd_for_chord(&chord))?; // TODO: We can do better with the error message here.
        if nidxes.len() > 1 {
            return Err(self.ambiguous_chord(&chord, nidxes));
        }
        // TODO: We need to handle conflicts here.
        let target = &self.dag[*nidxes.first().unwrap()];
        println!("found target for chord '{}': '{}'", chord, target.name);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parse {
        use super::*;

        use daggy::Walker;

        use crate::config::Options;

        impl<'a> Jam<'a> {
            fn node_has_target(
                &self,
                nidx: &NodeIndex<NIdx>,
                target_name: &str,
            ) -> Option<NodeIndex<NIdx>> {
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

            fn get_targets_by_chord(&self, chord: Chord) -> Option<&Vec<NodeIndex<NIdx>>> {
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
                        Some((depender_idx, dependent_idx)) => {
                            self.dag.find_edge(depender_idx, dependent_idx).is_some()
                        }
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

        impl Config {
            fn with_targets(targets: Vec<TargetCfg>) -> Config {
                Config {
                    options: Options {},
                    targets,
                }
            }
        }

        fn get_jam<'a>(cfg: &'a Config) -> Jam<'a> {
            Jam::parse(cfg).expect("expected no errors from parsing")
        }

        fn check_jam_err(targets: Vec<TargetCfg>, expected_err: &str) {
            let cfg = Config {
                options: Options {},
                targets,
            };
            if let Err(err) = Jam::parse(&cfg) {
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
                    .for_each(|name| assert!(jam.has_target(&name)));
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
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["foo", "bar"],
                    &vec![("foo", "bar")],
                );
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
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["foo", "bar", "quux"],
                    &vec![("foo", "bar"), ("foo", "quux")],
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
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["a", "b", "c", "d"],
                    &vec![("a", "c"), ("b", "d")],
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
                    target::dep("b", vec!["c"]),
                ]);
                let subjam = get_jam(&subcfg);
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
                let cfg = Config::with_targets(vec![target::lone("foo")]);
                let jam = get_jam(&cfg);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["f"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                )
            }

            #[test]
            fn automatic_of_double_word_is_first_char() {
                let cfg = Config::with_targets(vec![target::lone("foo-bar")]);
                let jam = get_jam(&cfg);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["f", "b"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                );
            }

            #[test]
            fn automatic_of_multi_word_is_first_char() {
                let cfg = Config::with_targets(vec![target::lone("foo-bar-baz-quux")]);
                let jam = get_jam(&cfg);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["f", "b", "b", "q"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    1,
                );
            }

            #[test]
            fn automatic_of_multiple_non_conflicting_targets() {
                let cfg = Config::with_targets(vec![
                    target::lone("foo"),
                    target::lone("bar"),
                    target::lone("quux"),
                ]);
                let jam = get_jam(&cfg);
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
                let cfg = Config::with_targets(vec![
                    target::lone("bar"),
                    target::lone("baz"),
                    target::lone("bam"),
                    target::lone("barr"),
                ]);
                let jam = get_jam(&cfg);
                assert_eq!(
                    jam.get_targets_by_chord(Chord::new(vec!["b"]))
                        .expect("expected to find the target by its expected chord")
                        .len(),
                    4,
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
                let jam = get_jam(&cfg);
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
