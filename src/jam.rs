use std::collections::{HashMap, VecDeque};

use anyhow::{anyhow, bail, Result};
use daggy::Walker;
use daggy::{Dag, NodeIndex};

use crate::config::{Config, TargetCfg};

// TODO: Exercise - can we use &str in any of these fields?
struct Target {
    name: String,
    shortname: String,
    help: String,
    cmd: Option<String>,
}

fn name_to_short<T: AsRef<str>>(name: T) -> String {
    // TODO: Eventually, this delimiter should be configured.
    name.as_ref()
        .split("-")
        .map(|segment| segment.chars().nth(0).unwrap().to_string())
        .collect::<Vec<String>>()
        .join("-")
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
            // TODO: The shortname needs to be computed, but right now it is just the same as the long name.
            shortname: value
                .shortname
                .clone()
                .unwrap_or(name_to_short(&value.name)),
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
    roots: Vec<NodeIndex<u32>>,
    exec_dag: Dag<Target, u32>,
}

impl Jam {
    pub fn parse(cfg: Config) -> Result<Jam> {
        let mut exec_dag: Dag<Target, u32> = Dag::new();
        let mut node_idxes: HashMap<String, NodeIndex<u32>> = HashMap::new();
        let mut target_queue: VecDeque<&TargetCfg> = VecDeque::new();
        let mut deps: Vec<(String, String)> = Vec::new();
        let mut roots: Vec<NodeIndex<u32>> = Vec::new();

        target_queue.extend(cfg.targets.iter());

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        let mut iterating_roots = true;
        while !target_queue.is_empty() {
            let queue_len = target_queue.len();
            for _ in 0..queue_len {
                let target: &TargetCfg = target_queue.pop_front().unwrap(); // The while loop condition guarantees this.
                println!("Looking at target: {}", target.name);

                // Target name validations:
                if target.name.len() == 0 {
                    bail!("cannot have an empty target name")
                } else if target.name.contains(".") {
                    // TODO: This and its sibling, '?', should have
                    // its checks changed to be more permissive --
                    // they're only not allowed _after_ a delimiter,
                    // as that's when they cause ambiguity. Anywhere
                    // else is actually fine. e.g. foo.bar-baz is
                    // actually fine!
                    bail!("cannot have a '.' in a target name: '{}'", target.name)
                } else if target.name.contains("?") {
                    bail!("cannot have a '?' in a target name: '{}'", target.name)
                }

                let node_idx = exec_dag.add_node(Target::from(target));
                if iterating_roots {
                    roots.push(node_idx.clone());
                }
                if let Some(targets) = &target.targets {
                    for subtarget in targets {
                        target_queue.push_back(subtarget);
                        deps.push((target.name.clone(), subtarget.name.clone()))
                    }
                }

                if let Some(target_deps) = &target.deps {
                    for dep in target_deps {
                        deps.push((target.name.clone(), dep.clone()));
                    }
                }
                if node_idxes.contains_key(&target.name) {
                    bail!("duplicate target name: '{}'", target.name)
                }
                node_idxes.insert(target.name.clone(), node_idx);
            }
            iterating_roots = false; // TODO: Cleaner way to write this?
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

        Ok(Jam { roots, exec_dag })
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
                if self.exec_dag[*nidx].name == target_name {
                    return Some(*nidx);
                }

                self.exec_dag
                    .children(*nidx)
                    .iter(&self.exec_dag)
                    .map(|(_, n)| self.node_has_target(&n, target_name))
                    .find(|n| n.is_some())
                    .flatten()
            }

            fn get_target(&self, target_name: &str) -> Option<&Target> {
                self.roots
                    .iter()
                    .map(|root| self.node_has_target(root, target_name))
                    .find(|found_node| found_node.is_some())
                    .flatten()
                    .map(|node_idx| &self.exec_dag[node_idx])
            }

            fn has_target(&self, target_name: &str) -> bool {
                self.get_target(target_name).is_some()
            }

            fn has_dep(&self, dependee: &str, dependent: &str) -> bool {
                for root in &self.roots {
                    let depender_idx = self.node_has_target(root, dependee);
                    let dependent_idx = self.node_has_target(root, dependent);
                    if match depender_idx.zip(dependent_idx) {
                        Some((depender_idx, dependent_idx)) => self
                            .exec_dag
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
                    shortname: None,
                    help: None,
                    cmd: None,
                    targets: None,
                    deps: None,
                }
            }

            pub fn dep(name: &str, deps: Vec<&str>) -> TargetCfg {
                TargetCfg {
                    name: String::from(name),
                    shortname: None,
                    help: None,
                    cmd: None,
                    targets: None,
                    deps: Some(deps.iter().map(|dep| String::from(*dep)).collect()),
                }
            }

            pub fn sub(name: &str, subs: Vec<TargetCfg>) -> TargetCfg {
                TargetCfg {
                    name: String::from(name),
                    shortname: None,
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

            assert_eq!(jam.exec_dag.node_count(), targets.len());
            assert_eq!(jam.exec_dag.edge_count(), deps.len());
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
                let expected_target_names = vec!["foo", "bar", "baz"];
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
                    target::lone("baz"),
                    target::dep("foo", vec!["bar", "baz"]),
                ]);

                let subjam = get_jam(vec![target::sub(
                    "foo",
                    vec![target::lone("bar"), target::lone("baz")],
                )]);
                verify_jam_dags(
                    vec![depjam, subjam],
                    &vec!["foo", "bar", "baz"],
                    &vec![("foo", "bar"), ("foo", "baz")],
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

            #[test]
            fn automatic_of_single_word_is_first_char() {
                let jam = get_jam(vec![target::lone("foo")]);
                assert_eq!(
                    jam.get_target("foo")
                        .expect("expected to find the 'foo' target")
                        .shortname,
                    "f"
                )
            }

            #[test]
            fn automatic_of_double_word_is_first_char() {
                let jam = get_jam(vec![target::lone("foo-bar")]);
                assert_eq!(
                    jam.get_target("foo-bar")
                        .expect("expected to find the 'foo' target")
                        .shortname,
                    "f-b"
                )
            }

            #[test]
            fn automatic_of_multi_word_is_first_char() {
                let jam = get_jam(vec![target::lone("foo-bar-baz-quux")]);
                assert_eq!(
                    jam.get_target("foo-bar-baz-quux")
                        .expect("expected to find the 'foo' target")
                        .shortname,
                    "f-b-b-q"
                )
            }

            #[test]
            fn override_respected() {
                let jam = get_jam(vec![TargetCfg {
                    name: String::from("foo"),
                    shortname: Some(String::from("x")),
                    help: None,
                    cmd: None,
                    targets: None,
                    deps: None,
                }]);
                assert_eq!(
                    jam.get_target("foo")
                        .expect("expected to find the 'foo' target")
                        .shortname,
                    "x"
                )
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
                        target::dep("foo", vec!["bar"]),
                        target::dep("bar", vec!["quux"]),
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
