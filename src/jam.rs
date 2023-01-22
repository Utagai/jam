use std::collections::{HashMap, VecDeque};

use anyhow::Result;
use daggy::{Dag, NodeIndex, Walker};

use crate::config::{Config, TargetCfg};

// TODO: Exercise - can we use &str in any of these fields?
struct Target {
    name: String,
    shortname: String,
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
            // TODO: The shortname needs to be computed, but right now it is just the same as the long name.
            shortname: value.shortname.clone().unwrap_or(value.name.clone()),
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
    // TODO: This function should be doing validation.
    // e.g.
    // 1. checking for cycles
    // 2. duplicate target names
    // 3. etc
    pub fn parse(cfg: Config) -> Result<Jam> {
        let mut exec_dag: Dag<Target, u32> = Dag::new();
        // TODO: I wonder if we can use NodeIndex<String> and just use
        // the target name to find a particular node, making the
        // node_idxes map unnecessary?
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
                node_idxes.insert(target.name.clone(), node_idx);
            }
            iterating_roots = false; // TODO: Cleaner way to write this?
        }

        // With their dependencies recorded, now add edges to the DAG to represent them:
        for dep in deps {
            // TODO: So we are relying on default `?` behavior to
            // propagate a cycle detection error here, but we likely
            // want to catch this and return a better error.  I
            // believe anyhow may have a way of annotating this error
            // or maybe even replacing it...  Or perhaps we do need to
            // get thiserror.
            exec_dag.add_edge(
                *node_idxes.get(&dep.0).unwrap(),
                *node_idxes.get(&dep.1).unwrap(),
                0,
            )?;
        }

        Ok(Jam { roots, exec_dag })
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

            fn has_target(&self, target_name: &str) -> bool {
                for root in &self.roots {
                    println!("looking @ {:?}", root);
                    if self.node_has_target(root, target_name).is_some() {
                        return true;
                    }
                }

                return false;
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
    }
}
