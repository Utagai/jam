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

    use crate::config::Options;

    impl Jam {
        fn node_has_target(&self, nidx: &NodeIndex<u32>, target_name: &str) -> bool {
            self.exec_dag[*nidx].name == target_name
                || self
                    .exec_dag
                    .children(*nidx)
                    .iter(&self.exec_dag)
                    .any(|(_, n)| {
                        self.exec_dag[n].name == target_name
                            || self.node_has_target(&n, target_name)
                    })
        }

        fn has_target(&self, target_name: &str) -> bool {
            for root in &self.roots {
                println!("looking @ {:?}", root);
                if self.node_has_target(root, target_name) {
                    return true;
                }
            }

            return false;
        }
    }

    fn lone(name: &str) -> TargetCfg {
        TargetCfg {
            name: String::from(name),
            shortname: None,
            help: None,
            cmd: None,
            targets: None,
            deps: None,
        }
    }

    #[test]
    fn parse_single_target() {
        let expected_target_name = "foo";
        let cfg = Config {
            options: Options {},
            targets: vec![lone(expected_target_name)],
        };
        let jam = Jam::parse(cfg).expect("expected no errors from parsing");
        assert!(jam.has_target(expected_target_name));
        assert_eq!(jam.exec_dag.node_count(), 1);
        assert_eq!(jam.exec_dag.edge_count(), 0);
    }

    #[test]
    fn parse_zero_targets() {
        let cfg = Config {
            options: Options {},
            targets: vec![],
        };
        let jam = Jam::parse(cfg).expect("expected no errors from parsing");
        assert_eq!(jam.exec_dag.node_count(), 0);
        assert_eq!(jam.exec_dag.edge_count(), 0);
    }

    #[test]
    fn parse_multiple_targets() {
        let expected_target_names = vec!["foo", "bar", "baz"];
        let cfg = Config {
            options: Options {},
            targets: expected_target_names
                .iter()
                .map(|name| lone(name))
                .collect(),
        };
        let jam = Jam::parse(cfg).expect("expected no errors from parsing");
        expected_target_names
            .iter()
            .for_each(|name| assert!(jam.has_target(&name)));
        assert_eq!(jam.exec_dag.node_count(), 3);
        assert_eq!(jam.exec_dag.edge_count(), 0);
    }
}
