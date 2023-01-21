use std::collections::{HashMap, VecDeque};

use anyhow::Result;
use daggy::{Dag, NodeIndex};

use crate::config::{Config, TargetCfg};

// TODO: Exercise - can we use &str in any of these fields?
// TODO: We need to reconcile the naming of 'Target' vs 'Task'.
struct Task {
    name: String,
    shortname: String,
    help: String,
    cmd: Option<String>,
}

impl From<&TargetCfg> for Task {
    fn from(value: &TargetCfg) -> Self {
        // TODO: The clone() calls here may go away if we manage to
        // use &str in the TaskCfg instead...  Right now they are all
        // String, which we can't take without moving or
        // cloning. Moving can't be done here since we are From'ing
        // from a shared reference, so we have to clone.
        return Task {
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
    exec_dag: Dag<Task, u32>,
}

impl Jam {
    // TODO: This function should be doing validation.
    // e.g.
    // 1. checking for cycles
    // 2. duplicate target/task names
    // 3. etc
    pub fn parse(cfg: Config) -> Result<Jam> {
        let mut exec_dag: Dag<Task, u32> = Dag::new();
        // TODO: I wonder if we can use NodeIndex<String> and just use
        // the task name to find a particular node, making the
        // node_idxes map unnecessary?
        let mut node_idxes: HashMap<String, NodeIndex<u32>> = HashMap::new();
        let mut target_queue: VecDeque<&TargetCfg> = VecDeque::new();
        let mut deps: Vec<(String, String)> = Vec::new();

        target_queue.extend(cfg.targets.iter());

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        while !target_queue.is_empty() {
            let target: &TargetCfg = target_queue.pop_front().unwrap(); // The while loop condition guarantees this.
            println!("Looking at target: {}", target.name);
            let node_idx = exec_dag.add_node(Task::from(target));
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

        Ok(Jam { exec_dag })
    }
}
