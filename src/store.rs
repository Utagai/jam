use anyhow::{anyhow, bail};
use daggy::{Dag, NodeIndex};
use sequence_trie::SequenceTrie;
use slog::{debug, info, o};
use std::{collections::HashMap, slice::Iter};
use thiserror::Error;

use crate::{
    config::{DesugaredConfig, DesugaredTargetCfg},
    executor::ExecuteKind,
    reconciler::{reconcile, Strategy},
};

// TODO: The fields here likely don't need to be pub eventually.
pub struct Target<'a> {
    pub name: &'a str,
    pub shortcut: Shortcut,
    pub help: &'a str,
    pub cmd: Option<&'a str>,
    pub execute_kind: ExecuteKind,
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

#[derive(Eq, Debug, PartialOrd, Ord)]
pub struct Shortcut(pub Vec<char>);

impl Shortcut {
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

    // NOTE: The same thing as described for append() above is worth considering
    // here as well.
    pub fn prepend(&self, key: &char) -> Shortcut {
        let mut new_vec = vec![*key];
        new_vec.extend_from_slice(&self.0);
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

impl From<Vec<&char>> for Shortcut {
    fn from(value: Vec<&char>) -> Self {
        Shortcut(value.iter().map(|c| **c).collect())
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

#[derive(Debug, PartialEq)]
pub enum Lookup {
    NotFound,
    Found,
    ReconciliationFailure(String),
    Conflict,
}

pub enum NextKey<'a> {
    LeafKey { key: char, target_name: &'a str },
    BranchKey { key: char },
}

impl<'a> NextKey<'a> {
    pub fn new(key: char, target_names: Vec<&'a str>) -> NextKey<'a> {
        if target_names.len() == 1 {
            NextKey::LeafKey {
                key,
                target_name: target_names[0],
            }
        } else {
            NextKey::BranchKey { key }
        }
    }

    pub fn key(&self) -> char {
        match self {
            NextKey::LeafKey { key, .. } => *key,
            NextKey::BranchKey { key } => *key,
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum ExecError {
    #[error("given shortcut '{shortcut}' is ambiguous (i.e. is it {conflict_msg}?)")]
    Conflict {
        shortcut: Shortcut,
        conflict_msg: String,
    },
    #[error("no command for given shortcut '{shortcut}'")]
    ShortcutNotFound { shortcut: Shortcut },
    #[error("no command for given target name '{target_name}'")]
    TargetNotFound { target_name: String },
    #[error("target '{name}' has no executable function")]
    CannotExec { name: String },
    #[error("{description}")]
    Reconciliation { description: String },
    #[error("{description}")]
    Executor { description: String },
    #[error("failed to execute dependency ('{dep_name}'): {err}")]
    Dependency {
        dep_name: String,
        err: Box<ExecError>,
    },
}

// ParseResult is not really very useful at the moment. However, it may be
// something that we wish to embellish further in the future. This type can
// allow us to do that and save a bit of keystrokes. More importantly though, it
// lets us split the API into two distinct classes errors: parsing errors &
// execution errors.
pub type ParseResult<T> = anyhow::Result<T, anyhow::Error>;
// TODO: Maybe we just move all of this to jam.rs? The key value-add of the work is to just decouple the store implementation from the parsing/execution implementaiton (i.e. jam).
pub type ExecResult<T> = Result<T, ExecError>;

pub(crate) trait TargetStore<'a> {
    fn new(logger: &slog::Logger, targets: &'a Vec<DesugaredTargetCfg>) -> ParseResult<Self>
    where
        Self: Sized;
    fn mappings(&self, strategy: Strategy) -> ExecResult<Vec<(Shortcut, &str)>>;
    // TODO: Shoudl we keep this as "Exec" result?
    fn next(&self, prefix: &Shortcut, strategy: Option<Strategy>) -> ExecResult<Vec<NextKey>>;
    fn lookup(&self, shortcut: &Shortcut) -> Lookup;
    fn execute_by_shortcut(&self, shortcut: &Shortcut) -> ExecResult<()>;
    fn execute_by_name(&self, name: &str) -> ExecResult<()>;
}

// TODO: Lot of pubs in this file and I don't think we need them all.
pub type IdxT = u32;
pub type NodeIdx = NodeIndex<IdxT>;
// NOTE: I wonder if we actually really need a trie. I think the more general
// N-ary tree would work just as well given how we use this trie. We're not
// actually making good use of the strengths of a trie, I think. In either case,
// it shouldn't really change much in the code, even in terms of complexity, so
// I'm just going to keep it here until I find a good enough reason to refactor.
// I mean really, for the sort of scales that this binary is executing at, we
// can probably just dump everything into a vector and be more than fast enough.
// NOTE: The value-type here is Vec<NodeIdx>. This is because a single shortcut
// can lead to multiple targets if it is ambiguous (e.g. a partial prefix).
pub type ShortcutTrie = SequenceTrie<char, Vec<NodeIdx>>;

struct TrieDagStore<'a> {
    shortcuts: ShortcutTrie,
    dag: Dag<Target<'a>, IdxT>,
    logger: slog::Logger,
}

impl<'a> TargetStore<'a> for TrieDagStore<'a> {
    fn new(logger: &slog::Logger, targets: &'a Vec<DesugaredTargetCfg>) -> ParseResult<Self> {
        let mut dag: Dag<Target, IdxT> = Dag::new();
        let mut deps: Vec<(&str, &str)> = Vec::new();
        let mut node_idxes: HashMap<&str, NodeIdx> = HashMap::new();
        let mut trie: ShortcutTrie = SequenceTrie::new();

        // Discover all targets & add them as nodes to the DAG, and record their dependencies.
        // While we are doing this, we can also add the long and short names to their respective tries.
        for target_cfg in targets {
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

        Ok(TrieDagStore {
            // NOTE: If we ever end up having a distinction between
            // config-time options vs. run-time options, we may want
            // to create a separate type here for isolation.
            dag,
            shortcuts: trie,
            logger: logger.new(o!()),
        })
    }

    fn mappings(&self, strategy: Strategy) -> ExecResult<Vec<(Shortcut, &str)>> {
        let mut mappings: Vec<(Shortcut, &str)> = self
            .shortcuts
            .iter()
            .map(|(shortcut, nidxes)| -> ExecResult<Vec<(Shortcut, &str)>> {
                let shortcut = Shortcut::from(shortcut);
                if nidxes.len() == 1 {
                    return Ok(vec![(shortcut, self.dag[nidxes[0]].name)]);
                }

                // If there are multiple though, we should reconcile and return
                // the shortcuts with the reconciled character added.
                let reconciliation_chars = self.reconcile(strategy, &shortcut, &nidxes)?;
                reconciliation_chars
                    .iter()
                    .enumerate()
                    .map(|(i, c)| Ok((shortcut.append(c), self.dag[nidxes[i]].name)))
                    .collect()
            })
            .collect::<ExecResult<Vec<Vec<(Shortcut, &str)>>>>()?
            .into_iter()
            .flatten() // Flattens the Vec<Vec<...>> into Vec<...>.
            .collect();

        // Sort so that the return value is deterministic.
        mappings.sort();
        Ok(mappings)
    }

    fn next(&self, prefix: &Shortcut, strategy: Option<Strategy>) -> ExecResult<Vec<NextKey>> {
        if let Some(strategy) = strategy {
            return self.next_conflict(strategy, prefix);
        } else {
            return self.next_no_conflict(prefix);
        }
    }

    fn lookup(&self, shortcut: &Shortcut) -> Lookup {
        todo!()
    }

    fn execute_by_shortcut(&self, shortcut: &Shortcut) -> ExecResult<()> {
        todo!()
    }

    fn execute_by_name(&self, name: &str) -> ExecResult<()> {
        todo!()
    }
}

pub fn validate_target_cfg(
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

impl<'a> TrieDagStore<'a> {
    fn reconcile(
        &self,
        strategy: Strategy,
        shortcut: &Shortcut,
        conflicts: &[NodeIdx],
    ) -> ExecResult<Vec<char>> {
        match reconcile(
            strategy,
            &self.shortcuts,
            &conflicts
                .iter()
                .map(|nidx| self.dag[*nidx].name)
                .collect::<Vec<&str>>(),
            shortcut,
        ) {
            Ok(keys) => Ok(keys),
            Err(err) => Err(ExecError::Reconciliation {
                description: err.to_string(),
            }),
        }
    }

    // TODO: Probably these two should be required methods and then next() is auto-implemented.
    fn next_conflict(&self, strategy: Strategy, prefix: &Shortcut) -> ExecResult<Vec<NextKey>> {
        let nidxes = self
            .shortcuts
            .get(prefix.iter())
            .ok_or(ExecError::ShortcutNotFound {
                shortcut: prefix.clone(),
            })?;
        self.reconcile(strategy, prefix, nidxes).and_then(|keys| {
            keys.into_iter()
                .map(|key| {
                    self.resolve_shortcut(strategy, &prefix.append(&key))
                        .map(|nidxes| {
                            NextKey::new(
                                key,
                                nidxes.iter().map(|idx| self.dag[*idx].name).collect(),
                            )
                        })
                })
                .collect::<ExecResult<Vec<NextKey>>>()
        })
    }

    /// resolve_shortcut takes a shortcut and returns the target(s) that it
    /// refers to by their node indexes. This method is special because it is
    /// capable of recognizing shortcuts with reconciliation characters, and
    /// deducing what target the given reconciled shortcut refers to.
    fn resolve_shortcut(
        &self,
        strategy: Strategy,
        shortcut: &Shortcut,
    ) -> ExecResult<Vec<NodeIdx>> {
        let shortcuts_ref = &self.shortcuts;
        let target_idxes;
        match shortcuts_ref.get(shortcut.iter()) {
            Some(nidxes) => target_idxes = nidxes.to_vec(),
            None => {
                debug!(
                    self.logger,
                    "found potential reconciliation character, attempting reconciliation"
                );
                // In this case, it is possible that the user has
                // actually specified a reconciliation character,
                // which won't exist in the trie until we
                // reconcile. So let's do that.
                let (tail, key) = shortcut.tail();
                // To see if this is correct, first, we expect the tail to exist:
                match shortcuts_ref.get(tail.iter()) {
                    Some(nidxes) => {
                        // Secondly, not only must it exist, but it
                        // must be referring to a shortcut that has
                        // conflicts. If it has none, then the user
                        // should just specify the tail.
                        if nidxes.len() <= 1 {
                            return Err(ExecError::ShortcutNotFound {
                                shortcut: Shortcut::from(shortcut),
                            });
                        }

                        info!(self.logger, "reconciling with strategy"; o!("strategy" => strategy.to_string()));

                        // If it does indeed have a conflict,
                        // then let's make sure that the specified
                        // reconciliation key is a valid one:
                        let reconciliation_keys = self.reconcile(strategy, &tail, nidxes)?;

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
                                return Err(ExecError::ShortcutNotFound {
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
                        return Err(ExecError::ShortcutNotFound {
                            shortcut: Shortcut::from(shortcut),
                        });
                    }
                }
            }
        };
        Ok(target_idxes)
    }

    fn next_no_conflict(&self, prefix: &Shortcut) -> ExecResult<Vec<NextKey>> {
        // If we're not in conflict, then we can just look up the prefix in the trie directly:
        let subtrie = self.shortcuts.get_node(prefix.iter());
        if let Some(subtrie) = subtrie {
            let mut keys_to_names: Vec<NextKey> = subtrie
                .children_with_keys()
                .iter()
                .filter_map(|(key, _)| {
                    // For this key, get the target names that that it leads to.
                    let names = self
                        .shortcuts
                        .get_node(prefix.append(*key).iter())
                        .expect("failed to look up non-conflict shortcut")
                        .values()
                        .flatten()
                        .map(|v| self.dag[*v].name)
                        .collect();
                    Some(NextKey::new(**key, names))
                })
                .collect();
            // Because the keys we return are individual _characters_ of
            // _full_ sequences, it is possible to return duplicates
            // here. For example, consider the following shortcuts to
            // exist:
            //   a
            //   ab
            // The first call with the prefix being the empty shortcut will
            // return [a,a] unless we de-dupe things.
            keys_to_names.sort_by(|a, b| a.key().cmp(&b.key()));
            keys_to_names.dedup_by(|a, b| a.key() == b.key());

            // Now return the keys, mapped to their next target names.
            Ok(keys_to_names)
        } else {
            // If there is no subtrie for this prefix, then there
            // should be no next_keys, there's nothing to extend it
            // with.
            Ok(vec![])
        }
    }
}
