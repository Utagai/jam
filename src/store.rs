use anyhow::{anyhow, bail};
use daggy::{Dag, NodeIndex, Walker};
use sequence_trie::SequenceTrie;
use slog::{debug, info, o};
use std::{collections::HashMap, slice::Iter};
use thiserror::Error;

use crate::{
    config::DesugaredTargetCfg,
    executor::ExecuteKind,
    reconciler::{reconcile, Strategy},
};

pub struct Target<'a> {
    pub name: &'a str,
    shortcut: Shortcut,
    help: &'a str,
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
    fn tail(&self) -> (Shortcut, Option<char>) {
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
    fn lookup(&self, strategy: Option<Strategy>, shortcut: &Shortcut) -> Lookup;
    // TODO: Should be able to autoimplement a more ergonomic method?
    fn get_by_shortcut(&self, strategy: Strategy, shortcut: Shortcut) -> ExecResult<&Target>;
    // NOTE: get_by_target_name does not require a reconciliation strategy
    // because target_names are globally unique, so there is no possibility of
    // conflict to reconcile.
    fn get_by_target_name(&self, target_name: &str) -> ExecResult<&Target>;
    fn children(&self, target: &Target) -> ExecResult<Vec<&Target>>;
}

type IdxT = u32;
type NodeIdx = NodeIndex<IdxT>;
// NOTE: I wonder if we actually really need a trie. I think the more general
// N-ary tree would work just as well given how we use this trie. We're not
// actually making good use of the strengths of a trie, I think. In either case,
// it shouldn't really change much in the code, even in terms of complexity, so
// I'm just going to keep it here until I find a good enough reason to refactor.
// I mean really, for the sort of scales that this binary is executing at, we
// can probably just dump everything into a vector and be more than fast enough.
// NOTE: The value-type here is Vec<NodeIdx>. This is because a single shortcut
// can lead to multiple targets if it is ambiguous (e.g. a partial prefix).
type ShortcutTrie = SequenceTrie<char, Vec<NodeIdx>>;

pub struct TrieDagStore<'a> {
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

    fn lookup(&self, strategy: Option<Strategy>, shortcut: &Shortcut) -> Lookup {
        if let Some(strategy) = strategy {
            match self.resolve_shortcut(strategy, shortcut) {
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
                Err(ExecError::Conflict { .. }) => Lookup::Conflict, // NOTE: This doesn't ever happen, it's here just in case.
                Err(ExecError::Reconciliation { description }) => {
                    Lookup::ReconciliationFailure(description)
                }
                Err(_) => Lookup::NotFound,
            }
        } else {
            if let Some(nidxes) = self.shortcuts.get(shortcut.iter()) {
                if nidxes.len() == 1 {
                    Lookup::Found
                } else {
                    Lookup::Conflict
                }
            } else {
                Lookup::NotFound
            }
        }
    }

    fn get_by_shortcut(&self, strategy: Strategy, shortcut: Shortcut) -> ExecResult<&Target> {
        let target_idxes = self.resolve_shortcut(strategy, &shortcut)?;
        if target_idxes.len() > 1 {
            return Err(ExecError::Conflict {
                shortcut,
                conflict_msg: target_idxes
                    .iter()
                    .map(|idx| format!("'{}'", self.dag[*idx].name))
                    .collect::<Vec<String>>()
                    .join(" or "),
            });
        }
        if let Some(nidx) = target_idxes.first() {
            return Ok(&self.dag[*nidx]);
        } else {
            Err(ExecError::ShortcutNotFound { shortcut })
        }
    }

    fn get_by_target_name(&self, target_name: &str) -> ExecResult<&Target> {
        let nidx = self
            .shortcuts
            .values()
            .flatten()
            .find(|nidx| self.dag[**nidx].name == target_name)
            .ok_or(ExecError::TargetNotFound {
                target_name: target_name.to_string(),
            })?;
        return Ok(&self.dag[*nidx]);
    }

    fn children(&self, target: &Target) -> ExecResult<Vec<&Target>> {
        // TODO: This is inefficient, but we will be replacing this with a simpler implementation anyways.
        let nidx = self
            .shortcuts
            .values()
            .flatten()
            .find(|nidx| self.dag[**nidx].name == target.name)
            .ok_or(ExecError::TargetNotFound {
                target_name: target.name.to_string(),
            })?;
        self.dag
            .children(*nidx)
            .iter(&self.dag)
            .map(|child| Ok(&self.dag[child.1]))
            .collect()
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

impl<'a> TrieDagStore<'a> {
    fn reconcile(
        &self,
        strategy: Strategy,
        shortcut: &Shortcut,
        conflicts: &[NodeIdx],
    ) -> ExecResult<Vec<char>> {
        match reconcile(
            strategy,
            self,
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

#[cfg(test)]
mod tests {
    use super::TrieDagStore;
    use crate::config::DesugaredConfig;
    use crate::store::{NodeIdx, Target, TargetStore};
    use crate::testutils::logger;

    fn get_ts(cfg: &DesugaredConfig) -> TrieDagStore {
        TrieDagStore::new(&logger::test(), &cfg.targets).expect("expected no errors from parsing")
    }

    mod parse {
        use super::*;

        use crate::config::target;
        use crate::config::{Config, Options, TargetCfg};
        use crate::log::Level;

        // TODO: This should probably test across all impls at once?
        impl<'a> TrieDagStore<'a> {
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

        fn check_ts_err(targets: Vec<TargetCfg>, expected_err: &str) {
            let cfg = Config {
                options: Options {
                    reconciliation_strategy: crate::reconciler::Strategy::Error,
                    log_level: Some(Level::Disabled),
                },
                imports: Some(vec![]),
                targets,
            }
            .desugar();
            if let Err(err) = TrieDagStore::new(&logger::test(), &cfg.targets) {
                assert_eq!(format!("{err}").trim(), expected_err)
            } else {
                panic!("expected an error from parsing, but got none")
            }
        }

        fn verify_ts_dag(ts: TrieDagStore, targets: &Vec<&str>, deps: &Vec<(&str, &str)>) {
            for target in targets {
                assert!(ts.has_target(target));
            }

            for dep in deps {
                assert!(ts.has_dep(dep.0, dep.1));
            }

            assert_eq!(ts.dag.node_count(), targets.len());
            assert_eq!(ts.dag.edge_count(), deps.len());
        }

        mod nodeps {
            use super::*;

            #[test]
            fn single_target() {
                let expected_target_name = "foo";
                let cfg = Config::with_targets(vec![target::lone(expected_target_name)]);
                let ts = get_ts(&cfg);
                verify_ts_dag(ts, &vec![expected_target_name], &vec![]);
            }

            #[test]
            fn zero_targets() {
                let cfg = Config::with_targets(vec![]);
                let ts = get_ts(&cfg);
                verify_ts_dag(ts, &vec![], &vec![]);
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
                let ts = get_ts(&cfg);
                expected_target_names
                    .iter()
                    .for_each(|name| assert!(ts.has_target(name)));
                verify_ts_dag(ts, &expected_target_names, &vec![]);
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
                let depts = get_ts(&depcfg);
                let subcfg =
                    Config::with_targets(vec![target::sub("foo", vec![target::lone("bar")])]);
                let subts = get_ts(&subcfg);
                verify_ts_dag(depts, &vec!["foo", "bar"], &vec![("foo", "bar")]);
                verify_ts_dag(subts, &vec!["foo", "foo-bar"], &vec![("foo", "foo-bar")]);
            }

            #[test]
            fn one_target_two_dependents() {
                let depcfg = Config::with_targets(vec![
                    target::lone("bar"),
                    target::lone("quux"),
                    target::dep("foo", vec!["bar", "quux"]),
                ]);
                let depts = get_ts(&depcfg);

                let subcfg = Config::with_targets(vec![target::sub(
                    "foo",
                    vec![target::lone("bar"), target::lone("quux")],
                )]);
                let subts = get_ts(&subcfg);
                verify_ts_dag(
                    depts,
                    &vec!["foo", "bar", "quux"],
                    &vec![("foo", "bar"), ("foo", "quux")],
                );
                verify_ts_dag(
                    subts,
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
                let ts = get_ts(&cfg);
                verify_ts_dag(ts, &vec!["foo", "bar"], &vec![("foo", "bar")]);
            }

            #[test]
            fn two_targets_one_dep_each() {
                let depcfg = Config::with_targets(vec![
                    target::lone("c"),
                    target::lone("d"),
                    target::dep("a", vec!["c"]),
                    target::dep("b", vec!["d"]),
                ]);
                let depts = get_ts(&depcfg);

                let subcfg = Config::with_targets(vec![
                    target::sub("a", vec![target::lone("c")]),
                    target::sub("b", vec![target::lone("d")]),
                ]);
                let subts = get_ts(&subcfg);
                verify_ts_dag(
                    depts,
                    &vec!["a", "b", "c", "d"],
                    &vec![("a", "c"), ("b", "d")],
                );
                verify_ts_dag(
                    subts,
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
                let depts = get_ts(&depcfg);

                let subcfg = Config::with_targets(vec![
                    target::sub("a", vec![target::lone("c")]),
                    target::dep("b", vec!["a-c"]),
                ]);
                let subts = get_ts(&subcfg);
                verify_ts_dag(depts, &vec!["a", "b", "c"], &vec![("a", "c"), ("b", "c")]);
                verify_ts_dag(
                    subts,
                    &vec!["a", "b", "a-c"],
                    &vec![("a", "a-c"), ("b", "a-c")],
                );
            }
        }

        mod errors {
            use super::*;

            #[test]
            fn immediate_cycle() {
                check_ts_err(
                    vec![
                        target::dep("baz", vec!["foo"]),
                        target::dep("foo", vec!["baz"]),
                    ],
                    "'foo' -> 'baz' creates a cycle",
                );
            }

            #[test]
            fn long_cycle() {
                check_ts_err(
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
                check_ts_err(
                    vec![
                        target::dep("baz", vec!["foo"]),
                        target::dep("foo", vec!["I DO NOT EXIST"]),
                    ],
                    "reference to nonexistent dep: I DO NOT EXIST",
                );
            }

            #[test]
            fn no_sub_or_deps_or_cmd() {
                check_ts_err(
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
                    check_ts_err(
                        vec![target::lone("foo"), target::lone("foo")],
                        "duplicate target name: 'foo'",
                    );
                }

                #[test]
                fn diff_level() {
                    check_ts_err(
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
                    check_ts_err(vec![target::lone("")], "cannot have an empty target name");
                }

                #[test]
                fn target_name_with_period() {
                    check_ts_err(
                        vec![target::lone("pow.")],
                        "cannot have a '.' in a target name: 'pow.'",
                    );
                }

                #[test]
                fn target_name_with_question() {
                    check_ts_err(
                        vec![target::lone("pow?")],
                        "cannot have a '?' in a target name: 'pow?'",
                    );
                }
            }
        }
    }
}
