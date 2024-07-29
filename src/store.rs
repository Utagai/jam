use anyhow::{anyhow, bail};
use slog::{debug, info, o};
use std::{
    cmp::Ordering,
    collections::HashMap,
    hash::{Hash, Hasher},
    slice::Iter,
};
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

impl Hash for Shortcut {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
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

// TODO: Add doc comments for each method.
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

// SimpleStore is a simple implementation of the TargetStore trait. It uses just
// two hashmaps to accomplish what is needed for this application.
// NOTE: In a prior version of this application, I used a Trie and a DAG to
// store the targets and their dependencies. While this made sense from an
// algorithms textbook perspective, I ended up finding the code substantially
// uglier and harder to read, reason about and update. Furthermore, the choice
// of using the right data structures was only really important in the pursuit
// of efficiency. However, the number of targets that we are likely to have in a
// jamfile is likely to be quite small, so the efficiency gains are likely to be
// negligible. Therefore, I have opted for a simpler, more readable, and more
// maintainable implementation.
pub struct SimpleStore<'a> {
    // shortcut_to_targets is a map from a shortcut to a list of targets that
    // that the shortcut is mapped to. This is not considering reconciliation.
    shortcut_to_targets: HashMap<Shortcut, Vec<Target<'a>>>,
    // deps is a map from a target name to a list of target names that the
    // target depends on.
    deps: HashMap<&'a str, &'a Vec<String>>,
    logger: slog::Logger,
}

impl<'a> TargetStore<'a> for SimpleStore<'a> {
    fn new(logger: &slog::Logger, target_cfgs: &'a Vec<DesugaredTargetCfg>) -> ParseResult<Self> {
        let mut hm: HashMap<Shortcut, Vec<Target>> = HashMap::new();
        let mut deps: HashMap<&str, &Vec<String>> = HashMap::new();
        for target_cfg in target_cfgs {
            // Validate the cfgs while we are already here, looping over them.
            if target_cfg.name.is_empty() {
                return Err(anyhow!("cannot have an empty target name"));
            } else if target_cfg.cmd.is_none() && target_cfg.deps.is_empty() {
                return Err(anyhow!(
                    "a command without an executable command must have dependencies or subtargets, but '{}' does not",
                    target_cfg.name
                ));
            }

            let target = Target::from(target_cfg);
            let shortcut = Shortcut::from_shortcut_str(&target_cfg.shortcut_str);
            deps.insert(target.name, &target_cfg.deps);
            if let Some(targets) = hm.get_mut(&shortcut) {
                targets.push(target);
            } else {
                hm.insert(shortcut, vec![target]);
            }
        }

        // We run the validations one more time in a separate loop. We can't do
        // it in the above loop since we need to have all the targets in the
        // hashmap before we can validate the deps.
        let targets = hm.values().flatten().collect::<Vec<&Target>>();

        // Validate targets
        for target in &targets {
            if target.name.contains('.') {
                return Err(anyhow!(
                    "cannot have a '.' in a target name: '{}'",
                    target.name
                ));
            } else if target.name.contains('?') {
                return Err(anyhow!(
                    "cannot have a '?' in a target name: '{}'",
                    target.name
                ));
            } else if targets.iter().filter(|t| t.name == target.name).count() > 1 {
                return Err(anyhow!("duplicate target name: '{}'", target.name));
            }
        }

        // Validate deps
        for dep in &deps {
            // Check if the dep exists:
            if let Some(dep_name) = dep.1.iter().find(|dep_name| {
                targets
                    .iter()
                    .find(|target| target.name == *dep_name)
                    .is_none()
            }) {
                return Err(anyhow!("reference to nonexistent dep: {}", dep_name));
            }
        }

        Ok(SimpleStore {
            shortcut_to_targets: hm,
            deps,
            logger: logger.new(o!()),
        })
    }

    // TODO: A lot of impls in the simple look very similar to the non-simple. This is a chance to maybe further crystallize the logic here.
    fn mappings(&self, strategy: Strategy) -> ExecResult<Vec<(Shortcut, &str)>> {
        let mut mappings: Vec<(Shortcut, &str)> = self
            .shortcut_to_targets
            .iter()
            .map(|(shortcut, targets)| -> ExecResult<Vec<(Shortcut, &str)>> {
                let shortcut = Shortcut::from(shortcut);
                if targets.len() == 1 {
                    return Ok(vec![(shortcut, targets[0].name)]);
                }

                // If there are multiple though, we should reconcile and return
                // the shortcuts with the reconciled character added.
                let reconciliation_chars = self.reconcile(strategy, &shortcut, &targets)?;
                reconciliation_chars
                    .iter()
                    .enumerate()
                    .map(|(i, c)| Ok((shortcut.append(c), targets[i].name)))
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
                Ok(targets) => {
                    if targets.is_empty() {
                        Lookup::NotFound
                    } else if targets.len() == 1 {
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
            if let Some(targets) = self.shortcut_to_targets.get(shortcut) {
                if targets.len() == 1 {
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
        let targets = self.resolve_shortcut(strategy, &shortcut)?;
        if targets.len() > 1 {
            return Err(ExecError::Conflict {
                shortcut,
                conflict_msg: targets
                    .iter()
                    .map(|target| format!("'{}'", target.name))
                    .collect::<Vec<String>>()
                    .join(" or "),
            });
        }
        if let Some(target) = targets.first() {
            return Ok(target);
        } else {
            Err(ExecError::ShortcutNotFound { shortcut })
        }
    }

    fn get_by_target_name(&self, target_name: &str) -> ExecResult<&Target> {
        let target = self
            .shortcut_to_targets
            .values()
            .flatten()
            .find(|target| target.name == target_name)
            .ok_or(ExecError::TargetNotFound {
                target_name: target_name.to_string(),
            })?;
        return Ok(target);
    }

    fn children(&self, target: &Target) -> ExecResult<Vec<&Target>> {
        let target = self
            .shortcut_to_targets
            .values()
            .flatten()
            .find(|t| t.name == target.name)
            .ok_or(ExecError::TargetNotFound {
                target_name: target.name.to_string(),
            })?;
        let dep_names = self
            .deps
            .get(target.name)
            .ok_or(ExecError::TargetNotFound {
                target_name: target.name.to_string(),
            })?;

        Ok(self
            .shortcut_to_targets
            .values()
            .flatten()
            .filter(|target| dep_names.iter().any(|dep_name| target.name == dep_name))
            .collect())
    }
}

impl<'a> SimpleStore<'a> {
    fn resolve_shortcut(
        &self,
        strategy: Strategy,
        shortcut: &Shortcut,
    ) -> ExecResult<Vec<&Target>> {
        let resolved_targets;
        match self.shortcut_to_targets.get(shortcut) {
            Some(targets) => resolved_targets = targets.iter().collect(),
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
                match self.shortcut_to_targets.get(&tail) {
                    Some(targets) => {
                        // Secondly, not only must it exist, but it
                        // must be referring to a shortcut that has
                        // conflicts. If it has none, then the user
                        // should just specify the tail.
                        if targets.len() <= 1 {
                            return Err(ExecError::ShortcutNotFound {
                                shortcut: Shortcut::from(shortcut),
                            });
                        }

                        info!(self.logger, "reconciling with strategy"; o!("strategy" => strategy.to_string()));

                        // If it does indeed have a conflict,
                        // then let's make sure that the specified
                        // reconciliation key is a valid one:
                        let reconciliation_keys = self.reconcile(strategy, &tail, targets)?;

                        // The returned reconciliation keys are 1:1
                        // with the node indexes. i.e., the
                        // reconciliation key at index i is the
                        // reconciliation key for the conflicting
                        // target at index i in nidxes.
                        let target = match reconciliation_keys
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
                            Some(pos) => &targets[pos],
                        };

                        debug!(self.logger, "reconciled");
                        resolved_targets = vec![target];
                    }
                    None => {
                        return Err(ExecError::ShortcutNotFound {
                            shortcut: Shortcut::from(shortcut),
                        });
                    }
                }
            }
        };
        Ok(resolved_targets)
    }

    fn reconcile(
        &self,
        strategy: Strategy,
        shortcut: &Shortcut,
        conflicts: &[Target],
    ) -> ExecResult<Vec<char>> {
        match reconcile(
            strategy,
            self,
            &conflicts
                .iter()
                .map(|conflict| conflict.name)
                .collect::<Vec<&str>>(),
            shortcut,
        ) {
            Ok(keys) => Ok(keys),
            Err(err) => Err(ExecError::Reconciliation {
                description: err.to_string(),
            }),
        }
    }

    fn next_conflict(&self, strategy: Strategy, prefix: &Shortcut) -> ExecResult<Vec<NextKey>> {
        let targets = self
            .shortcut_to_targets
            .get(prefix)
            .ok_or(ExecError::ShortcutNotFound {
                shortcut: prefix.clone(),
            })?;
        self.reconcile(strategy, prefix, targets).and_then(|keys| {
            keys.into_iter()
                .map(|key| {
                    self.resolve_shortcut(strategy, &prefix.append(&key))
                        .map(|targets| {
                            NextKey::new(key, targets.iter().map(|target| target.name).collect())
                        })
                })
                .collect::<ExecResult<Vec<NextKey>>>()
        })
    }

    fn next_no_conflict(&self, prefix: &Shortcut) -> ExecResult<Vec<NextKey>> {
        if !self
            .shortcut_to_targets
            .keys()
            .any(|key| key.0.starts_with(&prefix.0))
        {
            return Ok(vec![]);
        }

        let mut keys_to_names: Vec<NextKey> = self
            .shortcut_to_targets
            .iter()
            .filter_map(|(shortcut, targets)| {
                if shortcut.0.starts_with(&prefix.0) && shortcut.0.len() <= prefix.0.len() + 2 {
                    let key = shortcut.0.get(prefix.0.len());
                    if let Some(key) = key {
                        let tgts: Vec<&str> = targets.iter().map(|t| t.name).collect();
                        info!(self.logger, "checking shortcut"; o!("shortcut" => format!("{}", shortcut), "prefix" => format!("{}", prefix), "key" => key, "tgts" => format!("{:?}", tgts.len())));
                        return Some(NextKey::new(*key, tgts));
                    }
                }
                None
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
        // TODO: Update this comment. I'm not sure it is correct anymore, and
        // even if it is, it is now a partial explanation since the comparison
        // logic is more complex now.
        // TODO: Or, and here's a crazy idea. Technically, without this extra
        // crap, we won't show the '...' like we did before. However, arguably,
        // what we had before was wrong. There wasn't actually a branch really,
        // it was a single path it just wasn't a _leaf_. Could we introduce a
        // new NextKey variant for this kind of node and render it specially?
        keys_to_names.sort_by(|a, b| {
            let cmp = a.key().cmp(&b.key());
            if cmp == Ordering::Equal {
                match (a, b) {
                    (NextKey::BranchKey { .. }, NextKey::LeafKey { .. }) => Ordering::Less,
                    (NextKey::LeafKey { .. }, NextKey::BranchKey { .. }) => Ordering::Greater,
                    _ => Ordering::Equal,
                }
            } else {
                cmp
            }
        });
        keys_to_names.dedup_by(|a, b| a.key() == b.key());

        // Now return the keys, mapped to their next target names.
        info!(self.logger, "returning next keys"; o!("keys" => format!("{:?}", keys_to_names.len())));
        Ok(keys_to_names)
    }
}

#[cfg(test)]
mod tests {
    use super::SimpleStore;
    use crate::config::DesugaredConfig;
    use crate::store::{Target, TargetStore};
    use crate::testutils::logger;

    fn get_ts(cfg: &DesugaredConfig) -> SimpleStore {
        SimpleStore::new(&logger::test(), &cfg.targets).expect("expected no errors from parsing")
    }

    mod parse {
        use super::*;

        use crate::config::target;
        use crate::config::{Config, Options, TargetCfg};
        use crate::log::Level;

        // TODO: This should probably test across all impls at once?
        impl<'a> SimpleStore<'a> {
            fn get_target_by_name(&self, target_name: &str) -> Option<&Target> {
                self.shortcut_to_targets
                    .values()
                    .find(|targets| targets.iter().any(|target| target.name == target_name))
                    .and_then(|targets| targets.iter().find(|target| target.name == target_name))
            }

            fn has_target(&self, target_name: &str) -> bool {
                self.get_target_by_name(target_name).is_some()
            }

            fn has_dep(&self, dependee: &str, dependent: &str) -> bool {
                self.deps.iter().any(|dep| {
                    *dep.0 == dependee && dep.1.iter().any(|target_name| target_name == dependent)
                })
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
            if let Err(err) = SimpleStore::new(&logger::test(), &cfg.targets) {
                assert_eq!(format!("{err}").trim(), expected_err)
            } else {
                panic!("expected an error from parsing, but got none")
            }
        }

        fn verify_ts_dag(ts: SimpleStore, targets: &Vec<&str>, deps: &Vec<(&str, &str)>) {
            for target in targets {
                assert!(ts.has_target(target));
            }

            for dep in deps {
                assert!(ts.has_dep(dep.0, dep.1));
            }

            let num_total_deps: usize = ts.deps.iter().map(|(_, deps)| deps.len()).sum();
            let num_total_targets: usize = ts
                .shortcut_to_targets
                .iter()
                .map(|(_, nidxes)| nidxes.len())
                .sum();

            assert_eq!(num_total_deps, deps.len());
            assert_eq!(num_total_targets, targets.len());
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
