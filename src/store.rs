use daggy::{Dag, NodeIndex};
use sequence_trie::SequenceTrie;
use std::slice::Iter;
use thiserror::Error;

use crate::{
    config::{DesugaredConfig, DesugaredTargetCfg},
    executor::ExecuteKind,
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

pub(crate) trait TargetStore {
    fn new(cfgs: Vec<DesugaredTargetCfg>) -> ParseResult<Self>
    where
        Self: Sized;
    fn mappings(&self) -> ExecResult<Vec<(Shortcut, &str)>>;
    // TODO: Shoudl we keep this as "Exec" result?
    fn next(&self, prefix: &Shortcut, conflict: bool) -> ExecResult<Vec<NextKey>>;
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
    trie: ShortcutTrie,
    dag: Dag<Target<'a>, IdxT>,
}

impl<'a> TargetStore for TrieDagStore<'a> {
    fn new(cfgs: Vec<DesugaredTargetCfg>) -> ParseResult<Self> {
        todo!()
    }

    fn mappings(&self) -> ExecResult<Vec<(Shortcut, &str)>> {
        todo!()
    }

    fn next(&self, prefix: &Shortcut, conflict: bool) -> ExecResult<Vec<NextKey>> {
        todo!()
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
