use std::slice::Iter;
use thiserror::Error;

use crate::{config::DesugaredTargetCfg, executor::ExecuteKind};

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

// TODO: Maybe we just move all of this to jam.rs? The key value-add of the work is to just decouple the store implementation from the parsing/execution implementaiton (i.e. jam).
pub type ExecResult<T> = Result<T, ExecError>;

pub(crate) trait TargetStore {
    fn get(&self, key: &str) -> Option<&str>;
    fn set(&mut self, key: &str, value: &str);
    fn remove(&mut self, key: &str);
}
