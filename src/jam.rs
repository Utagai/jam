use anyhow::Result;
use daggy::Dag;
use trie_rs::Trie;

use crate::config::Config;

struct Task {
    name: String,
    shortname: String,
    help: String,
    cmd: Option<String>,
}

pub struct Jam {
    short_trie: Trie<String>,
    long_trie: Trie<String>,
    exec_dag: Dag<Task, u32>,
}

impl Jam {}
