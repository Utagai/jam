use anyhow::bail;
use radix_trie::Trie;
use serde::Deserialize;

use crate::jam::{Chord, ChordTrie};

type Result = anyhow::Result<(Chord, Chord)>;

type Reconciler = fn(chords: &ChordTrie, a: &str, ac: Chord, b: &str, bc: Chord) -> Result;

fn first_nonmatch_reconciler(chords: &ChordTrie, a: &str, ac: Chord, b: &str, bc: Chord) -> Result {
    Ok((Chord(vec![]), Chord(vec![])))
}

/// The simple reconciler attempts to reconcile a chord ambiguity by
/// finding the first non-matching character of the conflicting
/// targets that avoids any ambiguity.
pub static first_nonmatch: Reconciler = first_nonmatch_reconciler;

fn error_reconciler(chords: &ChordTrie, a: &str, ac: Chord, b: &str, bc: Chord) -> Result {
    bail!(
        "given chord '{}' is ambiguous (i.e. is it {}?)",
        ac,
        vec![a, b].join(" or "),
    )
}

pub static error: Reconciler = error_reconciler;

#[derive(PartialEq, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum Strategy {
    Error,
    FirstNonMatch,
}

impl Default for Strategy {
    fn default() -> Self {
        Self::Error
    }
}

pub fn reconcile(
    kind: Strategy,
    chords: &ChordTrie,
    a: &str,
    ac: Chord,
    b: &str,
    bc: Chord,
) -> Result {
    let reconciler: Reconciler = match kind {
        Strategy::Error => error,
        Strategy::FirstNonMatch => first_nonmatch,
    };

    reconciler(chords, a, ac, b, bc)
}
