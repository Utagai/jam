use anyhow::bail;
use radix_trie::Trie;

use crate::jam::{Chord, ChordTrie};

type Result = anyhow::Result<(Chord, Chord)>;

type Reconciler = fn(chords: &ChordTrie, a: &str, ac: Chord, b: &str, bc: Chord) -> Result;

fn simple_reconciler(chords: &ChordTrie, a: &str, ac: Chord, b: &str, bc: Chord) -> Result {
    Ok((Chord(vec![]), Chord(vec![])))
}

pub static simple: Reconciler = simple_reconciler;

fn erroring_reconciler(chords: &ChordTrie, a: &str, ac: Chord, b: &str, bc: Chord) -> Result {
    bail!(
        "given chord '{}' is ambiguous (i.e. is it {}?)",
        ac,
        vec![a, b].join(" or "),
    )
}

pub static erroring: Reconciler = erroring_reconciler;

pub enum ReconciliationKind {
    Error,
    Simple,
}

pub fn reconcile(
    kind: ReconciliationKind,
    chords: &ChordTrie,
    a: &str,
    ac: Chord,
    b: &str,
    bc: Chord,
) -> Result {
    let reconciler: Reconciler = match kind {
        ReconciliationKind::Error => erroring,
        ReconciliationKind::Simple => simple,
    };

    reconciler(chords, a, ac, b, bc)
}
