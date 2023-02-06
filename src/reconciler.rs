use anyhow::{anyhow, bail};
use radix_trie::Trie;
use serde::Deserialize;

use crate::jam::{Chord, ChordTrie};

type Result = anyhow::Result<(Chord, Chord)>;

fn reconciliation_err(c: Chord, a: &str, b: &str) -> anyhow::Error {
    anyhow!(
        "reconciliation failed for chord '{}'; still ambiguous (e.g. is it {}?)",
        c,
        vec![a, b].join(" or ")
    )
}

type Reconciler = fn(chords: &ChordTrie, chord: Chord, a: &str, b: &str) -> Result;

fn first_nonmatch_reconciler(chords: &ChordTrie, chord: Chord, a: &str, b: &str) -> Result {
    let len_diff = a.len().abs_diff(b.len());
    // TODO: This shouldn't be hardcoded or assumed. We should have
    // some kind of listing of disallowed characters somewhere and we
    // just take the first element from there.
    let a_iter = a
        .chars()
        .chain(std::iter::repeat('.'))
        .take(if a.len() > b.len() { 0 } else { len_diff });
    let b_iter = b
        .chars()
        .chain(std::iter::repeat('.'))
        .take(if a.len() > b.len() { 0 } else { len_diff });
    a_iter
        .zip(b_iter)
        .filter_map(|(ach, bch)| {
            if ach != bch {
                Some((chord.append(&ach), chord.append(&bch)))
            } else {
                None
            }
        })
        .find(|(new_ac, new_bc)| chords.get(new_ac).is_none() && chords.get(new_bc).is_none())
        .ok_or(reconciliation_err(chord, a, b))
}

/// The simple reconciler attempts to reconcile a chord ambiguity by
/// finding the first non-matching character of the conflicting
/// targets that avoids any ambiguity.
pub static first_nonmatch: Reconciler = first_nonmatch_reconciler;

fn error_reconciler(chords: &ChordTrie, chord: Chord, a: &str, b: &str) -> Result {
    Err(reconciliation_err(chord, a, b))
}

// NOTE: We have to name this err because otherwise it conflicts with
// a variable defined inside the anyhow! macro.
pub static err_reconciler: Reconciler = error_reconciler;

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

pub fn reconcile(kind: Strategy, chords: &ChordTrie, chord: Chord, a: &str, b: &str) -> Result {
    let reconciler: Reconciler = match kind {
        Strategy::Error => err_reconciler,
        Strategy::FirstNonMatch => first_nonmatch,
    };

    reconciler(chords, chord, a, b)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[rstest]
    #[case::fooey(Chord(vec![String::from("f")]), "foo", "far", Err(reconciliation_err(Chord(vec![String::from("f")]), "foo", "far")))]
    fn check(#[case] chord: Chord, #[case] a: &str, #[case] b: &str, #[case] expected: Result) {
        let trie = Trie::new();
        let res = first_nonmatch(&trie, chord, a, b);
        match expected {
            Ok(chords) => assert_eq!(chords, res.expect("expected no error")),
            Err(err) => assert_eq!(
                format!("{:#?}", err),
                format!("{:#?}", res.expect_err("expected an error"))
            ),
        }
    }
}
