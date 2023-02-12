use anyhow::{anyhow, bail};
use radix_trie::Trie;
use serde::Deserialize;

use crate::jam::{Chord, ChordTrie};

type Result = anyhow::Result<(Chord, Chord)>;

fn reconciliation_err(conflicts: Vec<&str>, c: Chord) -> anyhow::Error {
    anyhow!(
        "reconciliation failed for chord '{}'; still ambiguous (e.g. is it {}?)",
        c,
        conflicts.join(" or ")
    )
}

type Reconciler = fn(chords: &ChordTrie, conflicts: Vec<&str>, chord: Chord) -> Result;

fn first_nonmatch_reconciler(chords: &ChordTrie, conflicts: Vec<&str>, chord: Chord) -> Result {
    Err(reconciliation_err(conflicts, chord))
}

/// The simple reconciler attempts to reconcile a chord ambiguity by
/// finding the first non-matching character of the conflicting
/// targets that avoids any ambiguity.
pub static first_nonmatch: Reconciler = first_nonmatch_reconciler;

fn error_reconciler(chords: &ChordTrie, conflicts: Vec<&str>, chord: Chord) -> Result {
    Err(reconciliation_err(conflicts, chord))
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

pub fn reconcile(kind: Strategy, chords: &ChordTrie, conflicts: Vec<&str>, chord: Chord) -> Result {
    let reconciler: Reconciler = match kind {
        Strategy::Error => err_reconciler,
        Strategy::FirstNonMatch => first_nonmatch,
    };

    reconciler(chords, conflicts, chord)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    macro_rules! chord {
		    ( $( $x:expr ),* ) => {
		        {
		            let mut temp_vec = Vec::new();
		            $(
		                temp_vec.push(String::from($x));
		            )*
		            Chord(temp_vec)
		        }
		    };
		}

    // Shorthand for Err(reconciliation_err()).
    fn rerr(conflicts: Vec<&str>, chord: Chord) -> Result {
        Err(reconciliation_err(conflicts, chord))
    }

    #[rstest]
    // TODO: Would be nice if we could avoid repeating the values in the expected rerr somehow.
    // Maybe with another macro, or a check_err() function?
    #[case::fooey(chord!["f"], "foo", "far", rerr(vec!["foo", "far"], chord!["f"]))]
    fn check(#[case] chord: Chord, #[case] a: &str, #[case] b: &str, #[case] expected: Result) {
        let trie = Trie::new();
        let res = first_nonmatch(&trie, vec![a, b], chord);
        match expected {
            Ok(chords) => assert_eq!(chords, res.expect("expected no error")),
            Err(err) => assert_eq!(
                format!("{:#?}", err),
                format!("{:#?}", res.expect_err("expected an error"))
            ),
        }
    }
}
