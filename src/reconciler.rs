use std::{collections::HashSet, str::Chars};

use anyhow::{anyhow, bail};
use radix_trie::Trie;
use serde::Deserialize;

use crate::jam::{Chord, ChordTrie};

type Result = anyhow::Result<Vec<char>>;

fn reconciliation_err(conflicts: &Vec<&str>, chord: &Chord) -> anyhow::Error {
    anyhow!(
        "reconciliation failed for chord '{}'; still ambiguous (e.g. is it {}?)",
        chord,
        conflicts.join(" or ")
    )
}

type Reconciler = fn(chords: &ChordTrie, conflicts: &Vec<&str>, chord: &Chord) -> Result;

fn first_nonmatch_reconciler(chords: &ChordTrie, conflicts: &Vec<&str>, chord: &Chord) -> Result {
    // We now have an iterator in which every element is itself an
    // iterator over one conflicting target name. So, each of these iterators return one character each.

    // Now, we want to go through each of these iterators, one element
    // at a time. In other words, we are going through every character
    // of every conflict one at a time.
    let mut conflict_iters = conflicts
        .iter()
        .map(|conflict| conflict.chars())
        .collect::<Vec<Chars<'_>>>();
    let mut seen_chars: HashSet<char> = HashSet::new();
    let mut reconciliation = Vec::new();
    'outer: loop {
        seen_chars.clear();
        reconciliation.clear();
        println!("starting outer!");
        for i in 0..conflict_iters.len() {
            println!("are we even running?");
            match conflict_iters[i].next() {
                Some(ch) => {
                    println!("some ch: {}", ch);
                    if seen_chars.contains(&ch) {
                        println!("o no it contains :(");
                        continue 'outer;
                    } else {
                        let new_chord = chord.append(&ch);
                        if chords.get(&new_chord).is_some() {
                            // This chord extension may avoid conflicts here, but not elsewhere.
                            continue 'outer;
                        }
                        // If we get here though, we have a potential solution.
                        seen_chars.insert(ch);
                        reconciliation.push(ch);
                    }
                }
                None => break 'outer,
            }
        }
        // If we make it through the loop, we should have a solution. Let's return it.
        return Ok(reconciliation);
    }

    // If the outer loop ever breaks, it means we encountered a
    // situation where we couldn't find any non-matching characters.
    // This includes cases like foo vs. fool, where one target is a
    // complete prefix of the other.
    // But it also includes foo vs foo. This latter case is not really
    // possible in practice, since we validate against it, but is
    // captured here anyways for robustness.
    return Err(reconciliation_err(conflicts, chord));
}

/// The simple reconciler attempts to reconcile a chord ambiguity by
/// finding the first non-matching character of the conflicting
/// targets that avoids any ambiguity.
pub static first_nonmatch: Reconciler = first_nonmatch_reconciler;

fn error_reconciler(chords: &ChordTrie, conflicts: &Vec<&str>, chord: &Chord) -> Result {
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

pub fn reconcile(
    kind: Strategy,
    chords: &ChordTrie,
    conflicts: &Vec<&str>,
    chord: &Chord,
) -> Result {
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
		                temp_vec.push($x);
		            )*
		            Chord(temp_vec)
		        }
		    };
		}

    // Shorthand for Err(reconciliation_err()).
    fn rerr(conflicts: &Vec<&str>, chord: &Chord) -> Result {
        Err(reconciliation_err(conflicts, chord))
    }

    #[rstest]
    #[case::fooey(chord!['f'], vec!["foo", "far"], Ok(vec!['o', 'a']))]
    fn check(#[case] chord: Chord, #[case] conflicts: Vec<&str>, #[case] expected: Result) {
        let trie = Trie::new();
        let res = first_nonmatch(&trie, &conflicts, &chord);
        assert_eq!(
            expected.unwrap(),
            res.expect("expected no error, but got one")
        );
    }

    #[rstest]
    #[case::fooey(chord!['f'], vec!["foo", "foo"])]
    fn check_err(#[case] chord: Chord, #[case] conflicts: Vec<&str>) {
        let trie = Trie::new();
        let res = first_nonmatch(&trie, &conflicts, &chord);
        assert_eq!(
            format!("{:#?}", rerr(&conflicts, &chord).unwrap_err()),
            format!(
                "{:#?}",
                res.expect_err("expected an error, but didn't get one")
            )
        )
    }
}
