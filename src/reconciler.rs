use std::{collections::HashSet, str::Chars};

use anyhow::{anyhow, bail};
use radix_trie::Trie;
use serde::Deserialize;

use crate::jam::{Shortcut, ShortcutTrie};

type Result = anyhow::Result<Vec<char>>;

fn reconciliation_err(conflicts: &Vec<&str>, shortuct: &Shortcut) -> anyhow::Error {
    anyhow!(
        "reconciliation failed for shortcut '{}'; still ambiguous (e.g. is it {}?)",
        shortuct,
        conflicts.join(" or ")
    )
}

type Reconciler =
    fn(shortcuts: &ShortcutTrie, conflicts: &Vec<&str>, shortcut: &Shortcut) -> Result;

fn first_nonmatch_reconciler(
    shortcuts: &ShortcutTrie,
    conflicts: &Vec<&str>,
    shortcut: &Shortcut,
) -> Result {
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
    loop {
        // These are per-iteration states, so clear them before we
        // start one lest we include state from the last iteration.
        seen_chars.clear();
        reconciliation.clear();
        let mut still_conflict = false;
        println!("starting outer!");
        for i in 0..conflict_iters.len() {
            println!("are we even running?");
            let next_ch = conflict_iters[i].next();
            if still_conflict {
                // Just skip, since there's no purpose. We just need
                // to make sure we've iterated the iterator.
                continue;
            }
            match next_ch {
                Some(ch) => {
                    println!("some ch: {}", ch);
                    if seen_chars.contains(&ch) {
                        println!("o no it contains :(");
                        still_conflict = true;
                    } else {
                        let new_shortcut = shortcut.append(&ch);
                        if shortcuts.get(&new_shortcut).is_some() {
                            // This shortcut extension may avoid conflicts here, but not elsewhere.
                            still_conflict = true;
                        }
                        // If we get here though, we have a potential solution.
                        seen_chars.insert(ch);
                        reconciliation.push(ch);
                    }
                }
                None => return Err(reconciliation_err(conflicts, shortcut)), // One or more of these targets ran out of characters. We're done for.
            }
        }
        // If we make it through the loop, we should have a solution
        // as long as we didn't run into any conflicts. Let's return
        // it if so.
        if !still_conflict {
            return Ok(reconciliation);
        }
    }

    // If the outer loop ever breaks, it means we encountered a
    // situation where we couldn't find any non-matching characters.
    // This includes cases like foo vs. fool, where one target is a
    // complete prefix of the other.
    // But it also includes foo vs foo. This latter case is not really
    // possible in practice, since we validate against it, but is
    // captured here anyways for robustness.
}

/// The simple reconciler attempts to reconcile a shortcut ambiguity by
/// finding the first non-matching character of the conflicting
/// targets that avoids any ambiguity.
pub static first_nonmatch: Reconciler = first_nonmatch_reconciler;

fn error_reconciler(
    shortcuts: &ShortcutTrie,
    conflicts: &Vec<&str>,
    shortcut: &Shortcut,
) -> Result {
    Err(reconciliation_err(conflicts, shortcut))
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
    shortcuts: &ShortcutTrie,
    conflicts: &Vec<&str>,
    shortcut: &Shortcut,
) -> Result {
    let reconciler: Reconciler = match kind {
        Strategy::Error => err_reconciler,
        Strategy::FirstNonMatch => first_nonmatch,
    };

    reconciler(shortcuts, conflicts, shortcut)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    macro_rules! shortcut {
		    ( $( $x:expr ),* ) => {
		        {
		            let mut temp_vec = Vec::new();
		            $(
		                temp_vec.push($x);
		            )*
		            Shortcut(temp_vec)
		        }
		    };
		}

    // Shorthand for Err(reconciliation_err()).
    fn rerr(conflicts: &Vec<&str>, shortcut: &Shortcut) -> Result {
        Err(reconciliation_err(conflicts, shortcut))
    }

    #[rstest]
    #[case::simple(shortcut!['f'], vec!["foo", "far"], Ok(vec!['o', 'a']))]
    #[case::simple_overriden(shortcut!['z'], vec!["foo", "bar"], Ok(vec!['f', 'b']))]
    #[case::simple_overriden_initially_same(shortcut!['z'], vec!["foo", "far"], Ok(vec!['o', 'a']))]
    #[case::almost_same(shortcut!['f'], vec!["lalalah", "lalalaz"], Ok(vec!['h', 'z']))]
    #[case::three_way_conflict(shortcut!['f'], vec!["foo", "faz", "fiz"], Ok(vec!['o', 'a', 'i']))]
    #[case::three_way_conflict_almost_same(shortcut!['f'], vec!["lalalah", "lalalaz", "lalalab"], Ok(vec!['h', 'z', 'b']))]
    #[case::many_conflicts_keep_almost_reconciling(shortcut!['f'], vec!["fooly", "faozi", "failz"], Ok(vec!['y', 'i', 'z']))]
    #[case::unequal_length_conflicts_reconcile_in_time(shortcut!['f'], vec!["dinosaur", "rabbit", "river"], Ok(vec!['n', 'b', 'v']))]
    fn check(#[case] shortcut: Shortcut, #[case] conflicts: Vec<&str>, #[case] expected: Result) {
        let trie = Trie::new();
        let res = first_nonmatch(&trie, &conflicts, &shortcut);
        assert_eq!(
            expected.unwrap(),
            res.expect("expected no error, but got one")
        );
    }

    #[rstest]
    #[case::same_target_names(shortcut!['f'], vec!["foo", "foo"])]
    #[case::one_is_complete_prefix(shortcut!['f'], vec!["foo", "fool"])]
    #[case::multiple_same_target_names(shortcut!['f'], vec!["foo", "foo", "foo", "foo"])]
    #[case::multiple_one_is_complete_prefix(shortcut!['f'], vec!["foo", "fool", "baz", "quux"])]
    #[case::multiple_all_but_one_is_complete_prefix(shortcut!['f'], vec!["foo", "fool", "fooli", "foolicooli"])]
    fn check_err(#[case] shortcut: Shortcut, #[case] conflicts: Vec<&str>) {
        let trie = Trie::new();
        let res = first_nonmatch(&trie, &conflicts, &shortcut);
        assert_eq!(
            format!("{:#?}", rerr(&conflicts, &shortcut).unwrap_err()),
            format!(
                "{:#?}",
                res.expect_err("expected an error, but didn't get one")
            )
        )
    }
}
