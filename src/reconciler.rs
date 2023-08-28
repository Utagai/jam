use std::{
    collections::{HashMap, HashSet},
    str::Chars,
};

use anyhow::anyhow;
use serde::Deserialize;

use crate::jam::Shortcut;

type Result = anyhow::Result<Vec<char>>;

fn reconciliation_err(conflicts: &Vec<&str>, shortuct: &Shortcut) -> anyhow::Error {
    anyhow!(
        "reconciliation failed for shortcut '{}'; still ambiguous (e.g. is it {}?)",
        shortuct,
        conflicts.join(" or ")
    )
}

type Reconciler =
    fn(shortcuts: &HashMap<&str, Vec<&str>>, conflicts: &Vec<&str>, shortcut: &str) -> Result;

fn first_nonmatch_reconciler(
    shortcuts: &HashMap<&str, Vec<&str>>,
    conflicts: &Vec<&str>,
    shortcut_str: &str,
) -> Result {
    let shortcut = Shortcut::from_shortcut_str(shortcut_str);
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
        for conflict_iter in &mut conflict_iters {
            let next_ch = conflict_iter.next();
            if still_conflict {
                // Just skip, since there's no purpose. We just need
                // to make sure we've iterated the iterator.
                continue;
            }
            match next_ch {
                Some(ch) => {
                    if seen_chars.contains(&ch) {
                        still_conflict = true;
                    } else {
                        let new_shortcut = shortcut.append(&ch);
                        if shortcuts.get(new_shortcut.to_string().as_str()).is_some() {
                            // This shortcut extension may avoid conflicts here, but not elsewhere.
                            still_conflict = true;
                        }
                        // If we get here though, we have a potential solution.
                        seen_chars.insert(ch);
                        reconciliation.push(ch);
                    }
                }
                None => return Err(reconciliation_err(conflicts, &shortcut)), // One or more of these targets ran out of characters. We're done for.
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
pub static FIRST_NONMATCH: Reconciler = first_nonmatch_reconciler;

fn error_reconciler(
    _: &HashMap<&str, Vec<&str>>,
    conflicts: &Vec<&str>,
    shortcut_str: &str,
) -> Result {
    Err(reconciliation_err(
        conflicts,
        &Shortcut::from_shortcut_str(shortcut_str),
    ))
}

// NOTE: We have to name this err because otherwise it conflicts with
// a variable defined inside the anyhow! macro.
pub static ERR_RECONCILER: Reconciler = error_reconciler;

#[derive(PartialEq, Deserialize, Debug, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum Strategy {
    Error,
    FirstNonMatch,
}

impl std::fmt::Display for Strategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Default for Strategy {
    fn default() -> Self {
        Self::Error
    }
}

pub fn reconcile(
    kind: Strategy,
    shortcuts: &HashMap<&str, Vec<&str>>,
    conflicts: &Vec<&str>,
    shortcut_str: &str,
) -> Result {
    let reconciler: Reconciler = match kind {
        Strategy::Error => ERR_RECONCILER,
        Strategy::FirstNonMatch => FIRST_NONMATCH,
    };

    reconciler(shortcuts, conflicts, shortcut_str)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use rstest::rstest;

    // Shorthand for Err(reconciliation_err()).
    fn rerr(conflicts: &Vec<&str>, shortcut_str: &str) -> Result {
        Err(reconciliation_err(
            conflicts,
            &Shortcut::from_shortcut_str(shortcut_str),
        ))
    }

    #[rstest]
    #[case::simple("f", vec!["foo", "far"], Ok(vec!['o', 'a']))]
    #[case::simple_overriden("z", vec!["foo", "bar"], Ok(vec!['f', 'b']))]
    #[case::simple_overriden_initially_same("z", vec!["foo", "far"], Ok(vec!['o', 'a']))]
    #[case::almost_same("f", vec!["lalalah", "lalalaz"], Ok(vec!['h', 'z']))]
    #[case::three_way_conflict("f", vec!["foo", "faz", "fiz"], Ok(vec!['o', 'a', 'i']))]
    #[case::three_way_conflict_almost_same("f", vec!["lalalah", "lalalaz", "lalalab"], Ok(vec!['h', 'z', 'b']))]
    #[case::many_conflicts_keep_almost_reconciling("f", vec!["fooly", "faozi", "failz"], Ok(vec!['y', 'i', 'z']))]
    #[case::unequal_length_conflicts_reconcile_in_time("f", vec!["dinosaur", "rabbit", "river"], Ok(vec!['n', 'b', 'v']))]
    fn check(#[case] shortcut: &str, #[case] conflicts: Vec<&str>, #[case] expected: Result) {
        // TODO: Were we always testing with empty container? If so, we should probably fix this...
        let map = HashMap::new();
        let res = FIRST_NONMATCH(&map, &conflicts, &shortcut);
        assert_eq!(
            expected.unwrap(),
            res.expect("expected no error, but got one")
        );
    }

    #[rstest]
    #[case::same_target_names("f", vec!["foo", "foo"])]
    #[case::one_is_complete_prefix("f", vec!["foo", "fool"])]
    #[case::multiple_same_target_names("f", vec!["foo", "foo", "foo", "foo"])]
    #[case::multiple_one_is_complete_prefix("f", vec!["foo", "fool", "baz", "quux"])]
    #[case::multiple_all_but_one_is_complete_prefix("f", vec!["foo", "fool", "fooli", "foolicooli"])]
    fn check_err(#[case] shortcut: &str, #[case] conflicts: Vec<&str>) {
        let trie = HashMap::new();
        let res = FIRST_NONMATCH(&trie, &conflicts, &shortcut);
        assert_eq!(
            format!("{:#?}", rerr(&conflicts, &shortcut).unwrap_err()),
            format!(
                "{:#?}",
                res.expect_err("expected an error, but didn't get one")
            )
        )
    }
}
