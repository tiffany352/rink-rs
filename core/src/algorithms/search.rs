// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{
    cmp::{Ordering, Reverse},
    collections::BinaryHeap,
};

use strsim::jaro_winkler;

#[derive(PartialEq, Eq, Debug, Clone, Ord)]
struct SearchEntry<'a> {
    score: i32,
    term: &'a str,
}

impl<'a> PartialOrd for SearchEntry<'a> {
    fn partial_cmp(&self, other: &SearchEntry<'a>) -> Option<Ordering> {
        Reverse(self.score).partial_cmp(&Reverse(other.score))
    }
}

pub(crate) fn search_impl<'a>(
    candidates: impl Iterator<Item = &'a str>,
    query: &str,
    num_results: usize,
) -> Vec<&'a str> {
    let mut results = BinaryHeap::new();
    results.reserve(num_results);
    let query = query.to_lowercase();

    for candidate in candidates {
        let lowercased = candidate.to_lowercase();

        let modifier = if lowercased == query {
            4_000
        } else if lowercased.starts_with(&query) {
            3_000
        } else if lowercased.ends_with(&query) {
            2_000
        } else if lowercased.contains(&query) {
            1_000
        } else {
            0_000
        };

        let score = jaro_winkler(&lowercased, &query);

        results.push(SearchEntry {
            score: (score * 1000.0) as i32 + modifier,
            term: candidate,
        });
        while results.len() > num_results {
            results.pop();
        }
    }

    results
        .into_sorted_vec()
        .into_iter()
        .filter(|entry| entry.score > 800)
        .map(|entry| entry.term)
        .collect()
}
