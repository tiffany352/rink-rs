// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use context::Context;
use std::cmp::{PartialOrd, Ord, Ordering};
use strsim::jaro_winkler;
use std::collections::BinaryHeap;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SearchResult<'a> {
    score: i32,
    value: &'a str,
}

impl<'a> PartialOrd for SearchResult<'a> {
    fn partial_cmp(&self, other: &SearchResult<'a>) -> Option<Ordering> {
        self.score.partial_cmp(&other.score).map(|x| x.reverse())
    }
}

impl<'a> Ord for SearchResult<'a> {
    fn cmp(&self, other: &SearchResult<'a>) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub fn search<'a>(ctx: &'a Context, query: &str, num_results: usize) -> Vec<&'a str> {
    let mut results = BinaryHeap::new();
    let query = query.to_lowercase();
    {
        let mut try = |x: &'a str| {
            let borrow = x;
            let x = x.to_lowercase();
            let modifier = if x == query {
                4_000
            } else if x.starts_with(&query) {
                3_000
            } else if x.ends_with(&query) {
                2_000
            } else if x.contains(&query) {
                1_000
            } else {
                0_000
            };
            let score = jaro_winkler(
                &*x,
                &*query,
            );
            results.push(SearchResult {
                score: (score * 1000.0) as i32 + modifier,
                value: borrow,
            });
            while results.len() > num_results {
                results.pop();
            }
        };

        for k in &ctx.dimensions {
            try(&**k.0);
        }
        for k in ctx.units.keys() {
            try(&**k);
        }
        for k in ctx.quantities.values() {
            try(&**k);
        }
        for k in ctx.substances.keys() {
            try(&**k);
        }
    }
    results.into_sorted_vec()
        .into_iter()
        .filter_map(|x| if x.score > 800 { Some(x.value) } else { None })
        .collect()
}
