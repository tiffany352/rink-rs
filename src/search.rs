// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use context::Context;
use std::cmp::{PartialOrd, Ord, Ordering};
use strsim::jaro_winkler;
use std::collections::BinaryHeap;
use std::borrow::Cow;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SearchResult<'a> {
    score: i32,
    value: Cow<'a, str>,
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

#[cfg(feature = "lmdb")]
pub fn search_db<'a, F: FnMut(Cow<'a, str>)>(
    ctx: &'a Context, test: &mut F
) -> Result<(), String> {
    use lmdb::{Transaction, Cursor};
    use std::str::from_utf8;

    let lmdb = match ctx.lmdb {
        Some(ref lmdb) => lmdb,
        None => return Ok(())
    };
    let tx = try!(lmdb.env.begin_ro_txn().map_err(|e| format!(
        "Failed to create read transaction: {}", e
    )));
    let mut cursor = try!(tx.open_ro_cursor(lmdb.substances).map_err(|e| format!(
        "Failed to create read cursor: {}", e
    )));
    for (k, _v) in cursor.iter() {
        if let Ok(k) = from_utf8(k) {
            test(Cow::Owned(k.to_owned()))
        }
    }
    Ok(())
}

#[cfg(not(feature = "lmdb"))]
pub fn search_db<'a, F: FnMut(Cow<'a, str>)>(_ctx: &'a Context, _test: &mut F) {}

pub fn search<'a>(ctx: &'a Context, query: &str, num_results: usize) -> Vec<Cow<'a, str>> {
    let mut results = BinaryHeap::new();
    let query = query.to_lowercase();
    {
        let mut try = |x: Cow<'a, str>| {
            let borrow = x;
            let x = borrow.to_lowercase();
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
            try(Cow::Borrowed(&**k.0));
        }
        for (k, _v) in &ctx.units {
            try(Cow::Borrowed(&**k));
        }
        for (_u, k) in &ctx.quantities {
            try(Cow::Borrowed(&**k));
        }
        for (k, _sub) in &ctx.substances {
            try(Cow::Borrowed(&**k));
        }
        match search_db(ctx, &mut try) {
            Ok(()) => (),
            Err(e) => println!("Searching database failed: {}", e)
        }
    }
    results.into_sorted_vec()
        .into_iter()
        .filter_map(|x| if x.score > 800 { Some(x.value) } else { None })
        .collect()
}
