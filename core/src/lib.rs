// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

/*! The primary interface of this library is meant to expose a very
simple command-reply model for frontends, and to allow gradual
addition of more advanced functionality. For now, only basic
functionality exists.

Using Rink as a library for uses other than simple unit conversion
tools is not currently well supported, and if you wish to do so,
please make issues for any problems you have.

There are currently a number of hardcoded `println!`s and `unwrap()`s
because most of this code was written in a day without much thought
towards making it into a library.

To use the library, check how the CLI tool does it. To get additional
features like currency and BTC you'll need to fetch those files
yourself and add them into the Context.

## Example

```rust
use rink_core::*;

let mut ctx = simple_context().unwrap();
println!("{}", one_line(&mut ctx, "kWh / year -> W").unwrap());
```
*/

// False positives, or make code harder to understand.
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::option_as_ref_deref)]
#![allow(clippy::needless_lifetimes)]

#[macro_use]
extern crate serde_derive;

pub mod ast;
pub mod bigint;
pub mod bigrat;
pub mod context;
pub mod date;
pub mod eval;
pub mod factorize;
pub mod fmt;
pub mod formula;
pub mod gnu_units;
pub mod load;
pub mod number;
pub mod numeric;
pub mod reply;
pub mod search;
pub mod substance;
pub mod text_query;
pub mod value;

use reply::{QueryError, QueryReply};

pub use crate::context::Context;
pub use crate::number::Number;
pub use crate::value::Value;

use std::collections::BTreeMap;

#[cfg(feature = "gpl")]
pub static DEFAULT_FILE: Option<&'static str> = Some(include_str!("../definitions.units"));
#[cfg(not(feature = "gpl"))]
pub static DEFAULT_FILE: Option<&'static str> = None;

pub static DATES_FILE: &str = include_str!("../datepatterns.txt");
pub static CURRENCY_FILE: &str = include_str!("../currency.units");

pub fn eval(ctx: &mut Context, line: &str) -> Result<QueryReply, QueryError> {
    ctx.update_time();
    let mut iter = text_query::TokenIterator::new(line.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    let res = ctx.eval_outer(&expr)?;
    if ctx.save_previous_result {
        if let QueryReply::Number(ref number_parts) = res {
            if let Some(ref raw) = number_parts.raw_value {
                ctx.previous_result = Some(raw.clone());
            }
        }
    }
    Ok(res)
}

/// Evaluates a single line within a context.
pub fn one_line(ctx: &mut Context, line: &str) -> Result<String, String> {
    eval(ctx, line)
        .as_ref()
        .map(ToString::to_string)
        .map_err(ToString::to_string)
}

/// Tries to create a context that has core definitions only (contents
/// of definitions.units), will fail if the GPL feature isn't enabled.
/// Mainly intended for unit testing.
pub fn simple_context() -> Result<Context, String> {
    let units = match DEFAULT_FILE {
        Some(units) => units,
        None => return Err("GPL feature not enabled, cannot create simple context.".to_owned()),
    };

    let mut iter = gnu_units::TokenIterator::new(&*units).peekable();
    let units = gnu_units::parse(&mut iter);

    let dates = date::parse_datefile(DATES_FILE);

    let mut ctx = Context::new();
    ctx.load(units);
    ctx.load_dates(dates);
    Ok(ctx)
}

pub(crate) fn btree_merge<K: ::std::cmp::Ord + Clone, V: Clone, F: Fn(&V, &V) -> Option<V>>(
    left: &BTreeMap<K, V>,
    right: &BTreeMap<K, V>,
    merge_func: F,
) -> BTreeMap<K, V> {
    let mut res = BTreeMap::new();
    let mut a = left.iter().peekable();
    let mut b = right.iter().peekable();
    loop {
        match (a.peek().cloned(), b.peek().cloned()) {
            (Some((akey, aval)), Some((bkey, bval))) if akey == bkey => {
                if let Some(v) = merge_func(aval, bval) {
                    res.insert(akey.clone(), v);
                }
                a.next();
                b.next();
            }
            (Some((akey, _)), Some((bkey, bval))) if akey > bkey => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            }
            (Some((akey, aval)), Some((bkey, _))) if akey < bkey => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            }
            (Some(_), Some(_)) => unreachable!(),
            (None, Some((bkey, bval))) => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            }
            (Some((akey, aval)), None) => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            }
            (None, None) => break,
        }
    }
    res
}
