// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::ast::Query;
use crate::output::{QueryError, QueryReply};
use crate::parsing::text_query;
use crate::types::DateTime;
use crate::{Context, Value};

/// The default `definitions.units` file that contains all of the base
/// units, units, prefixes, quantities, and substances.
///
/// This will be Some if the `bundle-files` feature is enabled,
/// otherwise it will be None.
#[cfg(feature = "bundle-files")]
pub static DEFAULT_FILE: Option<&'static str> = Some(include_str!("../definitions.units"));
#[cfg(not(feature = "bundle-files"))]
pub static DEFAULT_FILE: Option<&'static str> = None;

/// The default `datepatterns.txt` file that contains patterns that rink
/// uses for parsing datetimes.
///
/// This will be Some if the `bundle-files` feature is enabled,
/// otherwise it will be None.
#[cfg(feature = "bundle-files")]
pub static DATES_FILE: Option<&'static str> = Some(include_str!("../datepatterns.txt"));
#[cfg(not(feature = "bundle-files"))]
pub static DATES_FILE: Option<&'static str> = None;

/// The default `currenty.units` file that contains currency information
/// that changes rarely. It's used together with live currency data
/// to add currency support to rink.
///
/// This will be Some if the `bundle-files` feature is enabled,
/// otherwise it will be None.
#[cfg(feature = "bundle-files")]
pub static CURRENCY_FILE: Option<&'static str> = Some(include_str!("../currency.units"));
#[cfg(not(feature = "bundle-files"))]
pub static CURRENCY_FILE: Option<&'static str> = None;

/// Helper function that updates the `now` to the current time, parses
/// the query, evaluates it, and updates the `previous_result` field
/// that's used to return the previous query when using `ans`.
///
/// ## Panics
///
/// Panics on platforms where fetching the current time is not possible,
/// such as WASM.
pub fn eval(ctx: &mut Context, line: &str) -> Result<QueryReply, QueryError> {
    ctx.set_time(DateTime::now());
    let mut iter = text_query::TokenIterator::new(line.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    let res = ctx.eval_query(&expr)?;
    if ctx.save_previous_result {
        if let QueryReply::Number(ref number_parts) = res {
            if let Some(ref raw) = number_parts.raw_value {
                ctx.previous_result = Some(Value::Number(raw.clone()));
            }
        }
    }
    Ok(res)
}

/// A version of eval() that converts results and errors into plain-text strings.
pub fn one_line(ctx: &mut Context, line: &str) -> Result<String, String> {
    eval(ctx, line)
        .as_ref()
        .map(ToString::to_string)
        .map_err(ToString::to_string)
}

/// Reformats a query based on Rink's interpretation (e.g. expanding mg to milligrams).
pub fn reformat(ctx: &mut Context, line: &str) -> Query {
    let mut iter = text_query::TokenIterator::new(line.trim()).peekable();
    let query = text_query::parse_query(&mut iter);
    let query = ctx.canonicalize_query(query);
    query
}

/// Tries to create a context that has core definitions only (contents
/// of definitions.units), will fail if the bundle-files feature isn't enabled.
/// Mainly intended for unit testing.
pub fn simple_context() -> Result<Context, String> {
    let message = "bundle-files feature not enabled, cannot create simple context.";

    let units = DEFAULT_FILE.ok_or(message.to_owned())?;
    let dates = DATES_FILE.ok_or(message.to_owned())?;

    let mut ctx = Context::new();
    ctx.load_definitions(units)?;
    ctx.load_date_file(dates);

    Ok(ctx)
}

/// Returns `env!("CARGO_PKG_VERSION")`, a string in `x.y.z` format.
pub fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}
