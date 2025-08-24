// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Rink is a small language for calculations and unit conversions.
//! It is available as a CLI, a web interface, an IRC client.
//! `rink_core` is the library that the frontends use.
//!
//! The API is designed to let you start simple and then progressively
//! add more features.
//!
//! Rink is designed to be used interactively, with the user typing a
//! query and then seeing the result. It's common for this to be a
//! session, so the previous query can be referenced with `ans`, and
//! some form of history is available using up/down arrows.
//!
//! Using rink for purposes other than this is out of scope, but may be
//! possible anyway depending on what you're trying to do.
//!
//! ## Example
//!
//! Minimal implementation.
//!
//! ```rust
//! # fn main() -> Result<(), String> {
//! // Create a context. This is expensive (30+ ms), so do it once at
//! // startup and keep it around.
//! let mut ctx = rink_core::simple_context()?;
//! // `one_line` is a helper function that parses a query, evaluates
//! // it, then converts the result into a plain text string.
//! println!("{}", rink_core::one_line(&mut ctx, "kWh / year -> W")?);
//! // Prints: approx. 0.1140795 watt (power)
//! # Ok(())
//! # }
//! ```
//!
//! ## Currency fetching
//!
//! The first step to adding currency fetching is to add code to
//! download this file:
//!
//! <https://rinkcalc.app/data/currency.json>
//!
//! You can use any http library, such as `curl` or `reqwest`. The file
//! updates about once an hour. Please make sure to set an accurate
//! user-agent when fetching it.
//!
//! ```rust
//! # fn fetch_from_http(_url: &str) -> String { include_str!("../tests/currency.snapshot.json").to_owned() }
//! # fn main() -> Result<(), String> {
//! # let mut ctx = rink_core::simple_context()?;
//! let live_data: String = fetch_from_http("https://rinkcalc.app/data/currency.json");
//! // CURRENCY_FILE requires that the `bundle-features` feature is
//! // enabled. Otherwise, you'll need to install and load this file
//! // yourself.
//! let base_defs = rink_core::CURRENCY_FILE.expect("bundle-files feature to be enabled");
//! ctx.load_currency(Some(&live_data), base_defs)?;
//!
//! println!("{}", rink_core::one_line(&mut ctx, "USD").unwrap());
//! // Definition: USD = (1 / 1.0843) EUR = approx. 922.2539 millieuro (money; EUR).
//! // Sourced from European Central Bank. Current as of 2024-05-27.
//! # Ok(())
//! # }
//! ```
//!
//! If `None` is passed as the live data, then attempting to use any currency
//! units will return `QueryError::MissingDeps` to indicate that currency data
//! needs fetching.
//!
//! ## Markup
//!
//! To add color highlighting, or other forms of rich markup such as
//! links or superscripts, you can use [eval] instead of [one_line] and
//! then call [output::fmt::TokenFmt::to_spans] on the result. This
//! returns a tree of spans, each of which has a formatting hint
//! attached to it. See [output::fmt::Span] and [output::fmt::FmtToken].
//!
//! ```rust
//! use rink_core::output::fmt::{TokenFmt, Span, FmtToken};
//! # let mut ctx = rink_core::simple_context().unwrap();
//! let result = rink_core::eval(&mut ctx, "meter");
//! // converts both the Ok and Err cases to spans
//! let spans = result.to_spans();
//!
//! fn write_xml(out: &mut String, spans: &[Span]) {
//!     for span in spans {
//!         match span {
//!             Span::Content {text, token: FmtToken::DocString} => {
//!                 out.push_str("<i>");
//!                 out.push_str(&text);
//!                 out.push_str("</i>");
//!             }
//!             Span::Content {text, ..} => out.push_str(&text),
//!             Span::Child(child) => write_xml(out, &child.to_spans()),
//!         }
//!     }
//! }
//!
//! let mut out = String::new();
//! write_xml(&mut out, &spans);
//! println!("{}", out);
//! ```

// False positives, or make code harder to understand.
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::from_str_radix_10)]
#![allow(clippy::option_as_ref_deref)]
#![allow(clippy::needless_lifetimes)]

pub mod ast;
pub mod commands;
pub mod loader;
pub mod output;
pub mod parsing;
pub mod runtime;
pub mod types;

pub(crate) mod algorithms;
mod helpers;

pub use crate::loader::Context;
pub use crate::runtime::Value;
pub use helpers::{
    eval, one_line, reformat, simple_context, version, CURRENCY_FILE, DATES_FILE, DEFAULT_FILE,
};
