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

pub mod ast;
pub mod commands;
pub mod loader;
pub mod number;
pub mod output;
pub mod parsing;
pub mod runtime;
pub mod substance;
pub mod types;

pub(crate) mod algorithms;
mod helpers;

pub use crate::loader::Context;
pub use crate::number::Number;
pub use crate::runtime::Value;
pub use helpers::{eval, one_line, simple_context, CURRENCY_FILE, DATES_FILE, DEFAULT_FILE};
