// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! [Value] stores all types that rink is capable of processing

mod eval;
mod substance;
mod value;

pub(crate) use eval::{eval_expr, eval_query};
pub(crate) use value::Show;

pub use substance::{Properties, Property, Substance, SubstanceGetError};
pub use value::MissingDeps;
pub use value::Value;
