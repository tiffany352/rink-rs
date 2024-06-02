// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Parsers for [rink's query language][text_query], [molecular
//! formulas][formula::substance_from_formula], and [datetimes][datetime].

pub mod datetime;
pub mod formula;
pub mod text_query;
