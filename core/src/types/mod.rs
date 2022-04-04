// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod base_unit;
mod bigint;
mod bigrat;
mod date;
mod number;
mod numeric;

pub use base_unit::BaseUnit;
pub use bigint::{BigInt, BigIntError};
pub use bigrat::BigRat;
pub use date::GenericDateTime;
pub use number::{Dimensionality, Number};
pub use numeric::Numeric;

#[deprecated(since = "0.7.0", note = "renamed to BaseUnit")]
pub type Dimension = BaseUnit;

#[deprecated(since = "0.7.0", note = "renamed to Dimensionality")]
pub type Quantity = Dimensionality;
