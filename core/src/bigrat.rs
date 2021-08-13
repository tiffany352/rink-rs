// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use num::cast::ToPrimitive;
use num::rational::BigRational as NumRat;
use num::traits::{sign::Signed, One, Zero};
use std::cmp::Ord;
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::bigint::BigInt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct BigRat {
    inner: NumRat,
}

impl BigRat {
    pub fn one() -> BigRat {
        BigRat {
            inner: NumRat::one(),
        }
    }

    pub fn zero() -> BigRat {
        BigRat {
            inner: NumRat::zero(),
        }
    }

    pub fn ratio(numerator: &BigInt, denominator: &BigInt) -> BigRat {
        BigRat {
            inner: NumRat::new(numerator.inner().clone(), denominator.inner().clone()),
        }
    }

    pub fn small_ratio(numerator: i64, denominator: i64) -> BigRat {
        BigRat {
            inner: NumRat::new(
                BigInt::from(numerator).into_inner(),
                BigInt::from(denominator).into_inner(),
            ),
        }
    }

    pub fn into_inner(self) -> NumRat {
        self.inner
    }

    pub fn numer(&self) -> BigInt {
        BigInt::from(self.inner.numer().clone())
    }

    pub fn denom(&self) -> BigInt {
        BigInt::from(self.inner.denom().clone())
    }

    pub fn abs(&self) -> BigRat {
        BigRat {
            inner: self.inner.abs(),
        }
    }

    pub fn as_float(&self) -> f64 {
        self.inner.to_f64().unwrap()
    }
}

impl From<NumRat> for BigRat {
    fn from(inner: NumRat) -> BigRat {
        BigRat { inner }
    }
}

impl From<f64> for BigRat {
    fn from(value: f64) -> BigRat {
        let inner = NumRat::from_float(value).unwrap();
        BigRat { inner }
    }
}

impl fmt::Display for BigRat {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(fmt)
    }
}

impl<'a> Add for &'a BigRat {
    type Output = BigRat;

    fn add(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            inner: &self.inner + &rhs.inner,
        }
    }
}

impl<'a> Sub for &'a BigRat {
    type Output = BigRat;

    fn sub(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            inner: &self.inner - &rhs.inner,
        }
    }
}

impl<'a> Neg for &'a BigRat {
    type Output = BigRat;

    fn neg(self) -> BigRat {
        BigRat {
            inner: -&self.inner,
        }
    }
}

impl<'a> Mul for &'a BigRat {
    type Output = BigRat;

    fn mul(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            inner: &self.inner * &rhs.inner,
        }
    }
}

impl<'a> Div for &'a BigRat {
    type Output = BigRat;

    fn div(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            inner: &self.inner / &rhs.inner,
        }
    }
}
