// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ::num::cast::ToPrimitive;
use ::num::rational::BigRational as NumRat;
use ::num::traits::{sign::Signed, One, Zero};
use std::cmp::Ord;
use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::bigint::BigInt;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BigRat {
    mpq: NumRat,
}

impl BigRat {
    pub fn one() -> BigRat {
        BigRat { mpq: NumRat::one() }
    }

    pub fn zero() -> BigRat {
        BigRat {
            mpq: NumRat::zero(),
        }
    }

    pub fn ratio(numerator: &BigInt, denominator: &BigInt) -> BigRat {
        BigRat {
            mpq: NumRat::new(numerator.inner().clone(), denominator.inner().clone()),
        }
    }

    pub fn small_ratio(numerator: i64, denominator: i64) -> BigRat {
        BigRat {
            mpq: NumRat::new(
                BigInt::from(numerator).into_inner(),
                BigInt::from(denominator).into_inner(),
            ),
        }
    }

    pub fn into_inner(self) -> NumRat {
        self.mpq
    }

    pub fn numer(&self) -> BigInt {
        BigInt::from(self.mpq.numer().clone())
    }

    pub fn denom(&self) -> BigInt {
        BigInt::from(self.mpq.denom().clone())
    }

    pub fn abs(&self) -> BigRat {
        BigRat {
            mpq: self.mpq.abs(),
        }
    }

    pub fn as_float(&self) -> f64 {
        self.mpq.to_f64().unwrap()
    }
}

impl From<NumRat> for BigRat {
    fn from(mpq: NumRat) -> BigRat {
        BigRat { mpq }
    }
}

impl From<f64> for BigRat {
    fn from(value: f64) -> BigRat {
        let mpq = NumRat::from_float(value).unwrap();
        BigRat { mpq }
    }
}

impl<'a> Add for &'a BigRat {
    type Output = BigRat;

    fn add(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            mpq: &self.mpq + &rhs.mpq,
        }
    }
}

impl<'a> Sub for &'a BigRat {
    type Output = BigRat;

    fn sub(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            mpq: &self.mpq - &rhs.mpq,
        }
    }
}

impl<'a> Neg for &'a BigRat {
    type Output = BigRat;

    fn neg(self) -> BigRat {
        BigRat { mpq: -&self.mpq }
    }
}

impl<'a> Mul for &'a BigRat {
    type Output = BigRat;

    fn mul(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            mpq: &self.mpq * &rhs.mpq,
        }
    }
}

impl<'a> Div for &'a BigRat {
    type Output = BigRat;

    fn div(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            mpq: &self.mpq / &rhs.mpq,
        }
    }
}
