// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use gmp::mpq::Mpq;
use std::ops::{Add, Div, Mul, Sub};

use crate::bigint::BigInt;

pub struct BigRat {
    mpq: Mpq,
}

impl BigRat {
    pub fn one() -> BigRat {
        BigRat { mpq: Mpq::one() }
    }

    pub fn zero() -> BigRat {
        BigRat { mpq: Mpq::zero() }
    }

    pub fn ratio(numerator: &BigInt, denominator: &BigInt) -> BigRat {
        BigRat {
            mpq: Mpq::ratio(numerator.raw_mpz(), denominator.raw_mpz()),
        }
    }

    pub fn small_ratio(numerator: i64, denominator: i64) -> BigRat {
        Self::ratio(&BigInt::from(numerator), &BigInt::from(denominator))
    }

    pub fn into_inner(self) -> Mpq {
        self.mpq
    }

    pub fn numer(&self) -> BigInt {
        BigInt::from(self.mpq.get_num())
    }

    pub fn denom(&self) -> BigInt {
        BigInt::from(self.mpq.get_den())
    }
}

impl From<Mpq> for BigRat {
    fn from(mpq: Mpq) -> BigRat {
        BigRat { mpq }
    }
}

impl From<f64> for BigRat {
    fn from(value: f64) -> BigRat {
        let mut mpq = Mpq::one();
        mpq.set_d(value);
        BigRat { mpq }
    }
}

impl PartialEq for BigRat {
    fn eq(&self, other: &Self) -> bool {
        self.mpq.eq(&other.mpq)
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
