// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use gmp::mpz::Mpz;
use std::cmp::Ord;
use std::fmt;
use std::ops::{Div, Mul, Rem};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct BigInt {
    mpz: Mpz,
}

#[derive(Debug)]
pub enum BigIntError {
    ParseError,
}

impl BigInt {
    pub fn one() -> BigInt {
        BigInt { mpz: Mpz::one() }
    }

    pub fn zero() -> BigInt {
        BigInt { mpz: Mpz::zero() }
    }

    pub fn raw_mpz(&self) -> &Mpz {
        &self.mpz
    }

    pub fn from_str_radix(input: &str, base: u8) -> Result<Self, BigIntError> {
        Mpz::from_str_radix(input, base)
            .map(|mpz| BigInt { mpz })
            .map_err(|_err| BigIntError::ParseError)
    }

    pub fn pow(&self, exponent: u32) -> BigInt {
        BigInt {
            mpz: self.mpz.pow(exponent),
        }
    }

    pub fn size_in_base(&self, base: u8) -> usize {
        self.mpz.size_in_base(base)
    }

    pub fn as_int(&self) -> Option<i64> {
        (&self.mpz).into()
    }
}

impl fmt::Display for BigInt {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.mpz.fmt(fmt)
    }
}

impl From<u64> for BigInt {
    fn from(value: u64) -> BigInt {
        BigInt {
            mpz: Mpz::from(value),
        }
    }
}

impl From<i64> for BigInt {
    fn from(value: i64) -> BigInt {
        BigInt {
            mpz: Mpz::from(value),
        }
    }
}

impl From<Mpz> for BigInt {
    fn from(mpz: Mpz) -> BigInt {
        BigInt { mpz }
    }
}

impl<'a> Mul for &'a BigInt {
    type Output = BigInt;

    fn mul(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            mpz: &self.mpz * &rhs.mpz,
        }
    }
}

impl<'a> Div for &'a BigInt {
    type Output = BigInt;

    fn div(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            mpz: &self.mpz / &rhs.mpz,
        }
    }
}

impl<'a> Rem for &'a BigInt {
    type Output = BigInt;

    fn rem(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            mpz: &self.mpz % &rhs.mpz,
        }
    }
}
