// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use num::bigint::BigInt as NumInt;
use num::cast::ToPrimitive;
use num::traits::{Num, One, Zero};
use std::cmp::Ord;
use std::fmt;
use std::ops::{Div, Mul, Rem};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct BigInt {
    inner: NumInt,
}

#[derive(Debug)]
pub enum BigIntError {
    ParseError,
}

impl BigInt {
    pub fn one() -> BigInt {
        BigInt {
            inner: NumInt::one(),
        }
    }

    pub fn zero() -> BigInt {
        BigInt {
            inner: NumInt::zero(),
        }
    }

    pub fn inner(&self) -> &NumInt {
        &self.inner
    }

    pub fn into_inner(self) -> NumInt {
        self.inner
    }

    pub fn from_str_radix(input: &str, base: u32) -> Result<Self, BigIntError> {
        NumInt::from_str_radix(input, base)
            .map(|inner| BigInt { inner })
            .map_err(|_err| BigIntError::ParseError)
    }

    pub fn pow(&self, exponent: u32) -> BigInt {
        BigInt {
            inner: self.inner.pow(exponent),
        }
    }

    pub fn size_in_base(&self, base: u8) -> usize {
        1 + ((self.inner.bits()) as f64 * std::f64::consts::LN_2 / (base as f64).ln()).floor()
            as usize
    }

    pub fn as_int(&self) -> Option<i64> {
        self.inner.to_i64()
    }
}

impl fmt::Display for BigInt {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(fmt)
    }
}

impl From<NumInt> for BigInt {
    fn from(inner: NumInt) -> BigInt {
        BigInt { inner }
    }
}

impl From<u64> for BigInt {
    fn from(value: u64) -> BigInt {
        BigInt {
            inner: NumInt::from(value),
        }
    }
}

impl From<i64> for BigInt {
    fn from(value: i64) -> BigInt {
        BigInt {
            inner: NumInt::from(value),
        }
    }
}

impl<'a> Mul for &'a BigInt {
    type Output = BigInt;

    fn mul(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner * &rhs.inner,
        }
    }
}

impl<'a> Div for &'a BigInt {
    type Output = BigInt;

    fn div(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner / &rhs.inner,
        }
    }
}

impl<'a> Rem for &'a BigInt {
    type Output = BigInt;

    fn rem(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner % &rhs.inner,
        }
    }
}

#[cfg(test)]
mod test {
    use super::BigInt;
    #[test]
    fn test_size_in_base_10() {
        // http://oeis.org/A034887
        let num_digits = [
            1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9,
            10, 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 15, 15, 15, 16, 16,
            16, 16, 17, 17, 17, 18, 18, 18, 19, 19, 19, 19, 20,
        ];

        let mut result = vec![];
        for i in 0..num_digits.len() {
            let num = BigInt::from(2u64).pow(i as u32);
            result.push(num.size_in_base(10));
        }
        assert_eq!(&result[..], &num_digits[..]);
    }
}
