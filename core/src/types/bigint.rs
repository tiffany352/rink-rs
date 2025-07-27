// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use num_bigint::BigInt as NumInt;
use num_traits::{Num, One, Signed, ToPrimitive, Zero};
use std::cmp::Ord;
use std::fmt;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, MulAssign, Rem, Sub};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
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

    pub fn next_power_of(&self, base: u8) -> usize {
        let mut value = BigInt::one();
        let base = BigInt::from(base as i64);
        let mut i = 0;
        loop {
            if self <= &value {
                break i;
            }
            value = &value * &base;
            i += 1;
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        self.inner.to_i64()
    }

    pub fn abs(&self) -> BigInt {
        BigInt {
            inner: self.inner.abs(),
        }
    }

    pub fn factorial(n: u32) -> BigInt {
        if n == 0 {
            return BigInt::one();
        }
        let mut value: BigInt = (n as u64).into();
        for i in (1..n as i64).rev() {
            value *= i;
        }
        value
    }
}

impl fmt::Display for BigInt {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt(fmt)
    }
}

impl fmt::Debug for BigInt {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl From<i32> for BigInt {
    fn from(value: i32) -> BigInt {
        (value as i64).into()
    }
}

impl<'a> Add for &'a BigInt {
    type Output = BigInt;

    fn add(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner + &rhs.inner,
        }
    }
}

impl<'a> Sub for &'a BigInt {
    type Output = BigInt;

    fn sub(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner - &rhs.inner,
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

impl MulAssign<i64> for BigInt {
    fn mul_assign(&mut self, rhs: i64) {
        self.inner *= rhs;
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

impl<'a> BitAnd for &'a BigInt {
    type Output = BigInt;

    fn bitand(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner & &rhs.inner,
        }
    }
}

impl<'a> BitOr for &'a BigInt {
    type Output = BigInt;

    fn bitor(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner | &rhs.inner,
        }
    }
}

impl<'a> BitXor for &'a BigInt {
    type Output = BigInt;

    fn bitxor(self, rhs: &'a BigInt) -> BigInt {
        BigInt {
            inner: &self.inner ^ &rhs.inner,
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

    #[test]
    fn test_factorial() {
        assert_eq!(BigInt::factorial(0), 1.into());
        assert_eq!(BigInt::factorial(1), 1.into());
        assert_eq!(BigInt::factorial(2), 2.into());
        assert_eq!(BigInt::factorial(3), 6.into());
        assert_eq!(BigInt::factorial(4), 24.into());
        assert_eq!(BigInt::factorial(5), 120.into());
        assert_eq!(
            BigInt::factorial(16),
            BigInt::from_str_radix("20_922_789_888_000", 10).unwrap()
        );
    }
}
