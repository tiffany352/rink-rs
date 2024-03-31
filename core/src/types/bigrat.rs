// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use num::cast::ToPrimitive;
use num::rational::BigRational as NumRat;
use num::traits::{sign::Signed, One, Zero};
use serde_derive::{Deserialize, Serialize};
use std::cmp::Ord;
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use crate::output::Digits;

use super::BigInt;

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

    fn to_digits_impl(&self, base: u8, digits: Digits) -> (bool, String) {
        let sign = *self < BigRat::zero();
        let rational = self.abs();
        let num = rational.numer();
        let den = rational.denom();
        let intdigits = (&num / &den).size_in_base(base) as u32;
        let mut buf = String::new();
        if sign {
            buf.push('-');
        }
        let zero = BigRat::zero();
        let one = BigInt::one();
        let ten = BigInt::from(base as u64);
        let ten_rational = BigRat::ratio(&ten, &one);
        let mut cursor = &rational / &BigRat::ratio(&ten.pow(intdigits), &one);
        let mut n = 0;
        let mut only_zeros = true;
        let mut zeros = 0;
        let mut placed_decimal = false;
        loop {
            let exact = cursor == zero;
            let placed_ints = n >= intdigits;
            let ndigits = match digits {
                Digits::Default | Digits::FullInt => 6,
                Digits::Digits(n) => intdigits as i32 + n as i32,
            };
            // Conditions for exiting:
            // 1. The number is already exact and we've placed all the
            //    integer positions, or
            // 2. The number is not exact, but we've placed all the
            //    integer positions, and we don't want to place anymore
            //    digits as the number is getting too long.
            let bail = (exact && placed_ints)
                || n as i32 - zeros as i32 > ::std::cmp::max(intdigits as i32, ndigits);
            if bail {
                return (exact, buf);
            }
            if n == intdigits {
                buf.push('.');
                placed_decimal = true;
            }

            // Indicate recurring decimal sequences up to 10 digits
            // long. The rule here checks if the current remainder
            // (`cursor`) divides cleanly into b^N-1 where b is the base
            // and N is the number of digits to check, then the result
            // of that division is the digits that recur. So for example
            // in 1/7, after the decimal point the remainder will be
            // 1/7, and 7 divides cleanly into 999999 to produce 142857,
            // the digits which recur. For remainders with a numerator
            // other than 1, we ignore the remainder when doing this
            // check, and then multiply the digits by it afterwards.
            if placed_decimal {
                // Do this in machine ints because the extra range
                // isn't necessary.
                if let (Some(numer), Some(denom)) =
                    (cursor.numer().as_int(), cursor.denom().as_int())
                {
                    for i in 1..10 {
                        let test = (base as i64).pow(i) - 1;
                        if test % denom == 0 {
                            // Recurring digits
                            let digits = (test / denom) * numer;
                            buf.push('[');
                            for n in 1..=i {
                                let digit = digits / (base as i64).pow(i - n) % base as i64;
                                buf.push(std::char::from_digit(digit as u32, base as u32).unwrap());
                            }
                            buf.push_str("]...");
                            return (true, buf);
                        }
                    }
                }
            }

            let digit = &(&(&cursor.numer() * &ten) / &cursor.denom()) % &ten;
            let v: Option<i64> = digit.as_int();
            let v = v.unwrap();
            if v != 0 {
                only_zeros = false
            } else if only_zeros {
                zeros += 1;
            }
            if v != 0 || !only_zeros || n >= intdigits - 1 {
                buf.push(std::char::from_digit(v as u32, base as u32).unwrap());
            }
            cursor = &cursor * &ten_rational;
            cursor = &cursor - &BigRat::ratio(&digit, &one);
            n += 1;
        }
    }

    pub fn to_scientific(&self, base: u8, digits: Digits) -> (bool, String) {
        let num = self.numer();
        let den = self.denom();
        let intdigits = (&num / &den).size_in_base(base) as u32 - 1;
        let absexp = BigInt::from(base as i64).pow((intdigits as i64).abs() as u32);

        let rational = if intdigits > 0 {
            self * &BigRat::ratio(&BigInt::one(), &absexp)
        } else {
            self * &BigRat::ratio(&absexp, &BigInt::one())
        };
        let (is_exact, mut result) = rational.to_digits_impl(base, digits);
        if !result.contains('.') {
            result.push('.');
            result.push('0');
        }
        result.push('e');
        result.push_str(&format!("{}", intdigits));
        (is_exact, result)
    }

    pub fn to_string(&self, base: u8, digits: Digits) -> (bool, String) {
        if self == &BigRat::small_ratio(0, 1) {
            return (true, "0".to_owned());
        }

        let abs = self.abs();
        let is_computer_base = base == 2 || base == 8 || base == 16 || base == 32;
        let is_computer_integer = is_computer_base && self.denom() == BigInt::one();
        let can_use_sci = digits == Digits::Default && !is_computer_integer;

        if can_use_sci
            && (&abs >= &BigRat::small_ratio(1_000_000_000, 1)
                || &abs <= &BigRat::small_ratio(1, 1_000_000_000))
        {
            self.to_scientific(base, digits)
        } else {
            self.to_digits_impl(base, digits)
        }
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

impl<'a> Rem for &'a BigRat {
    type Output = BigRat;

    fn rem(self, rhs: &'a BigRat) -> BigRat {
        BigRat {
            inner: &self.inner % &rhs.inner,
        }
    }
}
