// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::output::{Digits, NumericParts};
use crate::types::{BigInt, BigRat};
use serde_derive::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

/// Number type.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
#[serde(into = "NumericParts")]
pub enum Numeric {
    /// Arbitrary-precision rational fraction.
    Rational(BigRat),
    /// Machine floats.
    Float(f64),
}

/// Parity represents the result of coercing a pair of `Numeric`s into
/// having the same underlying representation. This is done calling
/// `Numeric::parity`.
enum Parity {
    Rational(BigRat, BigRat),
    Float(f64, f64),
}

impl Numeric {
    pub fn one() -> Numeric {
        Numeric::Rational(BigRat::one())
    }

    pub fn zero() -> Numeric {
        Numeric::Rational(BigRat::zero())
    }

    pub fn from_frac(num: impl Into<BigInt>, den: impl Into<BigInt>) -> Numeric {
        Numeric::Rational(BigRat::ratio(&num.into(), &den.into()))
    }

    pub fn abs(&self) -> Numeric {
        match *self {
            Numeric::Rational(ref rational) => Numeric::Rational(rational.abs()),
            Numeric::Float(f) => Numeric::Float(f.abs()),
        }
    }

    /// Converts a pair of numbers to have the same underlying
    /// representation. If either is a float, both will become floats.
    /// If both are rationals, then they are returned as is.
    fn parity(&self, other: &Numeric) -> Parity {
        match (self, other) {
            (&Numeric::Float(left), right) => Parity::Float(left, right.into()),
            (left, &Numeric::Float(right)) => Parity::Float(left.into(), right),
            (&Numeric::Rational(ref left), &Numeric::Rational(ref right)) => {
                Parity::Rational(left.clone(), right.clone())
            }
        }
    }

    pub fn div_rem(&self, other: &Numeric) -> (Numeric, Numeric) {
        match self.parity(other) {
            Parity::Rational(left, right) => {
                let div = &left / &right;
                let floor = &div.numer() / &div.denom();
                let rem = &left - &(&right * &BigRat::ratio(&floor, &BigInt::one()));
                (
                    Numeric::Rational(BigRat::ratio(&floor, &BigInt::one())),
                    Numeric::Rational(rem),
                )
            }
            Parity::Float(left, right) => {
                (Numeric::Float(left / right), Numeric::Float(left % right))
            }
        }
    }

    pub fn to_rational(&self) -> (BigInt, BigInt) {
        match *self {
            Numeric::Rational(ref rational) => (rational.numer(), rational.denom()),
            Numeric::Float(x) => {
                let rational = BigRat::from(x);
                (rational.numer(), rational.denom())
            }
        }
    }

    pub fn as_bigint(&self) -> Option<BigInt> {
        match *self {
            Numeric::Rational(ref rational) => {
                if rational.denom() == BigInt::one() {
                    Some(rational.numer())
                } else {
                    None
                }
            }
            Numeric::Float(_) => None,
        }
    }

    pub fn to_int(&self) -> Option<i64> {
        match *self {
            Numeric::Rational(ref rational) => (&rational.numer() / &rational.denom()).as_int(),
            Numeric::Float(f) => {
                if !f.is_nan() && !f.is_infinite() && f.abs() < i64::max_value() as f64 {
                    Some(f as i64)
                } else {
                    None
                }
            }
        }
    }

    pub fn to_f64(&self) -> f64 {
        self.into()
    }

    /// Returns (is_exact, repr).
    pub fn to_string(&self, base: u8, digits: Digits) -> (bool, String) {
        use std::num::FpCategory;

        match *self {
            Numeric::Rational(ref rational) => rational.to_string(base, digits),
            Numeric::Float(value) => match value.classify() {
                FpCategory::Nan => return (false, "NaN".to_owned()),
                FpCategory::Infinite if value.is_sign_positive() => {
                    return (false, "Inf".to_owned())
                }
                FpCategory::Infinite => return (false, "-Inf".to_owned()),
                _ => BigRat::from(value).to_string(base, digits),
            },
        }
    }

    pub fn string_repr(&self, base: u8, digits: Digits) -> (Option<String>, Option<String>) {
        match *self {
            Numeric::Rational(ref rational) => {
                let num = rational.numer();
                let den = rational.denom();

                match self.to_string(base, digits) {
                    (true, v) => (Some(v), None),
                    (false, v) => {
                        if den > BigInt::from(1_000u64) || num > BigInt::from(1_000_000u64) {
                            (None, Some(v))
                        } else {
                            (Some(format!("{}/{}", num, den)), Some(v))
                        }
                    }
                }
            }
            Numeric::Float(_f) => (None, Some(self.to_string(base, digits).1)),
        }
    }

    pub fn pow(&self, exp: i32) -> Numeric {
        if exp < 0 {
            &Numeric::one() / &self.pow(-exp)
        } else {
            match *self {
                Numeric::Rational(ref value) => {
                    let num = value.numer().pow(exp as u32);
                    let den = value.denom().pow(exp as u32);
                    Numeric::Rational(BigRat::ratio(&num, &den))
                }
                Numeric::Float(value) => Numeric::Float(value.powi(exp)),
            }
        }
    }
}

impl From<BigRat> for Numeric {
    fn from(rat: BigRat) -> Numeric {
        Numeric::Rational(rat)
    }
}

impl From<BigInt> for Numeric {
    fn from(int: BigInt) -> Numeric {
        Numeric::Rational(BigRat::ratio(&int, &BigInt::one()))
    }
}

impl From<i64> for Numeric {
    fn from(i: i64) -> Numeric {
        Numeric::from(BigInt::from(i))
    }
}

impl<'a> From<&'a Numeric> for f64 {
    fn from(value: &'a Numeric) -> f64 {
        match value {
            Numeric::Rational(ref rational) => rational.as_float(),
            Numeric::Float(f) => *f,
        }
    }
}

impl PartialOrd for Numeric {
    fn partial_cmp(&self, other: &Numeric) -> Option<Ordering> {
        match self.parity(other) {
            Parity::Rational(left, right) => left.partial_cmp(&right),
            Parity::Float(left, right) => left.partial_cmp(&right),
        }
    }
}

macro_rules! num_binop {
    ($what:ident, $func:ident) => {
        impl<'a, 'b> $what<&'b Numeric> for &'a Numeric {
            type Output = Numeric;

            fn $func(self, other: &'b Numeric) -> Numeric {
                match self.parity(other) {
                    Parity::Rational(left, right) => Numeric::Rational(left.$func(&right)),
                    Parity::Float(left, right) => Numeric::Float(left.$func(&right)),
                }
            }
        }
    };
}

num_binop!(Add, add);
num_binop!(Sub, sub);
num_binop!(Mul, mul);
num_binop!(Div, div);
num_binop!(Rem, rem);

impl<'a> Neg for &'a Numeric {
    type Output = Numeric;

    fn neg(self) -> Numeric {
        match *self {
            Numeric::Rational(ref rational) => Numeric::Rational(-rational),
            Numeric::Float(f) => Numeric::Float(-f),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{output::Digits, types::Numeric};

    #[test]
    fn test_tostring_simple() {
        assert_eq!(
            Numeric::from_frac(1, 1).to_string(10, Digits::Default),
            (true, "1".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 2).to_string(10, Digits::Default),
            (true, "0.5".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 8).to_string(10, Digits::Default),
            (true, "0.125".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(7, 8).to_string(10, Digits::Default),
            (true, "0.875".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(123456, 100).to_string(10, Digits::Default),
            (true, "1234.56".to_owned())
        );
    }

    #[test]
    fn test_recurring_fraction() {
        assert_eq!(
            Numeric::from_frac(1, 3).to_string(10, Digits::Default),
            (true, "0.[3]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(2, 3).to_string(10, Digits::Default),
            (true, "0.[6]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 7).to_string(10, Digits::Default),
            (true, "0.[142857]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1000, 3).to_string(10, Digits::Default),
            (true, "333.[3]...".to_owned())
        );
    }

    #[test]
    fn test_exponent() {
        assert_eq!(
            Numeric::from_frac(1_000_000_000_000_000i64, 1).to_string(10, Digits::Default),
            (true, "1.0e15".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1_000_000_000_000_000i64, 3).to_string(10, Digits::Default),
            (false, "3.333333e14".to_owned())
        );
    }

    #[test]
    fn test_negatives() {
        assert_eq!(
            Numeric::from_frac(-123, 1).to_string(10, Digits::Default),
            (true, "-123".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(-1000, 3).to_string(10, Digits::Default),
            (true, "-333.[3]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(-1_000_000_000_000_000i64, 1).to_string(10, Digits::Default),
            (true, "-1.0e15".to_owned())
        );
    }

    #[test]
    fn test_base2() {
        assert_eq!(
            Numeric::from_frac(1, 1).to_string(2, Digits::Default),
            (true, "1".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(2, 1).to_string(2, Digits::Default),
            (true, "10".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(3, 1).to_string(2, Digits::Default),
            (true, "11".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 2).to_string(2, Digits::Default),
            (true, "0.1".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 3).to_string(2, Digits::Default),
            (true, "0.[01]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 5).to_string(2, Digits::Default),
            (true, "0.[0011]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 7).to_string(2, Digits::Default),
            (true, "0.[001]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(1, 9).to_string(2, Digits::Default),
            (true, "0.[000111]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(-1000, 3).to_string(2, Digits::Default),
            (true, "-101001101.[01]...".to_owned())
        );
        assert_eq!(
            Numeric::from_frac(-1_000_000_000_000_000i64, 1).to_string(2, Digits::Default),
            (
                true,
                "-11100011010111111010100100110001101000000000000000".to_owned()
            )
        );
    }
}
