// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::cmp::{Ordering, PartialOrd};
use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::bigint::BigInt;
use crate::bigrat::BigRat;

mod ser {
    use crate::bigrat::BigRat;
    use serde::Serializer;

    pub fn serialize<S: Serializer>(rat: &BigRat, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&rat.to_string())
    }
}

/// Number type.
#[derive(Clone, PartialEq, Debug, Serialize)]
#[serde(untagged)]
pub enum Numeric {
    /// Arbitrary-precision rational fraction.
    #[serde(with = "ser")]
    Rational(BigRat),
    /// Machine floats.
    Float(f64),
    // /// Machine ints.
    // Int(i64),
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
        //Num::Int(1)
    }

    pub fn zero() -> Numeric {
        Numeric::Rational(BigRat::zero())
        //Num::Int(0)
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
            Numeric::Float(mut x) => {
                let mut m = [[1, 0], [0, 1]];
                let maxden = 1_000_000;

                // loop finding terms until denom gets too big
                loop {
                    let ai = x as i64;
                    if m[1][0] * ai + m[1][1] > maxden {
                        break;
                    }
                    let mut t;
                    t = m[0][0] * ai + m[0][1];
                    m[0][1] = m[0][0];
                    m[0][0] = t;
                    t = m[1][0] * ai + m[1][1];
                    m[1][1] = m[1][0];
                    m[1][0] = t;
                    let tmp = x - ai as f64;
                    if tmp == 0.0 {
                        break; // division by zero
                    }
                    x = tmp.recip();
                    if x as i64 > i64::max_value() / 2 {
                        break; // representation failure
                    }
                }

                (BigInt::from(m[0][0]), BigInt::from(m[1][0]))
            }
        }
    }

    pub fn to_int(&self) -> Option<i64> {
        match *self {
            Numeric::Rational(ref rational) => (&rational.numer() / &rational.denom()).as_int(),
            Numeric::Float(f) => {
                if f.abs() < i64::max_value() as f64 {
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

impl<'a> Into<f64> for &'a Numeric {
    fn into(self) -> f64 {
        match *self {
            Numeric::Rational(ref rational) => rational.as_float(),
            Numeric::Float(f) => f,
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

impl<'a> Neg for &'a Numeric {
    type Output = Numeric;

    fn neg(self) -> Numeric {
        match *self {
            Numeric::Rational(ref rational) => Numeric::Rational(-rational),
            Numeric::Float(f) => Numeric::Float(-f),
        }
    }
}
