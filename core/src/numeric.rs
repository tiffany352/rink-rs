// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::cmp::{Ordering, PartialOrd};
use std::ops::{Add, Div, Mul, Neg, Sub};

use crate::bigint::BigInt;
use crate::bigrat::BigRat;

/// Number type.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
#[serde(into = "NumericParts")]
pub enum Numeric {
    /// Arbitrary-precision rational fraction.
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

/// Used when converting to string representation to choose desired
/// output mode.
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum Digits {
    Default,
    FullInt,
    Digits(u64),
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NumericParts {
    numer: String,
    denom: String,
    exact_value: Option<String>,
    approx_value: Option<String>,
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
            Numeric::Float(x) => {
                let rational = BigRat::from(x);
                (rational.numer(), rational.denom())
            }
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
        use std::char::from_digit;
        use std::num::FpCategory;

        if let Numeric::Float(value) = *self {
            match value.classify() {
                FpCategory::Nan => return (false, "NaN".to_owned()),
                FpCategory::Infinite if value.is_sign_positive() => {
                    return (false, "Inf".to_owned())
                }
                FpCategory::Infinite => return (false, "-Inf".to_owned()),
                _ => (),
            }
        }

        let sign = *self < Numeric::zero();
        let rational = self.abs();
        let (num, den) = rational.to_rational();
        let rational = match rational {
            Numeric::Rational(rational) => rational,
            Numeric::Float(f) => BigRat::from(f),
        };
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
            let use_sci = if digits != Digits::Default
                || den == one && (base == 2 || base == 8 || base == 16 || base == 32)
            {
                false
            } else {
                intdigits + zeros > 9 * 10 / base as u32
            };
            let placed_ints = n >= intdigits;
            let ndigits = match digits {
                Digits::Default | Digits::FullInt => 6,
                Digits::Digits(n) => intdigits as i32 + n as i32,
            };
            let bail = (exact && (placed_ints || use_sci))
                || (n as i32 - zeros as i32 > ndigits && use_sci)
                || n as i32 - zeros as i32 > ::std::cmp::max(intdigits as i32, ndigits);
            if bail && use_sci {
                // scientific notation
                let off = if n < intdigits { 0 } else { zeros };
                buf = buf[off as usize + placed_decimal as usize + sign as usize..].to_owned();
                buf.insert(1, '.');
                if buf.len() == 2 {
                    buf.insert(2, '0');
                }
                if sign {
                    buf.insert(0, '-');
                }
                buf.push_str(&*format!("e{}", intdigits as i32 - zeros as i32 - 1));
                return (exact, buf);
            }
            if bail {
                return (exact, buf);
            }
            if n == intdigits {
                buf.push('.');
                placed_decimal = true;
            }
            let digit = &(&(&cursor.numer() * &ten) / &cursor.denom()) % &ten;
            let v: Option<i64> = digit.as_int();
            let v = v.unwrap();
            if v != 0 {
                only_zeros = false
            } else if only_zeros {
                zeros += 1;
            }
            if !(v == 0 && only_zeros && n < intdigits - 1) {
                buf.push(from_digit(v as u32, base as u32).unwrap());
            }
            cursor = &cursor * &ten_rational;
            cursor = &cursor - &BigRat::ratio(&digit, &one);
            n += 1;
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

impl From<Numeric> for NumericParts {
    fn from(value: Numeric) -> NumericParts {
        let (exact, approx) = value.string_repr(10, Digits::Default);
        let (num, den) = value.to_rational();
        NumericParts {
            numer: num.to_string(),
            denom: den.to_string(),
            exact_value: exact,
            approx_value: approx,
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
