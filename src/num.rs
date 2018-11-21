// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use gmp::mpq::Mpq;
use gmp::mpz::Mpz;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::cmp::Ordering;

pub type Int = Mpz;

/// Number type.
#[derive(Clone, PartialEq, Debug)]
pub enum Num {
    /// Arbitrary-precision rational fraction.
    Mpq(Mpq),
    /// Machine floats.
    Float(f64),
    // /// Machine ints.
    // Int(i64),
}

enum NumParity {
    Mpq(Mpq, Mpq),
    Float(f64, f64)
}

impl Num {
    pub fn one() -> Num {
        Num::Mpq(Mpq::one())
        //Num::Int(1)
    }

    pub fn zero() -> Num {
        Num::Mpq(Mpq::zero())
        //Num::Int(0)
    }

    pub fn abs(&self) -> Num {
        match *self {
            Num::Mpq(ref mpq) => Num::Mpq(mpq.abs()),
            Num::Float(f) => Num::Float(f.abs()),
        }
    }

    fn parity(&self, other: &Num) -> NumParity {
        match (self, other) {
            (&Num::Float(left), right) =>
                NumParity::Float(left, right.into()),
            (left, &Num::Float(right)) =>
                NumParity::Float(left.into(), right),
            (&Num::Mpq(ref left), &Num::Mpq(ref right)) =>
                NumParity::Mpq(left.clone(), right.clone()),
        }
    }

    pub fn div_rem(&self, other: &Num) -> (Num, Num) {
        match self.parity(other) {
            NumParity::Mpq(left, right) => {
                let div = &left / &right;
                let floor = &div.get_num() / div.get_den();
                let rem = &left - &(&right * &Mpq::ratio(&floor, &Mpz::one()));
                (Num::Mpq(Mpq::ratio(&floor, &Mpz::one())), Num::Mpq(rem))
            },
            NumParity::Float(left, right) => {
                (Num::Float(left / right), Num::Float(left % right))
            },
        }
    }

    pub fn to_rational(&self) -> (Int, Int) {
        match *self {
            Num::Mpq(ref mpq) => (mpq.get_num(), mpq.get_den()),
            Num::Float(mut x) => {
                let mut m = [
                    [1, 0],
                    [0, 1]
                ];
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

                (Int::from(m[0][0]), Int::from(m[1][0]))
            },
        }
    }

    pub fn to_int(&self) -> Option<i64> {
        match *self {
            Num::Mpq(ref mpq) => (&(mpq.get_num() / mpq.get_den())).into(),
            Num::Float(f) => if f.abs() < i64::max_value() as f64 {
                Some(f as i64)
            } else {
                None
            },
        }
    }

    pub fn to_f64(&self) -> f64 {
        self.into()
    }
}

impl From<Mpq> for Num {
    fn from(mpq: Mpq) -> Num {
        Num::Mpq(mpq)
    }
}

impl From<Mpz> for Num {
    fn from(mpz: Mpz) -> Num {
        Num::Mpq(Mpq::ratio(&mpz, &Mpz::one()))
    }
}

impl From<i64> for Num {
    fn from(i: i64) -> Num {
        Num::from(Mpz::from(i))
    }
}

impl<'a> Into<f64> for &'a Num {
    fn into(self) -> f64 {
        match *self {
            Num::Mpq(ref mpq) => mpq.clone().into(),
            Num::Float(f) => f,
        }
    }
}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Num) -> Option<Ordering> {
        match self.parity(other) {
            NumParity::Mpq(left, right) => left.partial_cmp(&right),
            NumParity::Float(left, right) => left.partial_cmp(&right),
        }
    }
}

macro_rules! num_binop {
    ($what:ident, $func:ident) => {
        impl<'a, 'b> $what<&'b Num> for &'a Num {
            type Output = Num;

            fn $func(self, other: &'b Num) -> Num {
                match self.parity(other) {
                    NumParity::Mpq(left, right) =>
                        Num::Mpq(left.$func(&right)),
                    NumParity::Float(left, right) =>
                        Num::Float(left.$func(&right)),
                }
            }
        }
    }
}

num_binop!(Add, add);
num_binop!(Sub, sub);
num_binop!(Mul, mul);
num_binop!(Div, div);

impl<'a> Neg for &'a Num {
    type Output = Num;

    fn neg(self) -> Num {
        match *self {
            Num::Mpq(ref mpq) => Num::Mpq(-mpq),
            Num::Float(f) => Num::Float(-f),
        }
    }
}
