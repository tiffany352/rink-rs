use lazy_static::lazy_static;
use num::{BigInt, BigRational, One, Zero};
use std::{
    ops,
    sync::{Arc, Mutex},
};

mod add;
mod approx;
mod mul;
mod neg;
mod one;
mod pi;
mod rational;
mod recip;
mod sqrt;
mod term;

pub use approx::Approx;
pub use term::Term;

#[derive(Clone, Debug)]
pub struct RReal {
    term: Arc<dyn Term>,
    cached: Arc<Mutex<Option<Approx>>>,
}

lazy_static! {
    static ref ONE: RReal = RReal::new(one::ConstOne);
    static ref PI: RReal = RReal::new(pi::Pi);
}

impl RReal {
    pub fn eval(&self, precision: u64) -> Approx {
        let mut lock = self.cached.lock().unwrap();
        if let Some(ref cached) = *lock {
            if cached.precision > precision {
                let delta = cached.precision - precision;
                let exact = cached.exact && {
                    let mask: BigInt = (BigInt::one() << delta) - 1;
                    (&cached.value & mask).is_zero()
                };
                let value = &cached.value >> delta;
                return Approx {
                    precision,
                    value,
                    exact,
                };
            }
        }
        let approx = self.term.eval(precision);
        *lock = Some(approx.clone());
        approx
    }

    fn describe(&self, writer: &mut String, prec: term::Precedence) {
        if prec > self.term.precedence() {
            writer.push('(');
            self.term.describe(writer, prec);
            writer.push(')');
        } else {
            self.term.describe(writer, prec);
        }
    }

    fn new(term: impl Term) -> RReal {
        RReal {
            term: Arc::new(term),
            cached: Arc::new(Mutex::new(None)),
        }
    }

    pub fn one() -> RReal {
        ONE.clone()
    }

    pub fn rational(value: BigRational) -> RReal {
        RReal::new(rational::ConstRational(value))
    }

    pub fn pi() -> RReal {
        PI.clone()
    }

    pub fn sqrt(self) -> RReal {
        RReal::new(sqrt::Sqrt(self))
    }

    pub fn reciprocal(self) -> RReal {
        RReal::new(recip::Recip(self))
    }
}

impl ops::Add for RReal {
    type Output = RReal;

    fn add(self, rhs: Self) -> Self::Output {
        RReal::new(add::Add {
            left: self,
            right: rhs,
        })
    }
}

impl ops::Sub for RReal {
    type Output = RReal;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl ops::Neg for RReal {
    type Output = RReal;

    fn neg(self) -> Self::Output {
        RReal::new(neg::Neg(self))
    }
}

impl ops::Mul for RReal {
    type Output = RReal;

    fn mul(self, rhs: Self) -> Self::Output {
        RReal::new(mul::Mul {
            left: self,
            right: rhs,
        })
    }
}

impl ops::Div for RReal {
    type Output = RReal;

    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.reciprocal()
    }
}

impl From<i64> for RReal {
    fn from(value: i64) -> Self {
        RReal::rational(BigRational::from_integer(BigInt::from(value)))
    }
}

impl From<BigRational> for RReal {
    fn from(value: BigRational) -> Self {
        RReal::rational(value)
    }
}

impl From<BigInt> for RReal {
    fn from(value: BigInt) -> Self {
        RReal::rational(BigRational::from_integer(value))
    }
}
