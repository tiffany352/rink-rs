use num::{BigInt, BigRational};
use std::{ops, sync::Arc};

mod add;
mod approx;
mod mul;
mod one;
mod pi;
mod rational;
mod sqrt;
mod term;

pub use approx::Approx;
pub use term::Term;

#[derive(Clone, Debug)]
pub struct RReal(Arc<dyn Term>);

impl RReal {
    pub fn eval(&self, precision: u64) -> Approx {
        self.0.eval(precision)
    }

    pub fn one() -> RReal {
        RReal(Arc::new(one::ConstOne))
    }

    pub fn rational(value: BigRational) -> RReal {
        RReal(Arc::new(rational::ConstRational(value)))
    }

    pub fn pi() -> RReal {
        RReal(Arc::new(pi::Pi))
    }

    pub fn sqrt(self) -> RReal {
        RReal(Arc::new(sqrt::Sqrt(self)))
    }
}

impl ops::Add for RReal {
    type Output = RReal;

    fn add(self, rhs: Self) -> Self::Output {
        RReal(Arc::new(add::Add {
            left: self,
            right: rhs,
        }))
    }
}

impl ops::Sub for RReal {
    type Output = RReal;

    fn sub(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl ops::Mul for RReal {
    type Output = RReal;

    fn mul(self, rhs: Self) -> Self::Output {
        RReal(Arc::new(mul::Mul {
            left: self,
            right: rhs,
        }))
    }
}

impl ops::Div for RReal {
    type Output = RReal;

    fn div(self, rhs: Self) -> Self::Output {
        todo!()
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
