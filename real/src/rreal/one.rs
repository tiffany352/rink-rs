use super::{Approx, Term};
use num::{BigInt, One};

#[derive(Debug)]
pub struct ConstOne;

impl Term for ConstOne {
    fn eval(&self, precision: u64) -> Approx {
        Approx::new(BigInt::one() << precision, precision, true)
    }
}
