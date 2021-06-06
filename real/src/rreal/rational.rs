use super::{Approx, Term};
use num::{BigRational, Zero};

#[derive(Debug)]
pub struct ConstRational(pub BigRational);

impl Term for ConstRational {
    fn eval(&self, precision: u64) -> Approx {
        let adjusted = self.0.numer() << precision;
        let is_exact = (&adjusted % self.0.denom()).is_zero();
        Approx::new(adjusted / self.0.denom(), precision, is_exact)
    }
}
