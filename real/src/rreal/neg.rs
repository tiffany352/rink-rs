use super::{Approx, RReal, Term};

#[derive(Debug)]
pub struct Neg(pub RReal);

impl Term for Neg {
    fn eval(&self, precision: u64) -> Approx {
        let mut result = self.0.eval(precision);
        result.value = -result.value;
        result
    }
}
