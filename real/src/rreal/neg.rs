use super::{term::Precedence, Approx, RReal, Term};

#[derive(Debug)]
pub struct Neg(pub RReal);

impl Term for Neg {
    fn eval(&self, precision: u64) -> Approx {
        let mut result = self.0.eval(precision);
        result.value = -result.value;
        result
    }

    fn describe(&self, writer: &mut String, prec: Precedence) {
        writer.push('-');
        self.0.describe(writer, prec)
    }

    fn precedence(&self) -> Precedence {
        Precedence::Neg
    }
}
