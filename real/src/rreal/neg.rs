use super::{term::Precedence, BigFloat, RReal, Term};

#[derive(Debug)]
pub struct Neg(pub RReal);

impl Term for Neg {
    fn eval(&self, precision: i64) -> BigFloat {
        let result = self.0.eval(precision);
        -result
    }

    fn describe(&self, writer: &mut String, prec: Precedence) {
        writer.push('-');
        self.0.describe(writer, prec)
    }

    fn precedence(&self) -> Precedence {
        Precedence::Neg
    }
}
