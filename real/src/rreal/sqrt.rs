use super::{term::Precedence, RReal, Term};
use crate::BigFloat;

#[derive(Debug)]
pub struct Sqrt(pub RReal);

impl Term for Sqrt {
    fn eval(&self, precision: i64) -> BigFloat {
        let arg_precision = precision * 2 - 1;
        let sample = self.0.eval(arg_precision);
        sample.sqrt(precision)
    }

    fn describe(&self, writer: &mut String, _prec: Precedence) {
        writer.push_str("sqrt(");
        self.0.describe(writer, Precedence::Add);
        writer.push_str(")");
    }

    fn precedence(&self) -> Precedence {
        Precedence::Term
    }
}
