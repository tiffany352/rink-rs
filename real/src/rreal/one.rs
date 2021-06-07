use super::{term::Precedence, Approx, Term};
use num::{BigInt, One};

#[derive(Debug)]
pub struct ConstOne;

impl Term for ConstOne {
    fn eval(&self, precision: u64) -> Approx {
        Approx::new(BigInt::one() << precision, precision, true)
    }

    fn describe(&self, writer: &mut String, _prec: Precedence) {
        writer.push_str("1")
    }

    fn precedence(&self) -> Precedence {
        Precedence::Term
    }
}
