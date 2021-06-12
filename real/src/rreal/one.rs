use super::{term::Precedence, BigFloat, Term};
use num::{BigInt, One};

#[derive(Debug)]
pub struct ConstOne;

impl Term for ConstOne {
    fn eval(&self, precision: i64) -> BigFloat {
        BigFloat::new(BigInt::one() << precision, precision, true)
    }

    fn describe(&self, writer: &mut String, _prec: Precedence) {
        writer.push_str("1")
    }

    fn precedence(&self) -> Precedence {
        Precedence::Term
    }
}
