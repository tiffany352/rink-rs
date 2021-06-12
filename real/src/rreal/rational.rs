use super::{term::Precedence, Term};
use crate::BigFloat;
use num::BigRational;

#[derive(Debug)]
pub struct ConstRational(pub BigRational);

impl Term for ConstRational {
    fn eval(&self, precision: i64) -> BigFloat {
        BigFloat::approximate_rational(&self.0, precision)
    }

    fn describe(&self, writer: &mut String, _prec: Precedence) {
        writer.push_str(&format!("{}", self.0))
    }

    fn precedence(&self) -> Precedence {
        Precedence::Term
    }
}
