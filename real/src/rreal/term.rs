use crate::BigFloat;
use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Precedence {
    Add,
    Mul,
    Recip,
    Neg,
    Term,
}

pub trait Term: Debug + Send + Sync + 'static {
    fn eval(&self, precision: i64) -> BigFloat;

    fn precedence(&self) -> Precedence;
    fn describe(&self, output: &mut String, prec: Precedence);
}
