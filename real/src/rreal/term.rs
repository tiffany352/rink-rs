use super::Approx;
use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Precedence {
    Add,
    Mul,
    Recip,
    Pow,
    Neg,
    Term,
}

pub trait Term: Debug + Send + Sync + 'static {
    fn eval(&self, precision: u64) -> Approx;

    fn precedence(&self) -> Precedence;
    fn describe(&self, output: &mut String, prec: Precedence);
}
