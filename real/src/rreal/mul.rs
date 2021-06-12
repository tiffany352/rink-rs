use super::{term::Precedence, BigFloat, RReal, Term};

#[derive(Debug)]
pub struct Mul {
    pub left: RReal,
    pub right: RReal,
}

impl Term for Mul {
    fn eval(&self, _precision: i64) -> BigFloat {
        todo!()
    }

    fn describe(&self, writer: &mut String, prec: Precedence) {
        self.left.describe(writer, prec);
        writer.push_str(" * ");
        self.right.describe(writer, prec);
    }

    fn precedence(&self) -> Precedence {
        Precedence::Mul
    }
}
