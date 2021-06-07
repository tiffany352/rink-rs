use super::{term::Precedence, Approx, RReal, Term};

#[derive(Debug)]
pub struct Mul {
    pub left: RReal,
    pub right: RReal,
}

impl Term for Mul {
    fn eval(&self, precision: u64) -> Approx {
        let left = self.left.eval(precision + 1);
        let right = self.right.eval(precision + 1);
        let value = (left.value + right.value) >> 1;
        Approx::new(value, precision, left.exact && right.exact)
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
