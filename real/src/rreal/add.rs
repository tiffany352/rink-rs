use super::{term::Precedence, BigFloat, RReal, Term};

#[derive(Debug)]
pub struct Add {
    pub left: RReal,
    pub right: RReal,
}

impl Term for Add {
    fn eval(&self, precision: i64) -> BigFloat {
        let left = self.left.eval(precision + 1);
        let right = self.right.eval(precision + 1);
        left + right
    }

    fn describe(&self, writer: &mut String, prec: Precedence) {
        self.left.describe(writer, prec);
        writer.push_str(" + ");
        self.right.describe(writer, prec);
    }

    fn precedence(&self) -> Precedence {
        Precedence::Add
    }
}

#[cfg(test)]
mod tests {
    use num::{BigInt, BigRational, One, Zero};

    use crate::rreal::{BigFloat, RReal, Term};

    use super::Add;

    #[test]
    fn test_add_one() {
        let one = Add {
            left: RReal::rational(BigRational::one()),
            right: RReal::rational(BigRational::one()),
        };
        assert_eq!(one.eval(0), BigFloat::new(BigInt::from(2 << 1), 1, true));
        assert_eq!(one.eval(1), BigFloat::new(BigInt::from(2 << 2), 2, true));
    }

    #[test]
    fn test_add_half() {
        let half = Add {
            left: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(2))),
            right: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(2))),
        };
        assert_eq!(half.eval(0), BigFloat::new(BigInt::from(2), 1, true));
        assert_eq!(half.eval(1), BigFloat::new(BigInt::from(4), 2, true));
        assert_eq!(half.eval(2), BigFloat::new(BigInt::from(8), 3, true));
    }

    #[test]
    fn test_add_quarter() {
        let half = Add {
            left: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(4))),
            right: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(4))),
        };
        assert_eq!(half.eval(0), BigFloat::new(BigInt::from(0), 0, false));
        assert_eq!(half.eval(1), BigFloat::new(BigInt::from(2), 2, true));
        assert_eq!(half.eval(2), BigFloat::new(BigInt::from(4), 3, true));
    }

    #[test]
    fn test_add_third() {
        let half = Add {
            left: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(3))),
            right: RReal::rational(BigRational::new(BigInt::from(2), BigInt::from(3))),
        };
        assert_eq!(half.eval(0), BigFloat::new(BigInt::zero(), 0, false));
        assert_eq!(half.eval(1), BigFloat::new(BigInt::one(), 1, false));
        assert_eq!(half.eval(2), BigFloat::new(BigInt::from(3), 2, false));
        assert_eq!(half.eval(3), BigFloat::new(BigInt::from(7), 3, false));
        assert_eq!(half.eval(4), BigFloat::new(BigInt::from(15), 4, false));
    }
}
