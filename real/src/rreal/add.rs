use super::{Approx, RReal, Term};

#[derive(Debug)]
pub struct Add {
    pub left: RReal,
    pub right: RReal,
}

impl Term for Add {
    fn eval(&self, precision: u64) -> Approx {
        let left = self.left.eval(precision + 1);
        let right = self.right.eval(precision + 1);
        let value = (left.value + right.value) >> 1;
        Approx::new(value, precision, left.exact && right.exact)
    }
}

#[cfg(test)]
mod tests {
    use num::{BigInt, BigRational, One, Zero};

    use crate::rreal::{Approx, RReal, Term};

    use super::Add;

    #[test]
    fn test_add_one() {
        let one = Add {
            left: RReal::rational(BigRational::one()),
            right: RReal::rational(BigRational::one()),
        };
        assert_eq!(one.eval(0), Approx::new(BigInt::from(2), 0, true));
        assert_eq!(one.eval(1), Approx::new(BigInt::from(2 << 1), 1, true));
    }

    #[test]
    fn test_add_half() {
        let half = Add {
            left: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(2))),
            right: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(2))),
        };
        assert_eq!(half.eval(0), Approx::new(BigInt::one(), 0, true));
        assert_eq!(half.eval(1), Approx::new(BigInt::from(2), 1, true));
        assert_eq!(half.eval(2), Approx::new(BigInt::from(4), 2, true));
    }

    #[test]
    fn test_add_quarter() {
        let half = Add {
            left: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(4))),
            right: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(4))),
        };
        assert_eq!(half.eval(0), Approx::new(BigInt::zero(), 0, false));
        assert_eq!(half.eval(1), Approx::new(BigInt::from(1), 1, true));
        assert_eq!(half.eval(2), Approx::new(BigInt::from(2), 2, true));
    }

    #[test]
    fn test_add_third() {
        let half = Add {
            left: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(3))),
            right: RReal::rational(BigRational::new(BigInt::from(2), BigInt::from(3))),
        };
        assert_eq!(half.eval(0), Approx::new(BigInt::zero(), 0, false));
        assert_eq!(half.eval(1), Approx::new(BigInt::one(), 1, false));
        assert_eq!(half.eval(2), Approx::new(BigInt::from(3), 2, false));
        assert_eq!(half.eval(3), Approx::new(BigInt::from(7), 3, false));
        assert_eq!(half.eval(4), Approx::new(BigInt::from(15), 4, false));
    }
}
