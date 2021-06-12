use super::{term::Precedence, BigFloat, RReal, Term};
use num::{BigInt, One, Zero};

#[derive(Debug)]
pub struct Recip(pub RReal);

// x * 2^-p = 1 / (y * 2^-p)
// x * 2^-p = 1 / y * 2^-p
// x * 2^-p = 2^p / y * 2^-p * 2^-p
// x = 2^p / y * 2^-p

impl Term for Recip {
    fn eval(&self, precision: i64) -> BigFloat {
        let sample = self.0.eval(precision * 2 + 3);
        let one = BigInt::one() << (precision * 3 + 3);

        let value: BigInt = &one / sample.mantissa();
        let exact = (&one % sample.mantissa()).is_zero();

        BigFloat::new(value, precision, sample.is_exact() && exact)
    }

    fn describe(&self, writer: &mut String, prec: Precedence) {
        writer.push('/');
        self.0.describe(writer, prec)
    }

    fn precedence(&self) -> Precedence {
        Precedence::Recip
    }
}

#[cfg(test)]
mod tests {
    use num::{BigInt, BigRational};

    use super::Recip;
    use crate::rreal::{BigFloat, RReal, Term};

    #[test]
    fn recip_one() {
        let one = Recip(RReal::one());
        assert_eq!(one.eval(0), BigFloat::new(BigInt::from(1), 0, true));
        assert_eq!(one.eval(1), BigFloat::new(BigInt::from(2), 1, true));
        assert_eq!(one.eval(2), BigFloat::new(BigInt::from(4), 2, true));
        assert_eq!(one.eval(3), BigFloat::new(BigInt::from(8), 3, true));
    }

    #[test]
    fn recip_half() {
        let half = Recip(RReal::rational(BigRational::new(
            BigInt::from(1),
            BigInt::from(2),
        )));
        assert_eq!(half.eval(0), BigFloat::new(BigInt::from(2), 0, true));
        assert_eq!(half.eval(1), BigFloat::new(BigInt::from(4), 1, true));
        assert_eq!(half.eval(2), BigFloat::new(BigInt::from(8), 2, true));
        assert_eq!(half.eval(3), BigFloat::new(BigInt::from(16), 3, true));
    }

    #[test]
    fn recip_third() {
        let half = Recip(RReal::rational(BigRational::new(
            BigInt::from(1),
            BigInt::from(3),
        )));
        assert_eq!(half.eval(0), BigFloat::new(BigInt::from(4), 0, false));
        assert_eq!(half.eval(1), BigFloat::new(BigInt::from(6), 1, false));
        assert_eq!(half.eval(2), BigFloat::new(BigInt::from(12), 2, false));
        assert_eq!(half.eval(3), BigFloat::new(BigInt::from(24), 3, false));
    }

    #[test]
    fn recip_two() {
        let two = Recip(RReal::rational(BigRational::new(
            BigInt::from(2),
            BigInt::from(1),
        )));
        assert_eq!(two.eval(0), BigFloat::new(BigInt::from(0), 0, false));
        assert_eq!(two.eval(1), BigFloat::new(BigInt::from(1), 1, true));
        assert_eq!(two.eval(2), BigFloat::new(BigInt::from(2), 2, true));
        assert_eq!(two.eval(3), BigFloat::new(BigInt::from(4), 3, true));
    }

    #[test]
    fn recip_four() {
        let two = Recip(RReal::rational(BigRational::new(
            BigInt::from(4),
            BigInt::from(1),
        )));
        assert_eq!(two.eval(0), BigFloat::new(BigInt::from(0), 0, false));
        assert_eq!(two.eval(1), BigFloat::new(BigInt::from(0), 1, false));
        assert_eq!(two.eval(2), BigFloat::new(BigInt::from(1), 2, true));
        assert_eq!(two.eval(3), BigFloat::new(BigInt::from(2), 3, true));
    }

    #[test]
    fn recip_three() {
        let two = Recip(RReal::rational(BigRational::new(
            BigInt::from(3),
            BigInt::from(1),
        )));
        assert_eq!(two.eval(0), BigFloat::new(BigInt::from(0), 0, false));
        assert_eq!(two.eval(1), BigFloat::new(BigInt::from(0), 1, false));
        assert_eq!(two.eval(2), BigFloat::new(BigInt::from(1), 2, false));
        assert_eq!(two.eval(3), BigFloat::new(BigInt::from(2), 3, false));
        assert_eq!(two.eval(4), BigFloat::new(BigInt::from(5), 4, false));
        assert_eq!(two.eval(5), BigFloat::new(BigInt::from(10), 5, false));
        assert_eq!(two.eval(6), BigFloat::new(BigInt::from(21), 6, false));
    }
}
