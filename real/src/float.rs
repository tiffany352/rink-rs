use std::{cmp::Ordering, fmt, ops};

use num::{bigint::Sign, BigInt, BigRational, Integer, One, Signed, Zero};

/// Approximates a number n = m / 2^p where n <= 2^-p < 2 * 2^-p.
///
/// This representation is useful for approximating irrational numbers,
/// but has less expressive power than normal rationals, as it can't
/// represent simple fractions like 1/3 exactly.
///
/// Even though this stores only an approximate representation, it still
/// implements Eq and Ord. This is because Eq only asks that properties
/// like a == a -> true, a == b -> b == a, etc. hold. Similarly, Ord is
/// just a specialization of PartialOrd that promises to never return
/// None.
#[derive(Clone, PartialEq, Eq, Ord)]
pub struct BigFloat {
    mantissa: BigInt,
    precision: i64,
    exact: bool,
}

impl BigFloat {
    pub fn new(mantissa: BigInt, precision: i64, exact: bool) -> BigFloat {
        BigFloat {
            mantissa,
            precision,
            exact,
        }
    }

    pub fn from_int(int: impl Into<BigInt>) -> BigFloat {
        BigFloat::new(int.into(), 0, true)
    }

    pub fn zero() -> BigFloat {
        BigFloat::new(BigInt::zero(), 0, true)
    }

    pub fn one() -> BigFloat {
        BigFloat::new(BigInt::one(), 0, true)
    }

    pub fn two_pow(exp: i64) -> BigFloat {
        BigFloat::new(BigInt::one(), exp, true)
    }

    /// Creates an approximation at the specified precision and returns
    /// whether or not it is an exact representation.
    pub fn approximate_rational(value: &BigRational, precision: i64) -> BigFloat {
        let numer = value.numer() << precision;
        let exact = numer.is_multiple_of(value.denom());
        let mantissa = numer / value.denom();
        BigFloat {
            mantissa,
            precision,
            exact,
        }
    }

    pub fn mantissa(&self) -> &BigInt {
        &self.mantissa
    }

    pub fn precision(&self) -> i64 {
        self.precision
    }

    /// Exact representations are stating that all of the inferred
    /// binary digits after the end of the mantissa are implied to be 0.
    pub fn is_exact(&self) -> bool {
        self.exact
    }

    /// Returns the most most significant bit of this float.
    pub fn msb(&self) -> i64 {
        self.mantissa.bits() as i64 - self.precision
    }

    pub fn to_rational(&self) -> BigRational {
        if self.precision >= 0 {
            let denom = BigInt::one() << self.precision;
            BigRational::new(self.mantissa.clone(), denom)
        } else {
            BigRational::from_integer(self.to_integer())
        }
    }

    pub fn to_integer(&self) -> BigInt {
        if self.precision > 0 {
            &self.mantissa >> self.precision
        } else {
            &self.mantissa << -self.precision
        }
    }

    pub fn abs(self) -> BigFloat {
        BigFloat::new(self.mantissa.abs(), self.precision, self.exact)
    }

    pub fn into_inexact(self) -> BigFloat {
        let BigFloat {
            mantissa,
            precision,
            ..
        } = self;
        BigFloat {
            mantissa,
            precision,
            exact: false,
        }
    }

    pub fn is_negative(&self) -> bool {
        self.mantissa.is_negative()
    }

    pub fn is_positive(&self) -> bool {
        self.mantissa.is_positive()
    }
}

impl fmt::Debug for BigFloat {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "{}{} * 2^{}",
            &self.mantissa,
            if self.exact { "" } else { "..." },
            self.precision
        )
    }
}

impl fmt::Display for BigFloat {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rational = self.to_rational();
        let sign = rational.numer().sign();
        let rational = rational.abs();

        let digits = 20;

        let integer = rational.to_integer().to_str_radix(10);
        let fraction = rational.fract() * BigRational::from_integer(BigInt::from(10).pow(digits));
        let exact = self.exact && fraction.is_integer();
        let fraction = fraction.to_integer().to_str_radix(10);

        let fraction = format!("{:0>width$}", fraction, width = digits as usize);
        let fraction = if exact {
            fraction.trim_end_matches('0').to_owned()
        } else {
            fraction
        };
        write!(
            fmt,
            "{}{}{}{}{}",
            if sign == Sign::Minus { "-" } else { "" },
            integer,
            if fraction.is_empty() { "" } else { "." },
            fraction,
            if exact { "" } else { "..." }
        )
    }
}

impl PartialOrd for BigFloat {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.mantissa.sign().cmp(&other.mantissa.sign()) {
            Ordering::Less => return Some(Ordering::Less),
            Ordering::Greater => return Some(Ordering::Greater),
            Ordering::Equal => {}
        }

        match self.msb().cmp(&other.msb()) {
            Ordering::Less => return Some(Ordering::Less),
            Ordering::Greater => return Some(Ordering::Greater),
            Ordering::Equal => {}
        }

        match self.precision.cmp(&other.precision) {
            Ordering::Less => {
                let delta = other.precision - self.precision;
                let value = &self.mantissa << delta;
                Some(value.cmp(&other.mantissa))
            }
            Ordering::Greater => {
                let delta = self.precision - other.precision;
                let value = &other.mantissa << delta;
                Some(self.mantissa.cmp(&value))
            }
            Ordering::Equal => Some(self.mantissa.cmp(&other.mantissa)),
        }
    }
}

impl ops::Add for BigFloat {
    type Output = BigFloat;

    /// Returns a number with 1 bit less precision than the inputs.
    /// Panics if the operands don't have the same precision.
    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl<'a> ops::Add for &'a BigFloat {
    type Output = BigFloat;

    /// Returns a number with 1 bit less precision than the inputs.
    /// Panics if the operands don't have the same precision.
    fn add(self, rhs: Self) -> Self::Output {
        let precision = if self.exact && rhs.exact {
            self.precision.max(rhs.precision)
        } else if self.exact {
            rhs.precision
        } else if rhs.exact {
            self.precision
        } else {
            self.precision.min(rhs.precision)
        };

        fn scale_by(value: &BigInt, scale: i64) -> BigInt {
            if scale > 0 {
                value >> scale
            } else {
                value << -scale
            }
        }

        let mut mantissa = scale_by(&self.mantissa, self.precision - precision);
        mantissa += scale_by(&rhs.mantissa, rhs.precision - precision);
        let exact = self.exact && rhs.exact && mantissa.bit(0) == false;
        let precision = if exact {
            precision
        } else {
            mantissa >>= 1;
            precision - 1
        };
        BigFloat {
            mantissa,
            precision,
            exact,
        }
    }
}

impl<'a> ops::Add<&'a BigFloat> for BigFloat {
    type Output = BigFloat;

    fn add(self, rhs: &'a BigFloat) -> Self::Output {
        &self + rhs
    }
}

impl ops::Add<i64> for BigFloat {
    type Output = BigFloat;

    fn add(self, rhs: i64) -> Self::Output {
        self + BigFloat::from_int(rhs)
    }
}

impl ops::Sub for BigFloat {
    type Output = BigFloat;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl<'a> ops::Sub for &'a BigFloat {
    type Output = BigFloat;

    fn sub(self, rhs: Self) -> Self::Output {
        self + &-rhs.clone()
    }
}

impl<'a> ops::Sub<&'a BigFloat> for BigFloat {
    type Output = BigFloat;

    fn sub(self, rhs: &'a BigFloat) -> Self::Output {
        self + -rhs.clone()
    }
}

impl ops::Sub<i64> for BigFloat {
    type Output = BigFloat;

    fn sub(self, rhs: i64) -> Self::Output {
        self - BigFloat::from_int(rhs)
    }
}

impl ops::Neg for BigFloat {
    type Output = BigFloat;

    fn neg(mut self) -> Self::Output {
        self.mantissa = -self.mantissa;
        self
    }
}

impl ops::Mul for BigFloat {
    type Output = BigFloat;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl<'a> ops::Mul for &'a BigFloat {
    type Output = BigFloat;

    fn mul(self, rhs: Self) -> Self::Output {
        let precision = self.precision + rhs.precision;
        let value = &self.mantissa * &rhs.mantissa;
        BigFloat::new(value, precision, self.exact && rhs.exact)
    }
}

impl ops::Shr<i64> for BigFloat {
    type Output = BigFloat;

    fn shr(self, rhs: i64) -> Self::Output {
        BigFloat {
            mantissa: self.mantissa,
            precision: self.precision + rhs,
            exact: self.exact,
        }
    }
}

impl ops::Shl<i64> for BigFloat {
    type Output = BigFloat;

    fn shl(self, rhs: i64) -> Self::Output {
        BigFloat {
            mantissa: self.mantissa,
            precision: self.precision - rhs,
            exact: self.exact,
        }
    }
}

#[cfg(test)]
mod tests {
    use num::BigInt;

    use crate::float::BigFloat;

    fn bf(value: i64, prec: i64) -> BigFloat {
        BigFloat::new(BigInt::from(value), prec, true)
    }

    #[test]
    fn add() {
        assert_eq!(BigFloat::one() + BigFloat::one(), bf(2, 0));
        assert_eq!(bf(123 << 1, 1) + bf(456 << 1, 1), bf(579 << 1, 1));
    }

    #[test]
    fn mul() {
        assert_eq!(BigFloat::one() * BigFloat::one(), BigFloat::one());
        assert_eq!(bf(1000, 0) * bf(1000, 0), bf(1_000_000, 0));

        assert_eq!(bf(1024, 24) * bf(1024, 24), bf(1024 * 1024, 48));

        // 0.5 * 0.5 = 0.25
        assert_eq!(bf(1, 1) * bf(1, 1), bf(1, 2));
    }

    #[test]
    fn display() {
        assert_eq!(format!("{}", bf(1, -1)), "2");
        assert_eq!(format!("{}", bf(1, 0)), "1");
        assert_eq!(format!("{}", bf(1, 1)), "0.5");
        assert_eq!(format!("{}", bf(1, 2)), "0.25");

        assert_eq!(format!("{}", bf(10, -1)), "20");
        assert_eq!(format!("{}", bf(10, 0)), "10");
        assert_eq!(format!("{}", bf(10, 1)), "5");
        assert_eq!(format!("{}", bf(10, 2)), "2.5");
    }

    #[test]
    fn end_to_end() {
        let left = bf(23 * 1024, 10);
        let right = bf(128, 9);

        assert_eq!(format!("{}", left), "23");
        assert_eq!(format!("{}", right), "0.25");

        assert_eq!(format!("{}", left.clone() * right.clone()), "5.75");
        assert_eq!(format!("{}", left + right), "23.25");
    }
}
