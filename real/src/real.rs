use std::{cmp::Ordering, fmt, ops};

use num::{bigint::Sign, BigInt, BigRational, One, Signed, Zero};

use crate::{util::log10_int, RReal};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Property {
    /// The number one.
    One,
    /// pi
    Pi,
    /// sqrt(i)
    Sqrt(BigRational),
    /// ln(i)
    LogN(BigRational),
    /// ln_10(i)
    Log10(BigRational),
    /// e^i
    Euler(BigRational),
    /// sin(pi i)
    Sin(BigRational),
    /// tan(pi i)
    Tan(BigRational),
}

impl Property {
    fn reconstitute(&self) -> RReal {
        match self {
            Property::One => RReal::one(),
            Property::Pi => todo!(),
            Property::Sqrt(arg) => RReal::from(arg.clone()).sqrt(),
            Property::LogN(_) => todo!(),
            Property::Log10(_) => todo!(),
            Property::Euler(_) => todo!(),
            Property::Sin(_) => todo!(),
            Property::Tan(_) => todo!(),
        }
    }

    fn is_sqrt(value: &Option<Self>) -> bool {
        match value {
            Some(Property::Sqrt(_)) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringRepr {
    exact: bool,
    sign: Sign,
    integer: String,
    fraction: String,
}

impl fmt::Display for StringRepr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.sign == Sign::Minus {
            write!(f, "-")?;
        }
        write!(f, "{}", self.integer)?;
        if !self.fraction.is_empty() {
            write!(f, ".{}", self.fraction)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Real {
    rational: BigRational,
    real: RReal,
    property: Option<Property>,
}

impl Real {
    fn new(rational: BigRational, property: Property) -> Real {
        Real {
            real: property.reconstitute(),
            rational,
            property: Some(property),
        }
    }

    pub(crate) fn from_rreal(real: RReal) -> Real {
        Real {
            rational: BigRational::one(),
            property: None,
            real,
        }
    }

    /// If this real represents a rational number, return it as a BigRational.
    pub fn as_rational(&self) -> Option<&BigRational> {
        match self.property {
            Some(Property::One) => Some(&self.rational),
            Some(Property::Pi) if self.rational.is_zero() => Some(&self.rational),
            _ => None,
        }
    }

    pub fn zero() -> Real {
        Real::rational(BigRational::zero())
    }

    pub fn one() -> Real {
        Real::rational(BigRational::one())
    }

    pub fn fraction(numer: impl Into<BigInt>, denom: impl Into<BigInt>) -> Real {
        Real::rational(BigRational::new(numer.into(), denom.into()))
    }

    pub fn rational(value: BigRational) -> Real {
        Real::new(value, Property::One)
    }

    pub fn sample(&self, precision: i64) -> (BigRational, bool) {
        if self.is_definitely_zero() {
            return (BigRational::zero(), true);
        }

        let additional = self.rational.denom().bits() as i64 - 1;
        let precision = precision + additional;

        let approx = self.real.eval(precision);
        let mut ratio = approx.to_rational();
        ratio *= &self.rational;

        (ratio, approx.is_exact())
    }

    fn definitely_independent(&self, rhs: &Real) -> bool {
        todo!()
    }

    fn is_definitely_one(&self) -> bool {
        self.property == Some(Property::One) && self.rational.is_one()
    }

    fn is_definitely_zero(&self) -> bool {
        if self.rational.is_zero() {
            return true;
        }
        match self.property {
            Some(Property::Sqrt(ref value)) => value.is_zero(),
            _ => false,
        }
    }

    pub fn is_comparable(&self, rhs: &Real) -> bool {
        // Do both numbers share the same (as determined bythe property)
        // known-non-zero (as determined by theproperty) recursive real
        // factor?
        if self.property.is_some() && self.property == rhs.property {
            return true;
        }

        // Are both rational factors zero?
        if self.rational.is_zero() && rhs.rational.is_zero() {
            return true;
        }

        // Can we prove (using the properties) that neither re-cursive
        // real is a rational multiple of the other? We alsoexplicitly
        // check that at least one of the operands has an absolute
        // value >2^−5000. This ensures that we canfind the nonzero digits
        // in a reasonable amount of time.
        // if Self::definitely_independent(self, rhs) && self > 2^-5000 && rhs > 2^-5000 {
        //     return true;
        // }

        // Do they have the same rational factor, and the
        // samepropertytag? Our properties are either constant
        // ormonotonic in the allowableargvalues, so we can justcompare
        // those
        if self.property.is_some() && self.property == rhs.property && self.rational == rhs.rational
        {
            return true;
        }

        // Do both numbers have properties of the form sqrt(x) (for
        // different x)? In that case they can easily be comparedby
        // comparing the (necessarily rational) squares.
        if Property::is_sqrt(&self.property) && Property::is_sqrt(&rhs.property) {
            return true;
        }

        // Finally, can we prove they are different by just evalu-ating
        // to a fixed precision?
        // (todo)

        false
    }

    pub fn to_string(&self, digits: u32, radix: u32) -> StringRepr {
        let bits = log10_int(digits) + 1;
        let (mut result, exact) = self.sample(bits as i64);
        let sign = result.numer().sign();
        result = result.abs();

        let integer = result.to_integer().to_str_radix(radix);
        let fraction = (result.fract() * BigRational::from_integer(BigInt::from(10).pow(digits)))
            .to_integer()
            .to_str_radix(radix);

        let fraction = format!("{:0>width$}", fraction, width = digits as usize);
        let fraction = if exact {
            fraction.trim_end_matches('0').to_owned()
        } else {
            fraction
        };

        StringRepr {
            exact,
            sign,
            integer,
            fraction,
        }
    }

    fn find_perfect_square_impl(
        value: &BigInt,
        min: &BigInt,
        max: &BigInt,
        mut pivot: BigInt,
    ) -> Option<BigInt> {
        let square = &pivot * &pivot;
        match square.cmp(value) {
            Ordering::Equal => Some(pivot),
            _ if min >= max => None,
            Ordering::Less => {
                pivot += 1;
                let new_pivot = (&pivot + max) >> 1;
                Self::find_perfect_square_impl(value, &pivot, max, new_pivot)
            }
            Ordering::Greater => {
                pivot -= 1;
                let new_pivot = (&pivot + min) >> 1;
                Self::find_perfect_square_impl(value, min, &pivot, new_pivot)
            }
        }
    }

    fn find_perfect_square(value: &BigInt) -> Option<BigInt> {
        if value.is_one() {
            Some(BigInt::one())
        } else if value.is_zero() {
            Some(BigInt::zero())
        } else {
            Self::find_perfect_square_impl(value, &BigInt::one(), value, value / 2)
        }
    }

    pub fn sqrt(self) -> Real {
        if let Some(rat) = self.as_rational() {
            // Find perfect squares and simplify.
            if let (Some(numer), Some(denom)) = (
                Self::find_perfect_square(rat.numer()),
                Self::find_perfect_square(rat.denom()),
            ) {
                return Real::rational(BigRational::new(numer, denom));
            }

            Real::new(BigRational::one(), Property::Sqrt(rat.clone()))
        } else {
            let real = if self.rational.is_one() {
                self.real
            } else {
                self.real * RReal::from(self.rational)
            };

            Real {
                rational: BigRational::one(),
                real: real.sqrt(),
                property: None,
            }
        }
    }

    pub fn reciprocal(self) -> Real {
        if self.is_definitely_zero() {
            panic!("Cannot take the reciprocal of zero");
        }

        if self.property == Some(Property::One) {
            Real {
                rational: self.rational.recip(),
                real: self.real,
                property: self.property,
            }
        } else if self.rational.is_one() {
            Real {
                rational: BigRational::one(),
                real: self.real.reciprocal(),
                property: None,
            }
        } else {
            Real {
                rational: BigRational::one(),
                real: (self.real * RReal::rational(self.rational)).reciprocal(),
                property: None,
            }
        }
    }

    pub fn pi() -> Real {
        Real {
            rational: BigRational::one(),
            real: RReal::pi(),
            property: Some(Property::Pi),
        }
    }
}

impl From<BigRational> for Real {
    fn from(value: BigRational) -> Self {
        Real::rational(value)
    }
}

impl From<BigInt> for Real {
    fn from(value: BigInt) -> Self {
        Real::rational(BigRational::from_integer(value))
    }
}

impl From<i64> for Real {
    fn from(value: i64) -> Self {
        Real::rational(BigRational::from_integer(BigInt::from(value)))
    }
}

impl ops::Add for Real {
    type Output = Real;

    fn add(self, rhs: Self) -> Self::Output {
        if self.property.is_some() && self.property == rhs.property {
            let rational = self.rational + rhs.rational;
            Real {
                rational,
                real: self.real,
                property: self.property,
            }
        } else {
            let left = RReal::rational(self.rational) * self.real;
            let right = RReal::rational(rhs.rational) * rhs.real;
            Real {
                rational: BigRational::one(),
                real: left + right,
                property: None,
            }
        }
    }
}

impl ops::Sub for Real {
    type Output = Real;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl ops::Neg for Real {
    type Output = Real;

    fn neg(mut self) -> Self::Output {
        self.rational = -self.rational;
        self
    }
}

impl ops::Mul for Real {
    type Output = Real;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_definitely_zero() || rhs.is_definitely_zero() {
            Real::zero()
        } else if self.is_definitely_one() {
            rhs
        } else if rhs.is_definitely_one() {
            self
        } else if self.property.is_some() && self.property == rhs.property {
            let rational = self.rational * rhs.rational;
            Real {
                rational,
                real: self.real,
                property: self.property,
            }
        } else {
            let rational = self.rational * rhs.rational;
            let real = self.real * rhs.real;
            Real {
                rational,
                real,
                property: None,
            }
        }
    }
}

impl ops::Div for Real {
    type Output = Real;

    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.reciprocal()
    }
}

#[cfg(test)]
mod tests {
    use crate::Real;
    use num::{BigInt, BigRational};

    fn int(i: i64) -> BigInt {
        BigInt::from(i)
    }

    #[test]
    fn test_perfect_squares() {
        let mut results = vec![];
        for i in 0..20 {
            results.push(Real::find_perfect_square(&int(i)));
        }
        assert_eq!(
            results,
            vec![
                Some(int(0)), // 0
                Some(int(1)), // 1
                None,         // 2
                None,         // 3
                Some(int(2)), // 4
                None,         // 5
                None,         // 6
                None,         // 7
                None,         // 8
                Some(int(3)), // 9
                None,         // 10
                None,         // 11
                None,         // 12
                None,         // 13
                None,         // 14
                None,         // 15
                Some(int(4)), // 16
                None,         // 17
                None,         // 18
                None,         // 19
            ]
        );

        let mut results = vec![];
        for i in 0..200 {
            if let Some(square) = Real::find_perfect_square(&int(i)) {
                results.push(square);
            }
        }
        assert_eq!(results, (0..=14).map(int).collect::<Vec<_>>());

        assert_eq!(Real::find_perfect_square(&int(1 << 60)), Some(int(1 << 30)));
    }

    fn rat(numer: i64, denom: i64) -> Real {
        Real::rational(BigRational::new(BigInt::from(numer), BigInt::from(denom)))
    }

    #[test]
    fn test_to_string_ints() {
        let result = vec![
            Real::from(0),
            Real::from(1),
            Real::from(2),
            Real::from(9),
            Real::from(10),
            Real::from(11),
            Real::from(19),
            Real::from(20),
            Real::from(21),
        ]
        .into_iter()
        .map(|num| format!("{}", num.to_string(5, 10)))
        .collect::<Vec<_>>();
        assert_eq!(
            result,
            vec!["0", "1", "2", "9", "10", "11", "19", "20", "21"]
        );
    }

    #[test]
    fn test_to_string_fracs() {
        let result = vec![
            rat(0, 10),
            rat(1, 10),
            rat(2, 10),
            rat(1, 2),
            rat(5, 4),
            rat(3, 4),
            rat(2, 3),
            rat(1, 7),
        ]
        .into_iter()
        .map(|num| format!("{}", num.to_string(5, 10)))
        .collect::<Vec<_>>();
        assert_eq!(
            result,
            vec!["0", "0.1", "0.2", "0.5", "1.25", "0.75", "0.66666", "0.14285"]
        );
    }
}
