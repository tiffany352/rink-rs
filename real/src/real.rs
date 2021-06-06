use std::ops;

use num::{BigInt, BigRational, One, Zero};

use crate::RReal;

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
            Property::Sqrt(_) => todo!(),
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

    pub fn rational(value: BigRational) -> Real {
        Real::new(value, Property::One)
    }

    pub fn sample(&self, precision: u64) -> (BigRational, bool) {
        let additional = self.rational.denom().bits();
        let approx = self.real.eval(precision + additional);
        let ratio = BigRational::new(approx.value, BigInt::one() << precision);
        (ratio, approx.exact)
    }

    fn definitely_independent(&self, rhs: &Real) -> bool {
        todo!()
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

impl ops::Mul for Real {
    type Output = Real;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.property.is_some() && self.property == rhs.property {
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
