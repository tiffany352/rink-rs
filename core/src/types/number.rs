// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{BaseUnit, Dimensionality};
use crate::loader::Context;
use crate::output::{Digits, NumberParts};
use crate::runtime::Show;
use crate::types::{BigInt, BigRat, Numeric};
use serde_derive::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

/// The basic representation of a number with a unit.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Number {
    pub value: Numeric,
    pub unit: Dimensionality,
}

impl Number {
    pub fn one() -> Number {
        Number {
            value: Numeric::one(),
            unit: Dimensionality::new(),
        }
    }

    pub fn one_unit(unit: BaseUnit) -> Number {
        Number::new_unit(Numeric::one(), unit)
    }

    pub fn zero() -> Number {
        Number {
            value: Numeric::zero(),
            unit: Dimensionality::new(),
        }
    }

    /// Creates a dimensionless value.
    pub fn new(num: Numeric) -> Number {
        Number {
            value: num,
            unit: Dimensionality::new(),
        }
    }

    /// Creates a value with a single dimension.
    pub fn new_unit(num: Numeric, unit: BaseUnit) -> Number {
        let unit = Dimensionality::base_unit(unit);
        Number { value: num, unit }
    }

    pub fn from_parts(
        integer: &str,
        frac: Option<&str>,
        exp: Option<&str>,
    ) -> Result<Numeric, String> {
        use std::str::FromStr;

        let num = BigInt::from_str_radix(integer, 10).unwrap();
        let frac = if let Some(ref frac) = frac {
            let frac_digits = frac.len();
            let frac = BigInt::from_str_radix(&*frac, 10).unwrap();
            BigRat::ratio(&frac, &BigInt::from(10u64).pow(frac_digits as u32))
        } else {
            BigRat::zero()
        };
        let exp = if let Some(ref exp) = exp {
            let exp: i32 = match FromStr::from_str(&*exp) {
                Ok(exp) => exp,
                // presumably because it is too large
                Err(e) => return Err(format!("Failed to parse exponent: {}", e)),
            };
            let res = BigInt::from(10u64).pow(exp.abs() as u32);
            if exp < 0 {
                BigRat::ratio(&BigInt::one(), &res)
            } else {
                BigRat::ratio(&res, &BigInt::one())
            }
        } else {
            BigRat::one()
        };
        let num = &BigRat::ratio(&num, &BigInt::one()) + &frac;
        Ok(Numeric::Rational(&num * &exp))
    }

    /// Computes the reciprocal (1/x) of the value.
    pub fn invert(&self) -> Number {
        Number {
            value: &Numeric::one() / &self.value,
            unit: self
                .unit
                .iter()
                .map(|(k, &power)| (k.clone(), -power))
                .collect::<Dimensionality>(),
        }
    }

    /// Raises a value to a dimensionless integer power.
    pub fn powi(&self, exp: i32) -> Number {
        let unit = self
            .unit
            .iter()
            .map(|(k, &power)| (k.clone(), power * exp as i64))
            .collect::<Dimensionality>();
        Number {
            value: self.value.pow(exp),
            unit,
        }
    }

    /// Computes the nth root of a value iff all of its units have
    /// powers divisible by n.
    pub fn root(&self, exp: i32) -> Result<Number, String> {
        if self.value < Numeric::zero() {
            return Err("Complex numbers are not implemented".to_string());
        }
        let mut res = Dimensionality::new();
        for (dim, &power) in self.unit.iter() {
            if power % exp as i64 != 0 {
                return Err("Result must have integer dimensions".to_string());
            } else {
                res.insert(dim.clone(), power / exp as i64);
            }
        }
        Ok(Number {
            value: Numeric::Float(self.value.to_f64().powf(1.0 / exp as f64)),
            unit: res,
        })
    }

    pub fn pow(&self, exp: &Number) -> Result<Number, String> {
        if !exp.dimless() {
            return Err("Exponent must be dimensionless".to_string());
        }
        if exp.value.abs() >= Numeric::from(1 << 31) {
            return Err("Exponent is too large".to_string());
        }
        let (num, den) = exp.value.to_rational();
        let one = BigInt::one();
        if den == one {
            let exp: Option<i64> = num.as_int();
            Ok(self.powi(exp.unwrap() as i32))
        } else if num == one {
            let exp: Option<i64> = den.as_int();
            self.root(exp.unwrap() as i32)
        } else if !self.dimless() {
            Err("Exponentiation must result in integer dimensions".to_string())
        } else {
            let exp = exp.value.to_f64();
            Ok(Number {
                value: Numeric::Float(self.value.to_f64().powf(exp)),
                unit: self.unit.clone(),
            })
        }
    }

    pub fn numeric_value(&self, base: u8, digits: Digits) -> (Option<String>, Option<String>) {
        self.value.string_repr(base, digits)
    }

    pub fn to_parts_simple(&self) -> NumberParts {
        let (exact, approx) = self.numeric_value(10, Digits::Default);
        NumberParts {
            raw_value: Some(self.clone()),
            exact_value: exact,
            approx_value: approx,
            dimensions: Some(Number::unit_to_string(&self.unit)),
            ..Default::default()
        }
    }

    /// Convert the units of the number from base units to display
    /// units, and possibly apply SI prefixes.
    pub fn prettify(&self, context: &Context) -> Number {
        let unit = self.pretty_unit(context);
        if let Some(orig) = unit.as_single() {
            use std::collections::HashSet;
            let prefixes = [
                "milli", "micro", "nano", "pico", "femto", "atto", "zepto", "yocto", "kilo",
                "mega", "giga", "tera", "peta", "exa", "zetta", "yotta",
            ]
            .iter()
            .cloned()
            .collect::<HashSet<&'static str>>();
            // kg special case
            let (val, orig) = if &**(orig.0).id == "kg" || &**(orig.0).id == "kilogram" {
                (
                    &self.value * &Numeric::from(1000).pow(orig.1 as i32),
                    (BaseUnit::new("gram"), orig.1),
                )
            } else {
                (self.value.clone(), (orig.0.clone(), orig.1))
            };
            for &(ref p, ref v) in &context.prefixes {
                if !prefixes.contains(&**p) {
                    continue;
                }
                let abs = val.abs();
                if abs >= v.value.pow(orig.1 as i32)
                    && abs < (&v.value * &Numeric::from(1000)).pow(orig.1 as i32)
                {
                    let res = &val / &v.value.pow(orig.1 as i32);
                    // tonne special case
                    let unit = if &**(orig.0).id == "gram" && p == "mega" {
                        "tonne".to_string()
                    } else {
                        format!("{}{}", p, orig.0)
                    };
                    let unit = Dimensionality::new_dim(BaseUnit::new(&unit), orig.1);
                    return Number { value: res, unit };
                }
            }
            let unit = Dimensionality::new_dim(orig.0.clone(), orig.1);
            Number { value: val, unit }
        } else {
            Number {
                value: self.value.clone(),
                unit,
            }
        }
    }

    pub fn to_parts(&self, context: &Context) -> NumberParts {
        let value = self.prettify(context);
        let (exact, approx) = value.numeric_value(10, Digits::Default);

        let quantity = context.quantities.get(&self.unit).cloned().or_else(|| {
            if let Some((unit, power)) = self.unit.as_single() {
                if power == 1 {
                    Some(unit.to_string())
                } else {
                    Some(format!("{}^{}", unit, power))
                }
            } else {
                None
            }
        });

        NumberParts {
            raw_value: Some(self.clone()),
            exact_value: exact,
            approx_value: approx,
            unit: if value.unit != self.unit {
                Some(Number::unit_to_string(&value.unit))
            } else {
                None
            },
            raw_unit: if value.unit != self.unit {
                Some(value.unit)
            } else {
                None
            },
            quantity,
            dimensions: Some(Number::unit_to_string(&self.unit)),
            raw_dimensions: Some(self.unit.clone()),
            ..Default::default()
        }
    }

    pub fn unit_to_string(unit: &Dimensionality) -> String {
        use std::io::Write;

        let mut out = vec![];
        let mut frac = vec![];

        for (dim, &exp) in unit.iter() {
            if exp < 0 {
                frac.push((dim, exp));
            } else {
                write!(out, " {}", dim).unwrap();
                if exp != 1 {
                    write!(out, "^{}", exp).unwrap();
                }
            }
        }
        if !frac.is_empty() {
            write!(out, " /").unwrap();
            for (dim, exp) in frac {
                let exp = -exp;
                write!(out, " {}", dim).unwrap();
                if exp != 1 {
                    write!(out, "^{}", exp).unwrap();
                }
            }
        }

        if !out.is_empty() {
            out.remove(0);
        }
        String::from_utf8(out).unwrap()
    }

    fn pretty_unit(&self, context: &Context) -> Dimensionality {
        let pretty = crate::algorithms::fast_decompose(self, &context.reverse);
        pretty
            .into_iter()
            .map(|(k, p)| {
                (
                    context
                        .canonicalizations
                        .get(&*k.id)
                        .map(|x| BaseUnit::new(x))
                        .unwrap_or(k),
                    p,
                )
            })
            .collect::<Dimensionality>()
    }

    pub fn complexity_score(&self) -> i64 {
        self.unit.iter().map(|(_, p)| 1 + p.abs()).sum()
    }

    pub fn dimless(&self) -> bool {
        self.unit.is_empty()
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parts = self.to_parts_simple();
        write!(fmt, "{}", parts)
    }
}

impl Show for Number {
    fn show(&self, context: &Context) -> String {
        let parts = self.to_parts(context);
        parts.to_string()
    }
}

impl<'a, 'b> Add<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn add(self, other: &Number) -> Self::Output {
        if self.unit != other.unit {
            return None;
        }
        Some(Number {
            value: &self.value + &other.value,
            unit: self.unit.clone(),
        })
    }
}

impl<'a, 'b> Sub<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn sub(self, other: &Number) -> Self::Output {
        if self.unit != other.unit {
            return None;
        }
        Some(Number {
            value: &self.value - &other.value,
            unit: self.unit.clone(),
        })
    }
}

impl<'a> Neg for &'a Number {
    type Output = Option<Number>;

    fn neg(self) -> Self::Output {
        Some(Number {
            value: -&self.value,
            unit: self.unit.clone(),
        })
    }
}

impl<'a, 'b> Mul<&'b Number> for &'a Number {
    type Output = Option<Number>;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, other: &Number) -> Self::Output {
        let val = crate::algorithms::btree_merge(&self.unit, &other.unit, |a, b| {
            if a + b != 0 {
                Some(a + b)
            } else {
                None
            }
        });
        Some(Number {
            value: &self.value * &other.value,
            unit: val.into(),
        })
    }
}

impl<'a, 'b> Div<&'b Number> for &'a Number {
    type Output = Option<Number>;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, other: &Number) -> Self::Output {
        if other.value == Numeric::zero() || other.value == Numeric::Float(0.0) {
            None
        } else {
            self * &other.invert()
        }
    }
}
