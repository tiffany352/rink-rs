// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use gmp::mpq::Mpq;
use gmp::mpz::Mpz;
use std::collections::BTreeMap;
use value::Show;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::rc::Rc;
use std::fmt;
use std::borrow::Borrow;
use context::Context;
use num::*;
use ast::Digits;

/// Alias for the primary representation of dimensionality.
pub type Unit = BTreeMap<Dim, i64>;

/// A newtype for a string dimension ID, so that we can implement traits for it.
#[cfg_attr(feature = "nightly", derive(Deserialize))]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Dim(pub Rc<String>);

#[cfg(feature = "nightly")]
impl ::serde::ser::Serialize for Dim {
    fn serialize<S>(
        &self, serializer: &mut S
    ) -> Result<(), S::Error>
    where S: ::serde::ser::Serializer {
        serializer.serialize_str(&**self.0)
    }
}

/// The basic representation of a number with a unit.
#[derive(Clone, PartialEq)]
pub struct Number {
    pub value: Num,
    pub unit: Unit,
}

impl Borrow<str> for Dim {
    fn borrow(&self) -> &str {
        &**self.0
    }
}

impl fmt::Display for Dim {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(fmt)
    }
}

impl Dim {
    pub fn new(dim: &str) -> Dim {
        Dim(Rc::new(dim.to_owned()))
    }
}

pub fn pow(left: &Num, exp: i32) -> Num {
    if exp < 0 {
        &Num::one() / &pow(left, -exp)
    } else {
        let left = match *left {
            Num::Mpq(ref left) => left,
            Num::Float(f) => return Num::Float(f.powi(exp))
        };
        let num = left.get_num().pow(exp as u32);
        let den = left.get_den().pow(exp as u32);
        Num::Mpq(Mpq::ratio(&num, &den))
    }
}

pub fn to_string(rational: &Num, base: u8, digits: Digits) -> (bool, String) {
    use std::char::from_digit;

    let sign = *rational < Num::zero();
    let rational = rational.abs();
    let (num, den) = rational.to_rational();
    let rational = match rational {
        Num::Mpq(mpq) => mpq,
        Num::Float(f) => {
            let mut m = Mpq::one();
            m.set_d(f);
            m
        },
    };
    let intdigits = (&num / &den).size_in_base(base) as u32;

    let mut buf = String::new();
    if sign {
        buf.push('-');
    }
    let zero = Mpq::zero();
    let one = Int::one();
    let ten = Int::from(base as u64);
    let ten_mpq = Mpq::ratio(&ten, &one);
    let mut cursor = &rational / &Mpq::ratio(&ten.pow(intdigits), &one);
    let mut n = 0;
    let mut only_zeros = true;
    let mut zeros = 0;
    let mut placed_decimal = false;
    loop {
        let exact = cursor == zero;
        let use_sci = if digits != Digits::Default || den == one && (base == 2 || base == 8 || base == 16 || base == 32) {
            false
        } else {
            intdigits+zeros > 9 * 10 / base as u32
        };
        let placed_ints = n >= intdigits;
        let ndigits = match digits {
            Digits::Default | Digits::FullInt => 6,
            Digits::Digits(n) => intdigits as i32 + n as i32
        };
        let bail =
            (exact && (placed_ints || use_sci)) ||
            (n as i32 - zeros as i32 > ndigits && use_sci) ||
            n as i32 - zeros as i32 > ::std::cmp::max(intdigits as i32, ndigits);
        if bail && use_sci {
            // scientific notation
            let off = if n < intdigits { 0 } else { zeros };
            buf = buf[off as usize + placed_decimal as usize + sign as usize..].to_owned();
            buf.insert(1, '.');
            if buf.len() == 2 {
                buf.insert(2, '0');
            }
            if sign {
                buf.insert(0, '-');
            }
            buf.push_str(&*format!("e{}", intdigits as i32 - zeros as i32 - 1));
            return (exact, buf)
        }
        if bail {
            return (exact, buf)
        }
        if n == intdigits {
            buf.push('.');
            placed_decimal = true;
        }
        let digit = &(&(&cursor.get_num() * &ten) / &cursor.get_den()) % &ten;
        let v: Option<i64> = (&digit).into();
        let v = v.unwrap();
        if v != 0 {
            only_zeros = false
        } else if only_zeros {
            zeros += 1;
        }
        if !(v == 0 && only_zeros && n < intdigits-1) {
            buf.push(from_digit(v as u32, base as u32).unwrap());
        }
        cursor = &cursor * &ten_mpq;
        cursor = &cursor - &Mpq::ratio(&digit, &one);
        n += 1;
    }
}

/// Several stringified properties of a number which are useful for
/// displaying it to a user.
#[derive(Clone, Debug, Default)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct NumberParts {
    /// Present if the number can be concisely represented exactly.
    /// May be decimal, fraction, or scientific notation.
    pub exact_value: Option<String>,
    /// Present if the number can't be exactly concisely represented
    /// in decimal or scientific notation.
    pub approx_value: Option<String>,
    /// Numerator factor by which the value is multiplied, if not one.
    pub factor: Option<String>,
    /// Divisor factor, if not one.
    pub divfactor: Option<String>,
    /// High-level unit decomposition, in format that can be manipulated.
    pub raw_unit: Option<Unit>,
    /// Higher-level unit decomposition, if available.
    pub unit: Option<String>,
    /// The physical quantity associated with the unit, if available.
    pub quantity: Option<String>,
    /// The dimensionality of the unit.
    pub dimensions: Option<String>,
}

impl NumberParts {
    /// A DSL for formatting numbers.
    ///
    /// - `a`: Approximate numerical value, if exists.
    /// - `e`: Exact numerical value, if exists.
    /// - `n`: Exact and approximate values.
    /// - `u`: Unit.
    /// - `q`: Quantity, if exists.
    /// - `w`: Quantity in parentheses, if exists.
    /// - `d`: Dimensionality, if not same as unit.
    /// - `D`: Dimensionality, always.
    /// - `p`: Quantity and dimensionality in parentheses.
    ///
    /// Display impl is equivalent to `n u w`.
    ///
    /// Whitespace is compacted. Any unrecognized characters are passed through.
    pub fn format(&self, pat: &str) -> String {
        use std::io::Write;

        let mut out = vec![];

        let mut in_ws = true;
        for c in pat.chars() {
            match c {
                'e' => if let Some(ex) = self.exact_value.as_ref() {
                    write!(out, "{}", ex).unwrap();
                } else {
                    continue
                },
                'a' => if let Some(ap) = self.approx_value.as_ref() {
                    write!(out, "{}", ap).unwrap();
                } else {
                    continue
                },
                'n' => match (self.exact_value.as_ref(), self.approx_value.as_ref()) {
                    (Some(ex), Some(ap)) => write!(out, "{}, approx. {}", ex, ap).unwrap(),
                    (Some(ex), None) => write!(out, "{}", ex).unwrap(),
                    (None, Some(ap)) => write!(out, "approx. {}", ap).unwrap(),
                    (None, None) => continue,
                },
                'u' => if let Some(unit) = self.raw_unit.as_ref() {
                    if unit.is_empty() { continue }
                    let mut frac = vec![];
                    let mut toks = vec![];

                    if let Some(f) = self.factor.as_ref() {
                        toks.push("*".to_string());
                        toks.push(f.to_string());
                    }
                    for (dim, &exp) in unit {
                        if exp < 0 {
                            frac.push((dim, exp));
                        } else if exp == 1 {
                            toks.push(dim.to_string())
                        } else {
                            toks.push(format!("{}^{}", dim, exp))
                        }
                    }
                    if !frac.is_empty() {
                        toks.push("/".to_string());
                        if let Some(d) = self.divfactor.as_ref() {
                            toks.push(d.to_string());
                        }
                        for (dim, exp) in frac {
                            let exp = -exp;
                            if exp == 1 {
                                toks.push(dim.to_string())
                            } else {
                                toks.push(format!("{}^{}", dim, exp))
                            }
                        }
                    }
                    write!(out, "{}", toks.join(" ")).unwrap();
                } else if let Some(unit) = self.unit.as_ref() {
                    if unit.is_empty() { continue }
                    if let Some(f) = self.factor.as_ref() {
                        write!(out, "* {} ", f).unwrap();
                    }
                    if let Some(d) = self.divfactor.as_ref() {
                        write!(out, "| {} ", d).unwrap();
                    }
                    write!(out, "{}", unit).unwrap();
                } else if let Some(dim) = self.dimensions.as_ref() {
                    if dim.is_empty() { continue }
                    if let Some(f) = self.factor.as_ref() {
                        write!(out, "* {} ", f).unwrap();
                    }
                    if let Some(d) = self.divfactor.as_ref() {
                        write!(out, "| {} ", d).unwrap();
                    }
                    write!(out, "{}", dim).unwrap();
                } else {
                    continue
                },
                'q' => if let Some(q) = self.quantity.as_ref() {
                    write!(out, "{}", q).unwrap();
                } else {
                    continue
                },
                'w' => if let Some(q) = self.quantity.as_ref() {
                    write!(out, "({})", q).unwrap();
                } else {
                    continue
                },
                'd' => if let Some(dim) = self.dimensions.as_ref() {
                    if self.unit.is_none() || dim.is_empty() { continue }
                    write!(out, "{}", dim).unwrap();
                } else {
                    continue
                },
                'D' => if let Some(dim) = self.dimensions.as_ref() {
                    if dim.is_empty() { continue }
                    write!(out, "{}", dim).unwrap();
                } else {
                    continue
                },
                'p' => match (self.quantity.as_ref(), self.dimensions.as_ref().and_then(|x| {
                    if self.unit.is_some() && !x.is_empty() {
                        Some(x)
                    } else {
                        None
                    }
                })) {
                    (Some(q), Some(d)) => write!(out, "({}; {})", q, d).unwrap(),
                    (Some(q), None) => write!(out, "({})", q).unwrap(),
                    (None, Some(d)) => write!(out, "({})", d).unwrap(),
                    (None, None) => continue
                },
                ' ' if in_ws => continue,
                ' ' if !in_ws => {
                    in_ws = true;
                    write!(out, " ").unwrap();
                    continue
                },
                x => write!(out, "{}", x).unwrap()
            }
            in_ws = false;
        }

        ::std::str::from_utf8(&out[..]).unwrap().trim().to_owned()
    }
}

impl fmt::Display for NumberParts {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.format("n u w"))
    }
}

impl Number {
    pub fn one() -> Number {
        Number {
            value: Num::one(),
            unit: Unit::new(),
        }
    }

    pub fn one_unit(unit: Dim) -> Number {
        Number::new_unit(Num::one(), unit)
    }

    pub fn zero() -> Number {
        Number {
            value: Num::zero(),
            unit: Unit::new(),
        }
    }

    /// Creates a dimensionless value.
    pub fn new(num: Num) -> Number {
        Number {
            value: num,
            unit: Unit::new(),
        }
    }

    /// Creates a value with a single dimension.
    pub fn new_unit(num: Num, unit: Dim) -> Number {
        let mut map = Unit::new();
        map.insert(unit, 1);
        Number {
            value: num,
            unit: map,
        }
    }

    pub fn from_parts(
        integer: &str, frac: Option<&str>, exp: Option<&str>
    ) -> Result<Num, String> {
        use std::str::FromStr;

        let num = Mpz::from_str_radix(integer, 10).unwrap();
        let frac = if let Some(ref frac) = frac {
            let frac_digits = frac.len();
            let frac = Mpz::from_str_radix(&*frac, 10).unwrap();
            Mpq::ratio(&frac, &Mpz::from(10).pow(frac_digits as u32))
        } else {
            Mpq::zero()
        };
        let exp = if let Some(ref exp) = exp {
            let exp: i32 = match FromStr::from_str(&*exp) {
                Ok(exp) => exp,
                // presumably because it is too large
                Err(e) => return Err(format!("Failed to parse exponent: {}", e))
            };
            let res = Mpz::from(10).pow(exp.abs() as u32);
            if exp < 0 {
                Mpq::ratio(&Mpz::one(), &res)
            } else {
                Mpq::ratio(&res, &Mpz::one())
            }
        } else {
            Mpq::one()
        };
        let num = &Mpq::ratio(&num, &Mpz::one()) + &frac;
        let num = &num * &exp;
        Ok(Num::Mpq(num))
    }

    /// Computes the reciprocal (1/x) of the value.
    pub fn invert(&self) -> Number {
        Number {
            value: &Num::one() / &self.value,
            unit: self.unit.iter()
                .map(|(k, &power)| (k.clone(), -power))
                .collect::<Unit>(),
        }
    }

    /// Raises a value to a dimensionless integer power.
    pub fn powi(&self, exp: i32) -> Number {
        let unit = self.unit.iter()
            .map(|(k, &power)| (k.clone(), power * exp as i64))
            .collect::<Unit>();
        Number {
            value: pow(&self.value, exp),
            unit,
        }
    }

    /// Computes the nth root of a value iff all of its units have
    /// powers divisible by n.
    pub fn root(&self, exp: i32) -> Result<Number, String> {
        if self.value < Num::zero() {
            return Err("Complex numbers are not implemented".to_string())
        }
        let mut res = Unit::new();
        for (dim, &power) in &self.unit {
            if power % exp as i64 != 0 {
                return Err("Result must have integer dimensions".to_string())
            } else {
                res.insert(dim.clone(), power / exp as i64);
            }
        }
        Ok(Number {
            value: Num::Float(self.value.to_f64().powf(1.0 / exp as f64)),
            unit: res
        })
    }

    pub fn pow(&self, exp: &Number) -> Result<Number, String> {
        if !exp.dimless() {
            return Err("Exponent must be dimensionless".to_string())
        }
        if exp.value.abs() >= Num::from(1 << 31) {
            return Err("Exponent is too large".to_string())
        }
        let (num, den) = exp.value.to_rational();
        let one = Int::one();
        if den == one {
            let exp: Option<i64> = (&num).into();
            Ok(self.powi(exp.unwrap() as i32))
        } else if num == one {
            let exp: Option<i64> = (&den).into();
            self.root(exp.unwrap() as i32)
        } else if !self.dimless() {
            Err("Exponentiation must result in integer dimensions".to_string())
        } else {
            let exp = exp.value.to_f64();
            Ok(Number {
                value: Num::Float(
                    self.value.to_f64().powf(exp)
                ),
                unit: self.unit.clone()
            })
        }
    }

    pub fn numeric_value(&self, base: u8, digits: Digits) -> (Option<String>, Option<String>) {
        match self.value {
            Num::Mpq(ref mpq) => {
                let num = mpq.get_num();
                let den = mpq.get_den();

                match to_string(&self.value, base, digits) {
                    (true, v) => (Some(v), None),
                    (false, v) => if den > Mpz::from(1_000) ||
                                      num > Mpz::from(1_000_000u64) {
                        (None, Some(v))
                    } else {
                        (Some(format!("{}/{}", num, den)), Some(v))
                    }
                }
            },
            Num::Float(_f) => {
                (None, Some(to_string(&self.value, base, digits).1))
            },
        }
    }

    pub fn to_parts_simple(&self) -> NumberParts {
        let (exact, approx) = self.numeric_value(10, Digits::Default);
        NumberParts {
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
        if unit.len() == 1 {
            use std::collections::HashSet;
            let prefixes = [
                "milli", "micro", "nano", "pico", "femto", "atto", "zepto", "yocto",
                "kilo", "mega", "giga", "tera", "peta", "exa", "zetta", "yotta"]
                .iter().cloned().collect::<HashSet<&'static str>>();
            let orig = unit.iter().next().unwrap();
            // kg special case
            let (val, orig) = if &**(orig.0).0 == "kg" || &**(orig.0).0 == "kilogram" {
                (&self.value * &pow(&Num::from(1000), (*orig.1) as i32),
                 (Dim::new("gram"), orig.1))
            } else {
                (self.value.clone(), (orig.0.clone(), orig.1))
            };
            for &(ref p, ref v) in &context.prefixes {
                if !prefixes.contains(&**p) {
                    continue;
                }
                let abs = val.abs();
                if abs >= pow(&v.value, (*orig.1) as i32) &&
                        abs < pow(&(&v.value * &Num::from(1000)),
                        (*orig.1) as i32) {
                    let res = &val / &pow(&v.value, (*orig.1) as i32);
                    // tonne special case
                    let unit = if &**(orig.0).0 == "gram" && p == "mega" {
                        "tonne".to_string()
                    } else {
                        format!("{}{}", p, orig.0)
                    };
                    let mut map = BTreeMap::new();
                    map.insert(Dim::new(&*unit), *orig.1);
                    return Number {
                        value: res,
                        unit: map,
                    }
                }
            }
            let mut map = BTreeMap::new();
            map.insert(orig.0.clone(), orig.1.clone());
            Number {
                value: val,
                unit: map,
            }
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
            if self.unit.len() == 1 {
                let e = self.unit.iter().next().unwrap();
                let ref n = *e.0;
                if *e.1 == 1 {
                    Some((&*n.0).clone())
                } else {
                    Some(format!("{}^{}", n, e.1))
                }
            } else {
                None
            }
        });

        NumberParts {
            exact_value: exact,
            approx_value: approx,
            unit: if value.unit != self.unit { Some(Number::unit_to_string(&value.unit)) } else { None },
            raw_unit: if value.unit != self.unit { Some(value.unit) } else { None },
            quantity,
            dimensions: Some(Number::unit_to_string(&self.unit)),
            ..Default::default()
        }
    }

    pub fn unit_to_string(unit: &Unit) -> String {
        use std::io::Write;

        let mut out = vec![];
        let mut frac = vec![];

        for (dim, &exp) in unit {
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

    fn pretty_unit(&self, context: &Context) -> Unit {
        let pretty = ::factorize::fast_decompose(self, &context.reverse);
        let pretty = pretty.into_iter()
            .map(|(k, p)| (context.canonicalizations.get(&*k.0).map(|x| Dim::new(x)).unwrap_or(k), p))
            .collect::<BTreeMap<_, _>>();
        pretty
    }

    pub fn complexity_score(&self) -> i64 {
        self.unit.iter().map(|(_, p)| 1 + p.abs()).sum()
    }

    pub fn dimless(&self) -> bool {
        self.unit.is_empty()
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
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
            return None
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
            return None
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

    fn mul(self, other: &Number) -> Self::Output {
        let val = ::btree_merge(&self.unit, &other.unit, |a, b| if a+b != 0 { Some(a + b) } else { None });
        Some(Number {
            value: &self.value * &other.value,
            unit: val,
        })
    }
}

impl<'a, 'b> Div<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn div(self, other: &Number) -> Self::Output {
        if other.value == Num::zero() || other.value == Num::Float(0.0) {
            None
        } else {
            self * &other.invert()
        }
    }
}
