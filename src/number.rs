// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use gmp::mpq::Mpq;
use gmp::mpz::Mpz;
use std::collections::BTreeMap;
use eval::Show;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::rc::Rc;
use std::fmt;
use std::borrow::Borrow;

/// Number type
pub type Num = Mpq;
/// Alias for the primary representation of dimensionality.
pub type Unit = BTreeMap<Dim, i64>;

/// A newtype for a string dimension ID, so that we can implement traits for it.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Dim(pub Rc<String>);

/// The basic representation of a number with a unit.
#[derive(Clone, PartialEq, Eq)]
pub struct Number(pub Num, pub Unit);

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

fn one() -> Mpq {
    Mpq::one()
}

fn zero() -> Mpq {
    Mpq::zero()
}

pub fn pow(left: &Mpq, exp: i32) -> Mpq {
    if exp < 0 {
        one() / pow(left, -exp)
    } else {
        let num = left.get_num().pow(exp as u32);
        let den = left.get_den().pow(exp as u32);
        Mpq::ratio(&num, &den)
    }
}

fn root(left: &Mpq, n: i32) -> Mpq {
    if n < 0 {
        one() / root(left, -n)
    } else {
        let num = left.get_num().root(n as u32);
        let den = left.get_den().root(n as u32);
        Mpq::ratio(&num, &den)
    }
}

pub fn to_string(rational: &Mpq) -> (bool, String) {
    use std::char::from_digit;

    let sign = *rational < Mpq::zero();
    let rational = rational.abs();
    let num = rational.get_num();
    let den = rational.get_den();
    let intdigits = (&num / &den).size_in_base(10) as u32;

    let mut buf = String::new();
    if sign {
        buf.push('-');
    }
    let zero = Mpq::zero();
    let one = Mpz::one();
    let ten = Mpz::from(10);
    let ten_mpq = Mpq::ratio(&ten, &one);
    let mut cursor = rational / Mpq::ratio(&ten.pow(intdigits), &one);
    let mut n = 0;
    let mut only_zeros = true;
    let mut zeros = 0;
    let mut placed_decimal = false;
    loop {
        let exact = cursor == zero;
        let use_sci = intdigits+zeros > 9;
        let placed_ints = n >= intdigits;
        let bail =
            (exact && (placed_ints || use_sci)) ||
            (n as i32 - zeros as i32 > 6 && use_sci) ||
            n as i32 - zeros as i32 > ::std::cmp::max(intdigits as i32, 6);
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
            buf.push(from_digit(v as u32, 10).unwrap());
        }
        cursor = &cursor * &ten_mpq;
        cursor = &cursor - &Mpq::ratio(&digit, &one);
        n += 1;
    }
}

/// Several stringified properties of a number which are useful for
/// displaying it to a user.
#[derive(Clone, Debug)]
pub struct NumberParts {
    /// Present if the number can be concisely represented exactly.
    /// May be decimal, fraction, or scientific notation.
    pub exact_value: Option<String>,
    /// Present if the number can't be exactly concisely represented
    /// in decimal or scientific notation.
    pub approx_value: Option<String>,
    /// Higher-level unit decomposition, if available.
    pub unit: Option<String>,
    /// The physical quantity associated with the unit, if available.
    pub quantity: Option<String>,
    /// The dimensionality of the unit.
    pub dimensions: String,
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
                    (None, None) => panic!("Number parts had neither exact nor approximate representation"),
                },
                'u' => if let Some(unit) = self.unit.as_ref() {
                    if unit.len() == 0 { continue }
                    write!(out, "{}", unit).unwrap();
                } else {
                    if self.dimensions.len() == 0 { continue }
                    write!(out, "{}", self.dimensions).unwrap();
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
                'd' => if self.unit.is_none() {
                    if self.dimensions.len() == 0 { continue }
                    write!(out, "{}", self.dimensions).unwrap();
                } else {
                    continue
                },
                'D' => if self.dimensions.len() > 0 {
                    write!(out, "{}", self.dimensions).unwrap();
                } else {
                    continue
                },
                'p' => match (self.quantity.as_ref(), self.unit.is_some() && self.dimensions.len() > 0) {
                    (Some(q), true) => write!(out, "({}; {})", q, self.dimensions).unwrap(),
                    (Some(q), false) => write!(out, "({})", q).unwrap(),
                    (None, true) => write!(out, "({})", self.dimensions).unwrap(),
                    (None, false) => continue
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
        Number(one(), Unit::new())
    }

    pub fn one_unit(unit: Dim) -> Number {
        Number::new_unit(one(), unit)
    }

    pub fn zero() -> Number {
        Number(zero(), Unit::new())
    }

    /// Creates a dimensionless value.
    pub fn new(num: Num) -> Number {
        Number(num, Unit::new())
    }

    /// Creates a value with a single dimension.
    pub fn new_unit(num: Num, unit: Dim) -> Number {
        let mut map = Unit::new();
        map.insert(unit, 1);
        Number(num, map)
    }

    pub fn from_parts(integer: &str, frac: Option<&str>, exp: Option<&str>) -> Result<Number, String> {
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
        Ok(Number::new(num))
    }

    /// Computes the reciprocal (1/x) of the value.
    pub fn invert(&self) -> Number {
        Number(&one() / &self.0,
               self.1.iter()
               .map(|(k, &power)| (k.clone(), -power))
               .collect::<Unit>())
    }

    /// Raises a value to a dimensionless integer power.
    pub fn powi(&self, exp: i32) -> Number {
        let unit = self.1.iter()
            .map(|(k, &power)| (k.clone(), power * exp as i64))
            .collect::<Unit>();
        Number(pow(&self.0, exp), unit)
    }

    /// Computes the nth root of a value iff all of its units have
    /// powers divisible by n.
    pub fn root(&self, exp: i32) -> Result<Number, String> {
        if self.0 < Mpq::zero() {
            return Err(format!("Complex numbers are not implemented"))
        }
        let mut res = Unit::new();
        for (dim, &power) in &self.1 {
            if power % exp as i64 != 0 {
                return Err(format!(
                    "Result must have integer dimensions"))
            } else {
                res.insert(dim.clone(), power / exp as i64);
            }
        }
        Ok(Number(root(&self.0, exp), res))
    }

    pub fn pow(&self, exp: &Number) -> Result<Number, String> {
        use std::convert::Into;

        if exp.1.len() != 0 {
            return Err(format!("Exponent must be dimensionless"))
        }
        let mut exp = exp.0.clone();
        exp.canonicalize();
        let num = exp.get_num();
        let den = exp.get_den();
        let one = Mpz::one();
        if den == one {
            let exp: Option<i64> = (&num).into();
            Ok(self.powi(exp.unwrap() as i32))
        } else if num == one {
            let exp: Option<i64> = (&den).into();
            self.root(exp.unwrap() as i32)
        } else {
            Err(format!("Exponent must be either an integer or the reciprocal of an integer"))
        }
    }

    fn numeric_value(&self) -> (Option<String>, Option<String>) {
        match to_string(&self.0) {
            (true, v) => (Some(v), None),
            (false, v) => if {self.0.get_den() > Mpz::from(1_000_000) ||
                              self.0.get_num() > Mpz::from(1_000_000_000u64)} {
                (None, Some(v))
            } else {
                (Some(format!("{:?}", self.0)), Some(v))
            }
        }
    }

    pub fn to_parts_simple(&self) -> NumberParts {
        let (exact, approx) = self.numeric_value();
        NumberParts {
            exact_value: exact,
            approx_value: approx,
            unit: None,
            quantity: None,
            dimensions: Number::unit_to_string(&self.1)
        }
    }

    /// Convert the units of the number from base units to display
    /// units, and possibly apply SI prefixes.
    pub fn prettify(&self, context: &::eval::Context) -> Number {
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
                (&self.0 * &Mpq::ratio(&Mpz::from(1000), &Mpz::one()),
                 (Dim::new("gram"), orig.1))
            } else {
                (self.0.clone(), (orig.0.clone(), orig.1))
            };
            for &(ref p, ref v) in &context.prefixes {
                if !prefixes.contains(&**p) {
                    continue;
                }
                let abs = val.abs();
                if abs >= v.0 && abs < &v.0 * &Mpq::ratio(&Mpz::from(1000), &Mpz::one()) {
                    let res = &val / &v.0;
                    // tonne special case
                    let unit = if &**(orig.0).0 == "gram" && p == "mega" {
                        format!("tonne")
                    } else {
                        format!("{}{}", p, orig.0)
                    };
                    let mut map = BTreeMap::new();
                    map.insert(Dim::new(&*unit), 1);
                    return Number(res, map)
                }
            }
            let mut map = BTreeMap::new();
            map.insert(orig.0.clone(), orig.1.clone());
            Number(val, map)
        } else {
            Number(self.0.clone(), unit)
        }
    }

    pub fn to_parts(&self, context: &::eval::Context) -> NumberParts {
        let value = self.prettify(context);
        let (exact, approx) = value.numeric_value();

        let quantity = context.quantities.get(&self.1).cloned().or_else(|| {
            if self.1.len() == 1 {
                let e = self.1.iter().next().unwrap();
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
            unit: if value.1 != self.1 { Some(Number::unit_to_string(&value.1)) } else { None },
            quantity: quantity,
            dimensions: Number::unit_to_string(&self.1),
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
        if frac.len() > 0 {
            write!(out, " /").unwrap();
            for (dim, exp) in frac {
                let exp = -exp;
                write!(out, " {}", dim).unwrap();
                if exp != 1 {
                    write!(out, "^{}", exp).unwrap();
                }
            }
        }

        if out.len() > 0 {
            out.remove(0);
        }
        String::from_utf8(out).unwrap()
    }

    fn pretty_unit(&self, context: &::eval::Context) -> Unit {
        let pretty = ::factorize::fast_decompose(self, &context.reverse);
        let pretty = pretty.into_iter()
            .map(|(k, p)| (context.canonicalizations.get(&*k.0).map(|x| Dim::new(x)).unwrap_or(k), p))
            .collect::<BTreeMap<_, _>>();
        pretty
    }

    pub fn complexity_score(&self) -> i64 {
        self.1.iter().map(|(_, p)| 1 + p.abs()).fold(0, |a,x| a+x)
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let parts = self.to_parts_simple();
        write!(fmt, "{}", parts)
    }
}

impl Show for Number {
    fn show(&self, context: &::eval::Context) -> String {
        let parts = self.to_parts(context);
        format!("{}", parts)
    }
}

impl<'a, 'b> Add<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn add(self, other: &Number) -> Self::Output {
        if self.1 != other.1 {
            return None
        }
        Some(Number(&self.0 + &other.0, self.1.clone()))
    }
}

impl<'a, 'b> Sub<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn sub(self, other: &Number) -> Self::Output {
        if self.1 != other.1 {
            return None
        }
        Some(Number(&self.0 - &other.0, self.1.clone()))
    }
}

impl<'a> Neg for &'a Number {
    type Output = Option<Number>;

    fn neg(self) -> Self::Output {
        Some(Number(-&self.0, self.1.clone()))
    }
}

impl<'a, 'b> Mul<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn mul(self, other: &Number) -> Self::Output {
        let val = ::btree_merge(&self.1, &other.1, |a, b| if a+b != 0 { Some(a + b) } else { None });
        Some(Number(&self.0 * &other.0, val))
    }
}

impl<'a, 'b> Div<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn div(self, other: &Number) -> Self::Output {
        if other.0 == zero() {
            None
        } else {
            self * &other.invert()
        }
    }
}
