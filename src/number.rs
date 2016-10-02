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
use std::cmp::Ordering;

pub type Int = Mpz;

/// Number type.
#[derive(Clone, PartialEq, Debug)]
pub enum Num {
    /// Arbitrary-precision rational fraction.
    Mpq(Mpq),
    /// Machine floats.
    Float(f64),
    // /// Machine ints.
    // Int(i64),
}

enum NumParity {
    Mpq(Mpq, Mpq),
    Float(f64, f64)
}

impl Num {
    pub fn one() -> Num {
        Num::Mpq(Mpq::one())
        //Num::Int(1)
    }

    pub fn zero() -> Num {
        Num::Mpq(Mpq::zero())
        //Num::Int(0)
    }

    pub fn abs(&self) -> Num {
        match *self {
            Num::Mpq(ref mpq) => Num::Mpq(mpq.abs()),
            Num::Float(f) => Num::Float(f.abs()),
        }
    }

    fn parity(&self, other: &Num) -> NumParity {
        match (self, other) {
            (&Num::Float(left), right) =>
                NumParity::Float(left, right.into()),
            (left, &Num::Float(right)) =>
                NumParity::Float(left.into(), right),
            (&Num::Mpq(ref left), &Num::Mpq(ref right)) =>
                NumParity::Mpq(left.clone(), right.clone()),
        }
    }

    pub fn div_rem(&self, other: &Num) -> (Num, Num) {
        match self.parity(other) {
            NumParity::Mpq(left, right) => {
                let div = &left / &right;
                let floor = &div.get_num() / div.get_den();
                let rem = &left - &(&right * &Mpq::ratio(&floor, &Mpz::one()));
                (Num::Mpq(Mpq::ratio(&floor, &Mpz::one())), Num::Mpq(rem))
            },
            NumParity::Float(left, right) => {
                (Num::Float(left / right), Num::Float(left % right))
            },
        }
    }

    pub fn to_rational(&self) -> (Int, Int) {
        match *self {
            Num::Mpq(ref mpq) => (mpq.get_num(), mpq.get_den()),
            Num::Float(mut x) => {
                let mut m = [
                    [1, 0],
                    [0, 1]
                ];
                let maxden = 1_000_000;

                // loop finding terms until denom gets too big
                loop {
                    let ai = x as i64;
                    if m[1][0] * ai + m[1][1] > maxden {
                        break;
                    }
                    let mut t;
                    t = m[0][0] * ai + m[0][1];
                    m[0][1] = m[0][0];
                    m[0][0] = t;
                    t = m[1][0] * ai + m[1][1];
                    m[1][1] = m[1][0];
                    m[1][0] = t;
                    if x == ai as f64 {
                        break; // division by zero
                    }
                    x = 1.0/(x - ai as f64);
                    if x as i64 > i64::max_value() / 2 {
                        break; // representation failure
                    }
                }

                (Int::from(m[0][0]), Int::from(m[1][0]))
            },
        }
    }

    pub fn to_int(&self) -> Option<i64> {
        match *self {
            Num::Mpq(ref mpq) => (&(mpq.get_num() / mpq.get_den())).into(),
            Num::Float(f) => if f.abs() < i64::max_value() as f64 {
                Some(f as i64)
            } else {
                None
            },
        }
    }

    pub fn to_f64(&self) -> f64 {
        self.into()
    }
}

impl From<Mpq> for Num {
    fn from(mpq: Mpq) -> Num {
        Num::Mpq(mpq)
    }
}

impl From<Mpz> for Num {
    fn from(mpz: Mpz) -> Num {
        Num::Mpq(Mpq::ratio(&mpz, &Mpz::one()))
    }
}

impl From<i64> for Num {
    fn from(i: i64) -> Num {
        Num::from(Mpz::from(i))
    }
}

impl<'a> Into<f64> for &'a Num {
    fn into(self) -> f64 {
        match *self {
            Num::Mpq(ref mpq) => mpq.clone().into(),
            Num::Float(f) => f,
        }
    }
}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Num) -> Option<Ordering> {
        match self.parity(other) {
            NumParity::Mpq(left, right) => left.partial_cmp(&right),
            NumParity::Float(left, right) => left.partial_cmp(&right),
        }
    }
}

macro_rules! num_binop {
    ($what:ident, $func:ident) => {
        impl<'a, 'b> $what<&'b Num> for &'a Num {
            type Output = Num;

            fn $func(self, other: &'b Num) -> Num {
                match self.parity(other) {
                    NumParity::Mpq(left, right) =>
                        Num::Mpq(left.$func(&right)),
                    NumParity::Float(left, right) =>
                        Num::Float(left.$func(&right)),
                }
            }
        }
    }
}

num_binop!(Add, add);
num_binop!(Sub, sub);
num_binop!(Mul, mul);
num_binop!(Div, div);

impl<'a> Neg for &'a Num {
    type Output = Num;

    fn neg(self) -> Num {
        match *self {
            Num::Mpq(ref mpq) => Num::Mpq(-mpq),
            Num::Float(f) => Num::Float(-f),
        }
    }
}

/// Alias for the primary representation of dimensionality.
pub type Unit = BTreeMap<Dim, i64>;

/// A newtype for a string dimension ID, so that we can implement traits for it.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Dim(pub Rc<String>);

/// The basic representation of a number with a unit.
#[derive(Clone, PartialEq)]
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

pub fn to_string(rational: &Num, base: u8) -> (bool, String) {
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
            buf.push(from_digit(v as u32, base as u32).unwrap());
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

impl Default for NumberParts {
    fn default() -> Self {
        NumberParts {
            exact_value: None,
            approx_value: None,
            factor: None,
            divfactor: None,
            raw_unit: None,
            unit: None,
            quantity: None,
            dimensions: None,
        }
    }
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
                    if unit.len() == 0 { continue }
                    let mut frac = vec![];
                    let mut toks = vec![];

                    if let Some(f) = self.factor.as_ref() {
                        toks.push(format!("*"));
                        toks.push(format!("{}", f));
                    }
                    for (dim, &exp) in unit {
                        if exp < 0 {
                            frac.push((dim, exp));
                        } else {
                            if exp == 1 {
                                toks.push(format!("{}", dim))
                            } else {
                                toks.push(format!("{}^{}", dim, exp))
                            }
                        }
                    }
                    if frac.len() > 0 {
                        toks.push(format!("/"));
                        if let Some(d) = self.divfactor.as_ref() {
                            toks.push(format!("{}", d));
                        }
                        for (dim, exp) in frac {
                            let exp = -exp;
                            if exp == 1 {
                                toks.push(format!("{}", dim))
                            } else {
                                toks.push(format!("{}^{}", dim, exp))
                            }
                        }
                    }
                    write!(out, "{}", toks.join(" ")).unwrap();
                } else if let Some(unit) = self.unit.as_ref() {
                    if unit.len() == 0 { continue }
                    if let Some(f) = self.factor.as_ref() {
                        write!(out, "* {} ", f).unwrap();
                    }
                    if let Some(d) = self.divfactor.as_ref() {
                        write!(out, "| {} ", d).unwrap();
                    }
                    write!(out, "{}", unit).unwrap();
                } else if let Some(dim) = self.dimensions.as_ref() {
                    if dim.len() == 0 { continue }
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
                    if self.unit.is_none() || dim.len() == 0 { continue }
                    write!(out, "{}", dim).unwrap();
                } else {
                    continue
                },
                'D' => if let Some(dim) = self.dimensions.as_ref() {
                    if dim.len() == 0 { continue }
                    write!(out, "{}", dim).unwrap();
                } else {
                    continue
                },
                'p' => match (self.quantity.as_ref(), self.dimensions.as_ref().and_then(|x| {
                    if self.unit.is_some() && x.len() > 0 {
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
        Number(Num::one(), Unit::new())
    }

    pub fn one_unit(unit: Dim) -> Number {
        Number::new_unit(Num::one(), unit)
    }

    pub fn zero() -> Number {
        Number(Num::zero(), Unit::new())
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

    pub fn from_parts(integer: &str, frac: Option<&str>, exp: Option<&str>) -> Result<Num, String> {
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
        Number(&Num::one() / &self.0,
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
        if self.0 < Num::zero() {
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
        Ok(Number(Num::Float(self.0.to_f64().powf(1.0 / exp as f64)), res))
    }

    pub fn pow(&self, exp: &Number) -> Result<Number, String> {
        use std::convert::Into;

        if exp.1.len() != 0 {
            return Err(format!("Exponent must be dimensionless"))
        }
        let (num, den) = exp.0.to_rational();
        let one = Int::one();
        if den == one {
            let exp: Option<i64> = (&num).into();
            Ok(self.powi(exp.unwrap() as i32))
        } else if num == one {
            let exp: Option<i64> = (&den).into();
            self.root(exp.unwrap() as i32)
        } else {
            if self.1.len() > 0 {
                Err(format!(
                    "Exponentiation must result in integer dimensions"))
            } else {
                let exp = exp.0.to_f64();
                Ok(Number(Num::Float(
                    self.0.to_f64().powf(exp)),
                    self.1.clone()))
            }
        }
    }

    pub fn numeric_value(&self, base: u8) -> (Option<String>, Option<String>) {
        match self.0 {
            Num::Mpq(ref mpq) => {
                let num = mpq.get_num();
                let den = mpq.get_den();

                match to_string(&self.0, base) {
                    (true, v) => (Some(v), None),
                    (false, v) => if {den > Mpz::from(1_000) ||
                                      num > Mpz::from(1_000_000u64)} {
                        (None, Some(v))
                    } else {
                        (Some(format!("{}/{}", num, den)), Some(v))
                    }
                }
            },
            Num::Float(_f) => {
                (None, Some(to_string(&self.0, base).1))
            },
        }
    }

    pub fn to_parts_simple(&self) -> NumberParts {
        let (exact, approx) = self.numeric_value(10);
        NumberParts {
            exact_value: exact,
            approx_value: approx,
            dimensions: Some(Number::unit_to_string(&self.1)),
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
                (&self.0 * &pow(&Num::from(1000), (*orig.1) as i32),
                 (Dim::new("gram"), orig.1))
            } else {
                (self.0.clone(), (orig.0.clone(), orig.1))
            };
            for &(ref p, ref v) in &context.prefixes {
                if !prefixes.contains(&**p) {
                    continue;
                }
                let abs = val.abs();
                if { abs >= pow(&v.0, (*orig.1) as i32) &&
                     abs < pow(&(&v.0 * &Num::from(1000)),
                               (*orig.1) as i32) } {
                    let res = &val / &pow(&v.0, (*orig.1) as i32);
                    // tonne special case
                    let unit = if &**(orig.0).0 == "gram" && p == "mega" {
                        format!("tonne")
                    } else {
                        format!("{}{}", p, orig.0)
                    };
                    let mut map = BTreeMap::new();
                    map.insert(Dim::new(&*unit), *orig.1);
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

    pub fn to_parts(&self, context: &Context) -> NumberParts {
        let value = self.prettify(context);
        let (exact, approx) = value.numeric_value(10);

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
            raw_unit: if value.1 != self.1 { Some(value.1) } else { None },
            quantity: quantity,
            dimensions: Some(Number::unit_to_string(&self.1)),
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

    fn pretty_unit(&self, context: &Context) -> Unit {
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
    fn show(&self, context: &Context) -> String {
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
        if other.0 == Num::zero() {
            None
        } else {
            self * &other.invert()
        }
    }
}
