// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::bigrat::BigRat;
use crate::context::Context;
use crate::fmt::TokenFmt;
use crate::numeric::*;
use crate::value::Show;
use crate::{bigint::BigInt, fmt::Span};
use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::sync::Arc;

/// Alias for the primary representation of dimensionality.
pub type Quantity = BTreeMap<Dimension, i64>;

/// A newtype for a string dimension ID, so that we can implement traits for it.
#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[serde(transparent)]
pub struct Dimension {
    pub id: Arc<String>,
}

/// The basic representation of a number with a unit.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Number {
    pub value: Numeric,
    pub unit: Quantity,
}

impl Borrow<str> for Dimension {
    fn borrow(&self) -> &str {
        &**self.id
    }
}

impl fmt::Display for Dimension {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(fmt)
    }
}

impl Dimension {
    pub fn new(dim: &str) -> Dimension {
        Dimension {
            id: Arc::new(dim.to_owned()),
        }
    }
}

pub fn pow(left: &Numeric, exp: i32) -> Numeric {
    if exp < 0 {
        &Numeric::one() / &pow(left, -exp)
    } else {
        let left = match *left {
            Numeric::Rational(ref left) => left,
            Numeric::Float(f) => return Numeric::Float(f.powi(exp)),
        };
        let num = left.numer().pow(exp as u32);
        let den = left.denom().pow(exp as u32);
        Numeric::Rational(BigRat::ratio(&num, &den))
    }
}

/// Several stringified properties of a number which are useful for
/// displaying it to a user.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NumberParts {
    /// Raw numeric value, so precise number can be extracted from then NumberParts again.
    pub raw_value: Option<Number>,
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
    pub raw_unit: Option<Quantity>,
    /// Higher-level unit decomposition, if available.
    pub unit: Option<String>,
    /// The physical quantity associated with the unit, if available.
    pub quantity: Option<String>,
    /// The dimensionality of the unit.
    pub dimensions: Option<String>,
    /// Map of base units and their dimensions.
    pub raw_dimensions: Option<Quantity>,
}

pub struct NumberPartsFmt<'a> {
    number: &'a NumberParts,
    pattern: &'a str,
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
                'e' => {
                    if let Some(ex) = self.exact_value.as_ref() {
                        write!(out, "{}", ex).unwrap();
                    } else {
                        continue;
                    }
                }
                'a' => {
                    if let Some(ap) = self.approx_value.as_ref() {
                        write!(out, "{}", ap).unwrap();
                    } else {
                        continue;
                    }
                }
                'n' => match (self.exact_value.as_ref(), self.approx_value.as_ref()) {
                    (Some(ex), Some(ap)) => write!(out, "{}, approx. {}", ex, ap).unwrap(),
                    (Some(ex), None) => write!(out, "{}", ex).unwrap(),
                    (None, Some(ap)) => write!(out, "approx. {}", ap).unwrap(),
                    (None, None) => continue,
                },
                'u' => {
                    if let Some(unit) = self.raw_unit.as_ref() {
                        if unit.is_empty() {
                            continue;
                        }
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
                        if unit.is_empty() {
                            continue;
                        }
                        if let Some(f) = self.factor.as_ref() {
                            write!(out, "* {} ", f).unwrap();
                        }
                        if let Some(d) = self.divfactor.as_ref() {
                            write!(out, "| {} ", d).unwrap();
                        }
                        write!(out, "{}", unit).unwrap();
                    } else if let Some(dim) = self.dimensions.as_ref() {
                        if dim.is_empty() {
                            continue;
                        }
                        if let Some(f) = self.factor.as_ref() {
                            write!(out, "* {} ", f).unwrap();
                        }
                        if let Some(d) = self.divfactor.as_ref() {
                            write!(out, "| {} ", d).unwrap();
                        }
                        write!(out, "{}", dim).unwrap();
                    } else {
                        continue;
                    }
                }
                'q' => {
                    if let Some(q) = self.quantity.as_ref() {
                        write!(out, "{}", q).unwrap();
                    } else {
                        continue;
                    }
                }
                'w' => {
                    if let Some(q) = self.quantity.as_ref() {
                        write!(out, "({})", q).unwrap();
                    } else {
                        continue;
                    }
                }
                'd' => {
                    if let Some(dim) = self.dimensions.as_ref() {
                        if self.unit.is_none() || dim.is_empty() {
                            continue;
                        }
                        write!(out, "{}", dim).unwrap();
                    } else {
                        continue;
                    }
                }
                'D' => {
                    if let Some(dim) = self.dimensions.as_ref() {
                        if dim.is_empty() {
                            continue;
                        }
                        write!(out, "{}", dim).unwrap();
                    } else {
                        continue;
                    }
                }
                'p' => match (
                    self.quantity.as_ref(),
                    self.dimensions.as_ref().and_then(|x| {
                        if self.unit.is_some() && !x.is_empty() {
                            Some(x)
                        } else {
                            None
                        }
                    }),
                ) {
                    (Some(q), Some(d)) => write!(out, "({}; {})", q, d).unwrap(),
                    (Some(q), None) => write!(out, "({})", q).unwrap(),
                    (None, Some(d)) => write!(out, "({})", d).unwrap(),
                    (None, None) => continue,
                },
                ' ' if in_ws => continue,
                ' ' if !in_ws => {
                    in_ws = true;
                    write!(out, " ").unwrap();
                    continue;
                }
                x => write!(out, "{}", x).unwrap(),
            }
            in_ws = false;
        }

        ::std::str::from_utf8(&out[..]).unwrap().trim().to_owned()
    }

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
    pub fn token_format<'a>(&'a self, pattern: &'a str) -> NumberPartsFmt<'a> {
        NumberPartsFmt {
            number: self,
            pattern,
        }
    }
}

enum PatternToken<'a> {
    Exact,                // e
    Approx,               // a
    Numeric,              // n
    Unit,                 // u
    Quantity,             // q
    QuantityParen,        // w
    Dimensions,           // d
    DimensionsNonEmpty,   // D
    QuantityAndDimension, // p
    Whitespace,
    Passthrough(&'a str),
}

struct FnIter<F>(F);

impl<F, R> Iterator for FnIter<F>
where
    F: FnMut() -> Option<R>,
{
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        (self.0)()
    }
}

fn is_whitespace(ch: u8) -> bool {
    match ch {
        b' ' | b'\n' | b'\t' => true,
        _ => false,
    }
}

fn is_pattern_char(ch: u8) -> bool {
    match ch {
        b'e' | b'a' | b'n' | b'u' | b'q' | b'w' | b'd' | b'D' | b'p' => true,
        ch if is_whitespace(ch) => true,
        _ => false,
    }
}

fn parse_pattern<'a>(input: &'a str) -> impl Iterator<Item = PatternToken<'a>> {
    let mut i = 0;
    let orig = input;
    let input = input.as_bytes();
    FnIter(move || {
        if i >= input.len() {
            return None;
        }
        let tok = match input[i] {
            b'e' => PatternToken::Exact,
            b'a' => PatternToken::Approx,
            b'n' => PatternToken::Numeric,
            b'u' => PatternToken::Unit,
            b'q' => PatternToken::Quantity,
            b'w' => PatternToken::QuantityParen,
            b'd' => PatternToken::Dimensions,
            b'D' => PatternToken::DimensionsNonEmpty,
            b'p' => PatternToken::QuantityAndDimension,
            ch if is_whitespace(ch) => {
                while i <= input.len() && is_whitespace(input[i]) {
                    i += 1;
                }
                return Some(PatternToken::Whitespace);
            }
            _ => {
                let start = i;
                while i <= input.len() && is_pattern_char(input[i]) {
                    i += 1;
                }
                return Some(PatternToken::Passthrough(&orig[start..i]));
            }
        };
        i += 1;
        Some(tok)
    })
}

impl<'a> NumberPartsFmt<'a> {
    /// Converts the fmt to a span tree. Note that this is not an impl
    /// of TokenFmt, as it usually won't live long enough.
    pub fn to_spans(&self) -> Vec<Span<'a>> {
        let mut tokens: Vec<Span<'a>> = vec![];

        let parts = self.number;

        let mut last_was_ws = true;
        for tok in parse_pattern(self.pattern) {
            match tok {
                PatternToken::Exact => {
                    if let Some(ref value) = parts.exact_value {
                        tokens.push(Span::number(value));
                    }
                }
                PatternToken::Approx => {
                    if let Some(ref value) = parts.approx_value {
                        tokens.push(Span::number(value));
                    }
                }
                PatternToken::Numeric => {
                    match (parts.exact_value.as_ref(), parts.approx_value.as_ref()) {
                        (Some(ex), Some(ap)) => {
                            tokens.push(Span::number(ex));
                            tokens.push(Span::plain(", approx. "));
                            tokens.push(Span::number(ap));
                        }
                        (Some(ex), None) => {
                            tokens.push(Span::number(ex));
                        }
                        (None, Some(ap)) => {
                            tokens.push(Span::plain("approx. "));
                            tokens.push(Span::number(ap));
                        }
                        (None, None) => (),
                    }
                }
                PatternToken::Unit => {
                    if let Some(ref unit) = parts.raw_unit {
                        if unit.is_empty() {
                            continue;
                        }
                        let mut frac = vec![];

                        if let Some(ref f) = parts.factor {
                            tokens.push(Span::plain("* "));
                            tokens.push(Span::number(f));
                            tokens.push(Span::plain(" "));
                            last_was_ws = true;
                        }
                        let mut first = true;
                        for (dim, &exp) in unit {
                            if exp < 0 {
                                frac.push((dim, exp));
                            } else {
                                if first {
                                    first = false;
                                } else {
                                    tokens.push(Span::plain(" "));
                                }
                                tokens.push(Span::unit(&*dim.id));
                                if exp != 1 {
                                    tokens.push(Span::pow(format!("^{}", exp)));
                                }
                                last_was_ws = false;
                            }
                        }
                        if !frac.is_empty() || parts.divfactor.is_some() {
                            if last_was_ws {
                                tokens.push(Span::plain("/"));
                            } else {
                                tokens.push(Span::plain(" /"));
                            }
                            if let Some(ref d) = parts.divfactor {
                                tokens.push(Span::plain(" "));
                                tokens.push(Span::number(d));
                            }
                            for (dim, exp) in frac {
                                let exp = -exp;
                                tokens.push(Span::plain(" "));
                                tokens.push(Span::unit(&*dim.id));
                                if exp != 1 {
                                    tokens.push(Span::pow(format!("^{}", exp)));
                                }
                            }
                        }
                    } else if let Some(ref unit) = parts.unit {
                        if unit.is_empty() {
                            continue;
                        }
                        if let Some(ref f) = parts.factor {
                            tokens.push(Span::plain("* "));
                            tokens.push(Span::number(f));
                        }
                        if let Some(ref d) = parts.divfactor {
                            tokens.push(Span::plain("| "));
                            tokens.push(Span::number(d));
                        }
                        tokens.push(Span::unit(unit));
                    } else if let Some(ref dim) = parts.dimensions {
                        if dim.is_empty() {
                            continue;
                        }
                        if let Some(ref f) = parts.factor {
                            tokens.push(Span::plain("* "));
                            tokens.push(Span::number(f));
                        }
                        if let Some(ref d) = parts.divfactor {
                            tokens.push(Span::plain("| "));
                            tokens.push(Span::number(d));
                        }
                        tokens.push(Span::unit(dim));
                    }
                }
                PatternToken::Quantity => {
                    if let Some(ref quantity) = parts.quantity {
                        tokens.push(Span::quantity(quantity));
                    }
                }
                PatternToken::QuantityParen => {
                    if let Some(ref quantity) = parts.quantity {
                        tokens.push(Span::plain("("));
                        tokens.push(Span::quantity(quantity));
                        tokens.push(Span::plain(")"));
                    }
                }
                PatternToken::Dimensions => {
                    if let Some(ref dim) = parts.dimensions {
                        if parts.unit.is_some() || !dim.is_empty() {
                            tokens.push(Span::unit(dim));
                        }
                    }
                }
                PatternToken::DimensionsNonEmpty => {
                    if let Some(ref dim) = parts.dimensions {
                        if !dim.is_empty() {
                            tokens.push(Span::unit(dim));
                        }
                    }
                }
                PatternToken::QuantityAndDimension => match (
                    parts.quantity.as_ref(),
                    parts.dimensions.as_ref().and_then(|dim| {
                        if parts.unit.is_some() && !dim.is_empty() {
                            Some(dim)
                        } else {
                            None
                        }
                    }),
                ) {
                    (Some(quantity), Some(dim)) => {
                        tokens.push(Span::plain("("));
                        tokens.push(Span::quantity(quantity));
                        tokens.push(Span::plain("; "));
                        tokens.push(Span::unit(dim));
                        tokens.push(Span::plain(")"));
                    }
                    (Some(quantity), None) => {
                        tokens.push(Span::plain("("));
                        tokens.push(Span::quantity(quantity));
                        tokens.push(Span::plain(")"));
                    }
                    (None, Some(dim)) => {
                        tokens.push(Span::plain("("));
                        tokens.push(Span::unit(dim));
                        tokens.push(Span::plain(")"));
                    }
                    (None, None) => (),
                },
                PatternToken::Whitespace => {
                    tokens.push(Span::plain(" "));
                    last_was_ws = true;
                    continue;
                }
                PatternToken::Passthrough(text) => {
                    tokens.push(Span::plain(text));
                }
            }
            last_was_ws = false;
        }
        // Remove trailing whitespace
        loop {
            match tokens.last() {
                Some(Span::Content { text, .. }) if text.trim().is_empty() => (),
                _ => break,
            }
            tokens.pop();
        }
        tokens
    }
}

impl<'a> TokenFmt<'a> for NumberParts {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        self.token_format("n u w").to_spans()
    }
}

impl fmt::Display for NumberParts {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.format("n u w"))
    }
}

impl Number {
    pub fn one() -> Number {
        Number {
            value: Numeric::one(),
            unit: Quantity::new(),
        }
    }

    pub fn one_unit(unit: Dimension) -> Number {
        Number::new_unit(Numeric::one(), unit)
    }

    pub fn zero() -> Number {
        Number {
            value: Numeric::zero(),
            unit: Quantity::new(),
        }
    }

    /// Creates a dimensionless value.
    pub fn new(num: Numeric) -> Number {
        Number {
            value: num,
            unit: Quantity::new(),
        }
    }

    /// Creates a value with a single dimension.
    pub fn new_unit(num: Numeric, unit: Dimension) -> Number {
        let mut map = Quantity::new();
        map.insert(unit, 1);
        Number {
            value: num,
            unit: map,
        }
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
                .collect::<Quantity>(),
        }
    }

    /// Raises a value to a dimensionless integer power.
    pub fn powi(&self, exp: i32) -> Number {
        let unit = self
            .unit
            .iter()
            .map(|(k, &power)| (k.clone(), power * exp as i64))
            .collect::<Quantity>();
        Number {
            value: pow(&self.value, exp),
            unit,
        }
    }

    /// Computes the nth root of a value iff all of its units have
    /// powers divisible by n.
    pub fn root(&self, exp: i32) -> Result<Number, String> {
        if self.value < Numeric::zero() {
            return Err("Complex numbers are not implemented".to_string());
        }
        let mut res = Quantity::new();
        for (dim, &power) in &self.unit {
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
        if unit.len() == 1 {
            use std::collections::HashSet;
            let prefixes = [
                "milli", "micro", "nano", "pico", "femto", "atto", "zepto", "yocto", "kilo",
                "mega", "giga", "tera", "peta", "exa", "zetta", "yotta",
            ]
            .iter()
            .cloned()
            .collect::<HashSet<&'static str>>();
            let orig = unit.iter().next().unwrap();
            // kg special case
            let (val, orig) = if &**(orig.0).id == "kg" || &**(orig.0).id == "kilogram" {
                (
                    &self.value * &pow(&Numeric::from(1000), (*orig.1) as i32),
                    (Dimension::new("gram"), orig.1),
                )
            } else {
                (self.value.clone(), (orig.0.clone(), orig.1))
            };
            for &(ref p, ref v) in &context.prefixes {
                if !prefixes.contains(&**p) {
                    continue;
                }
                let abs = val.abs();
                if abs >= pow(&v.value, (*orig.1) as i32)
                    && abs < pow(&(&v.value * &Numeric::from(1000)), (*orig.1) as i32)
                {
                    let res = &val / &pow(&v.value, (*orig.1) as i32);
                    // tonne special case
                    let unit = if &**(orig.0).id == "gram" && p == "mega" {
                        "tonne".to_string()
                    } else {
                        format!("{}{}", p, orig.0)
                    };
                    let mut map = BTreeMap::new();
                    map.insert(Dimension::new(&*unit), *orig.1);
                    return Number {
                        value: res,
                        unit: map,
                    };
                }
            }
            let mut map = BTreeMap::new();
            map.insert(orig.0.clone(), *orig.1);
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
                let n = &(*e.0);
                if *e.1 == 1 {
                    Some((&*n.id).clone())
                } else {
                    Some(format!("{}^{}", n, e.1))
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

    pub fn unit_to_string(unit: &Quantity) -> String {
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

    fn pretty_unit(&self, context: &Context) -> Quantity {
        let pretty = crate::factorize::fast_decompose(self, &context.reverse);
        pretty
            .into_iter()
            .map(|(k, p)| {
                (
                    context
                        .canonicalizations
                        .get(&*k.id)
                        .map(|x| Dimension::new(x))
                        .unwrap_or(k),
                    p,
                )
            })
            .collect::<BTreeMap<_, _>>()
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
        let val = crate::btree_merge(&self.unit, &other.unit, |a, b| {
            if a + b != 0 {
                Some(a + b)
            } else {
                None
            }
        });
        Some(Number {
            value: &self.value * &other.value,
            unit: val,
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
