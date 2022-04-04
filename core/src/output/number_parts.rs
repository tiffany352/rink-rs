// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::fmt::{Span, TokenFmt};
use crate::types::{Dimensionality, Number};
use serde_derive::{Deserialize, Serialize};
use std::fmt;

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
    pub raw_unit: Option<Dimensionality>,
    /// Higher-level unit decomposition, if available.
    pub unit: Option<String>,
    /// The physical quantity associated with the unit, if available.
    pub quantity: Option<String>,
    /// The dimensionality of the unit.
    pub dimensions: Option<String>,
    /// Map of base units and their dimensions.
    pub raw_dimensions: Option<Dimensionality>,
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
                        for (dim, &exp) in unit.iter() {
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
                        for (dim, &exp) in unit.iter() {
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
