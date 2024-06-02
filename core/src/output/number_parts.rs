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
        let spans = self.token_format(pat).to_spans();
        let mut out = String::new();
        crate::output::fmt::write_spans_string(&mut out, &spans);
        out
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

impl<'a> NumberPartsFmt<'a> {
    /// Converts the fmt to a span tree. Note that this is not an impl
    /// of TokenFmt, as it usually won't live long enough.
    pub fn to_spans(&self) -> Vec<Span<'a>> {
        let mut tokens: Vec<Span<'a>> = vec![];

        let parts = self.number;

        for ch in self.pattern.chars() {
            match ch {
                'e' => {
                    if let Some(ref value) = parts.exact_value {
                        tokens.push(Span::number(value));
                    }
                }
                'a' => {
                    if let Some(ref value) = parts.approx_value {
                        tokens.push(Span::number(value));
                    }
                }
                'n' => match (parts.exact_value.as_ref(), parts.approx_value.as_ref()) {
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
                },
                'u' => {
                    if let Some(ref unit) = parts.raw_unit {
                        if unit.is_dimensionless() {
                            continue;
                        }
                        let mut frac = vec![];

                        if let Some(ref f) = parts.factor {
                            tokens.push(Span::plain("* "));
                            tokens.push(Span::number(f));
                            tokens.push(Span::plain(" "));
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
                            }
                        }
                        if !frac.is_empty() || parts.divfactor.is_some() {
                            if tokens.last().map(|s| s.is_ws()) != Some(true) {
                                tokens.push(Span::plain(" /"));
                            } else {
                                tokens.push(Span::plain("/"));
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
                'q' => {
                    if let Some(ref quantity) = parts.quantity {
                        tokens.push(Span::quantity(quantity));
                    }
                }
                'w' => {
                    if let Some(ref quantity) = parts.quantity {
                        tokens.push(Span::plain("("));
                        tokens.push(Span::quantity(quantity));
                        tokens.push(Span::plain(")"));
                    }
                }
                'd' => {
                    if let Some(ref dim) = parts.dimensions {
                        if parts.unit.is_some() || !dim.is_empty() {
                            tokens.push(Span::unit(dim));
                        }
                    }
                }
                'D' => {
                    if let Some(ref dim) = parts.dimensions {
                        if !dim.is_empty() {
                            tokens.push(Span::unit(dim));
                        }
                    }
                }
                'p' => match (
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
                ' ' => {
                    if tokens.last().map(|s| s.is_ws()) != Some(true) {
                        tokens.push(Span::plain(" "));
                    }
                }
                _ => {
                    // Very inefficient, but this functionality is not used anywhere in rink_core.
                    tokens.push(Span::plain(String::from(ch)))
                }
            }
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
