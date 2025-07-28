// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Abstract syntax tree for rink's query language.

use crate::output::Digits;
use crate::types::Numeric;
use serde_derive::Serialize;
use std::fmt;

mod def;
mod expr;
mod query;
#[cfg(test)]
mod test;

pub use def::{DatePattern, Def, DefEntry, Defs, ExprString, Property};
pub use expr::{Expr, Precedence};
pub use query::{Conversion, Query};

#[derive(Debug, Clone, Serialize, Copy, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum Degree {
    Celsius,
    Fahrenheit,
    Reaumur,
    Romer,
    Delisle,
    Newton,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum DateToken {
    Literal(String),
    Number(String, Option<String>),
    Colon,
    Dash,
    Space,
    Plus,
    Error(String),
}

#[derive(Debug, Clone, Serialize, Copy, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum BinOpType {
    Add,
    Sub,
    Frac,
    Pow,
    Equals,
    ShiftL,
    ShiftR,
    Mod,
    And,
    Or,
    Xor,
}

impl BinOpType {
    pub fn symbol(self) -> &'static str {
        match self {
            BinOpType::Add => " + ",
            BinOpType::Sub => " - ",
            BinOpType::Frac => " / ",
            BinOpType::Pow => "^",
            BinOpType::Equals => " = ",
            BinOpType::ShiftL => " << ",
            BinOpType::ShiftR => " >> ",
            BinOpType::Mod => " mod ",
            BinOpType::And => " and ",
            BinOpType::Or => " or ",
            BinOpType::Xor => " xor ",
        }
    }
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct BinOpExpr {
    pub op: BinOpType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Serialize, Copy, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum UnaryOpType {
    Negative,
    Positive,
    Degree(Degree),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct UnaryOpExpr {
    pub op: UnaryOpType,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum Function {
    Sqrt,
    Exp,
    Ln,
    Log2,
    Log10,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Asinh,
    Acosh,
    Atanh,
    Log,
    Hypot,
    Atan2,
    Fac,
}

impl Function {
    pub fn name(&self) -> &str {
        match *self {
            Function::Sqrt => "sqrt",
            Function::Exp => "exp",
            Function::Ln => "ln",
            Function::Log2 => "log2",
            Function::Log10 => "log10",
            Function::Sin => "sin",
            Function::Cos => "cos",
            Function::Tan => "tan",
            Function::Asin => "asin",
            Function::Acos => "acos",
            Function::Atan => "atan",
            Function::Sinh => "sinh",
            Function::Cosh => "cosh",
            Function::Tanh => "tanh",
            Function::Asinh => "asinh",
            Function::Acosh => "acosh",
            Function::Atanh => "atanh",
            Function::Log => "log",
            Function::Hypot => "hypot",
            Function::Atan2 => "atan2",
            Function::Fac => "fac",
        }
    }

    pub fn from_name(s: &str) -> Option<Self> {
        let func = match s {
            "sqrt" => Function::Sqrt,
            "exp" => Function::Exp,
            "ln" => Function::Ln,
            "log2" => Function::Log2,
            "log10" => Function::Log10,
            "sin" => Function::Sin,
            "cos" => Function::Cos,
            "tan" => Function::Tan,
            "asin" => Function::Asin,
            "acos" => Function::Acos,
            "atan" => Function::Atan,
            "sinh" => Function::Sinh,
            "cosh" => Function::Cosh,
            "tanh" => Function::Tanh,
            "asinh" => Function::Asinh,
            "acosh" => Function::Acosh,
            "atanh" => Function::Atanh,
            "log" => Function::Log,
            "hypot" => Function::Hypot,
            "atan2" => Function::Atan2,
            "fac" => Function::Fac,
            _ => return None,
        };
        Some(func)
    }
}

impl fmt::Display for Degree {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Degree::Celsius => write!(fmt, "°C"),
            Degree::Fahrenheit => write!(fmt, "°F"),
            Degree::Newton => write!(fmt, "°N"),
            Degree::Reaumur => write!(fmt, "°Ré"),
            Degree::Romer => write!(fmt, "°Rø"),
            Degree::Delisle => write!(fmt, "°De"),
        }
    }
}

impl Degree {
    pub fn name_base_scale(&self) -> (&str, &str, &str) {
        match *self {
            Degree::Celsius => ("C", "zerocelsius", "kelvin"),
            Degree::Fahrenheit => ("F", "zerofahrenheit", "degrankine"),
            Degree::Reaumur => ("Ré", "zerocelsius", "reaumur_absolute"),
            Degree::Romer => ("Rø", "zeroromer", "romer_absolute"),
            Degree::Delisle => ("De", "zerodelisle", "delisle_absolute"),
            Degree::Newton => ("N", "zerocelsius", "newton_absolute"),
        }
    }
}

impl fmt::Display for DateToken {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DateToken::Literal(ref l) => write!(fmt, "{}", l),
            DateToken::Number(ref i, None) => write!(fmt, "{}", i),
            DateToken::Number(ref i, Some(ref f)) => write!(fmt, "{}.{}", i, f),
            DateToken::Colon => write!(fmt, ":"),
            DateToken::Dash => write!(fmt, "-"),
            DateToken::Space => write!(fmt, " "),
            DateToken::Plus => write!(fmt, "+"),
            DateToken::Error(ref e) => write!(fmt, "<{}>", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::DateToken;

    use super::Function;

    const FUNCTIONS: &'static [Function] = &[
        Function::Sqrt,
        Function::Exp,
        Function::Ln,
        Function::Log2,
        Function::Log10,
        Function::Sin,
        Function::Cos,
        Function::Tan,
        Function::Asin,
        Function::Acos,
        Function::Atan,
        Function::Sinh,
        Function::Cosh,
        Function::Tanh,
        Function::Asinh,
        Function::Acosh,
        Function::Atanh,
        Function::Log,
        Function::Hypot,
        Function::Atan2,
        Function::Fac,
    ];

    #[test]
    fn roundtrip_function_names() {
        for &func in FUNCTIONS {
            assert_eq!(Function::from_name(func.name()), Some(func));
        }
    }

    #[test]
    fn date_tokens_display() {
        assert_eq!(DateToken::Colon.to_string(), ":");
        assert_eq!(DateToken::Dash.to_string(), "-");
        assert_eq!(DateToken::Space.to_string(), " ");
        assert_eq!(DateToken::Plus.to_string(), "+");
        assert_eq!(DateToken::Literal("a".to_owned()).to_string(), "a");
        assert_eq!(DateToken::Number("123".to_owned(), None).to_string(), "123");
        assert_eq!(
            DateToken::Number("123".to_owned(), Some("456".to_owned())).to_string(),
            "123.456"
        );
        assert_eq!(DateToken::Error("test".to_owned()).to_string(), "<test>");
    }
}
