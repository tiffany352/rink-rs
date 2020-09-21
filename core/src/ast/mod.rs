// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::numeric::{Digits, Numeric};
use chrono_tz::Tz;
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

#[derive(Debug, Clone, Serialize)]
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
}

impl BinOpType {
    pub fn symbol(self) -> &'static str {
        match self {
            BinOpType::Add => " + ",
            BinOpType::Sub => " - ",
            BinOpType::Frac => " / ",
            BinOpType::Pow => "^",
            BinOpType::Equals => " = ",
        }
    }
}

#[derive(Debug, Clone, Serialize)]
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

impl UnaryOpType {
    pub fn is_prefix(self) -> bool {
        match self {
            UnaryOpType::Degree(_) => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct UnaryOpExpr {
    pub op: UnaryOpType,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, Serialize)]
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
