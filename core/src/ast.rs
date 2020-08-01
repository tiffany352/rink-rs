// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::numeric::Numeric;
use chrono_tz::Tz;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Degree {
    Celsius,
    Fahrenheit,
    Reaumur,
    Romer,
    Delisle,
    Newton,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DateToken {
    Literal(String),
    Number(String, Option<String>),
    Colon,
    Dash,
    Space,
    Plus,
    Error(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, Copy, Eq, PartialEq)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinOp {
    pub op: BinOpType,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
pub enum Expr {
    Unit(String),
    Quote(String),
    #[serde(skip_deserializing)]
    Const(Numeric),
    Date(Vec<DateToken>),
    BinOp(BinOp),
    Mul(Vec<Expr>),
    Neg(Box<Expr>),
    Plus(Box<Expr>),
    Suffix {
        suffix: Degree,
        expr: Box<Expr>,
    },
    Of {
        property: String,
        expr: Box<Expr>,
    },
    Call {
        func: Function,
        args: Vec<Expr>,
    },
    Error(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Conversion {
    None,
    Expr(Expr),
    Degree(Degree),
    List(Vec<String>),
    Offset(i64),
    #[serde(skip)]
    Timezone(Tz),
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Digits {
    Default,
    FullInt,
    Digits(u64),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub enum Query {
    Expr(Expr),
    Convert(Expr, Conversion, Option<u8>, Digits),
    Factorize(Expr),
    UnitsFor(Expr),
    Search(String),
    Error(String),
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DatePattern {
    Literal(String),
    Match(String),
    Optional(Vec<DatePattern>),
    Dash,
    Colon,
    Space,
}

#[derive(Debug)]
pub struct Property {
    pub name: String,
    pub input: Expr,
    pub input_name: String,
    pub output: Expr,
    pub output_name: String,
    pub doc: Option<String>,
}

#[derive(Debug)]
pub enum Def {
    Dimension,
    Canonicalization(String),
    Prefix(Expr),
    SPrefix(Expr),
    Unit(Expr),
    Quantity(Expr),
    Substance {
        symbol: Option<String>,
        properties: Vec<Property>,
    },
    Category(String),
    Error(String),
}

#[derive(Debug)]
pub struct DefEntry {
    pub name: String,
    pub def: Rc<Def>,
    pub doc: Option<String>,
    pub category: Option<String>,
}

#[derive(Debug)]
pub struct Defs {
    pub defs: Vec<DefEntry>,
}

impl fmt::Display for Conversion {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Conversion::None => write!(fmt, "nothing"),
            Conversion::Expr(ref expr) => write!(fmt, "{}", expr),
            Conversion::Degree(ref deg) => write!(fmt, "{}", deg),
            Conversion::List(ref list) => {
                let list = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(fmt, "{}", list)
            }
            Conversion::Offset(off) => write!(fmt, "{:02}:{:02}", off / 3600, (off / 60) % 60),
            Conversion::Timezone(ref tz) => write!(fmt, "{:?}", tz),
        }
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

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub enum Precedence {
    Term,
    Plus,
    Pow,
    Mul,
    Div,
    Add,
    Equals,
}

impl Precedence {
    pub fn from(binop_type: BinOpType) -> Precedence {
        match binop_type {
            BinOpType::Add => Precedence::Add,
            BinOpType::Sub => Precedence::Add,
            BinOpType::Pow => Precedence::Pow,
            BinOpType::Frac => Precedence::Div,
            BinOpType::Equals => Precedence::Equals,
        }
    }

    pub fn next(binop_type: BinOpType) -> Precedence {
        match binop_type {
            BinOpType::Add => Precedence::Div,
            BinOpType::Sub => Precedence::Div,
            BinOpType::Pow => Precedence::Term,
            BinOpType::Frac => Precedence::Mul,
            BinOpType::Equals => Precedence::Add,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn recurse(expr: &Expr, fmt: &mut fmt::Formatter<'_>, prec: Precedence) -> fmt::Result {
            match *expr {
                Expr::Unit(ref name) => write!(fmt, "{}", name),
                Expr::Quote(ref name) => write!(fmt, "'{}'", name),
                Expr::Const(ref num) => {
                    let (_exact, val) = crate::number::to_string(num, 10, Digits::Default);
                    write!(fmt, "{}", val)
                }
                Expr::Date(ref _date) => write!(fmt, "NYI: date expr Display"),
                Expr::BinOp(ref binop) => {
                    let op_prec = Precedence::from(binop.op);
                    let succ = Precedence::next(binop.op);
                    if prec < op_prec {
                        write!(fmt, "(")?;
                    }
                    recurse(&binop.left, fmt, succ)?;
                    write!(fmt, "{}", binop.op.symbol())?;
                    recurse(&binop.right, fmt, op_prec)?;
                    if prec < op_prec {
                        write!(fmt, ")")?;
                    }
                    Ok(())
                }
                Expr::Mul(ref exprs) => {
                    if prec < Precedence::Mul {
                        write!(fmt, "(")?;
                    }
                    if let Some(first) = exprs.first() {
                        recurse(first, fmt, Precedence::Pow)?;
                    }
                    for expr in exprs.iter().skip(1) {
                        write!(fmt, " ")?;
                        recurse(expr, fmt, Precedence::Pow)?;
                    }
                    if prec < Precedence::Mul {
                        write!(fmt, ")")?;
                    }
                    Ok(())
                }
                Expr::Call { ref func, ref args } => {
                    write!(fmt, "{}(", func.name())?;
                    if let Some(first) = args.first() {
                        recurse(first, fmt, Precedence::Equals)?;
                    }
                    for arg in args.iter().skip(1) {
                        write!(fmt, ", ")?;
                        recurse(arg, fmt, Precedence::Equals)?;
                    }
                    write!(fmt, ")")
                }
                Expr::Plus(ref expr) => {
                    write!(fmt, "+")?;
                    recurse(expr, fmt, Precedence::Plus)
                }
                Expr::Neg(ref expr) => {
                    write!(fmt, "-")?;
                    recurse(expr, fmt, Precedence::Plus)
                }
                Expr::Suffix {
                    ref suffix,
                    ref expr,
                } => {
                    if prec < Precedence::Mul {
                        write!(fmt, "(")?;
                    }
                    recurse(expr, fmt, Precedence::Mul)?;
                    write!(fmt, " {}", suffix)?;
                    if prec < Precedence::Mul {
                        write!(fmt, ")")?;
                    }
                    Ok(())
                }
                Expr::Of {
                    ref property,
                    ref expr,
                } => {
                    if prec < Precedence::Add {
                        write!(fmt, "(")?;
                    }
                    write!(fmt, "{} of ", property)?;
                    recurse(expr, fmt, Precedence::Div)?;
                    if prec < Precedence::Add {
                        write!(fmt, ")")?;
                    }
                    Ok(())
                }
                Expr::Error(ref err) => write!(fmt, "<error: {}>", err),
            }
        }

        recurse(self, fmt, Precedence::Equals)
    }
}

impl fmt::Display for DatePattern {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DatePattern::Literal(ref l) => write!(fmt, "'{}'", l),
            DatePattern::Match(ref n) => write!(fmt, "{}", n),
            DatePattern::Optional(ref pats) => {
                write!(fmt, "[")?;
                for p in pats {
                    p.fmt(fmt)?;
                }
                write!(fmt, "]")
            }
            DatePattern::Dash => write!(fmt, "-"),
            DatePattern::Colon => write!(fmt, ":"),
            DatePattern::Space => write!(fmt, " "),
        }
    }
}

pub fn show_datepattern(pat: &[DatePattern]) -> String {
    use std::io::Write;

    let mut buf = vec![];
    for p in pat {
        write!(buf, "{}", p).unwrap();
    }
    String::from_utf8(buf).unwrap()
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

impl Expr {
    pub fn new_call(func: Function, args: Vec<Expr>) -> Expr {
        Expr::Call { func, args }
    }

    pub fn new_bin(op: BinOpType, numer: Expr, denom: Expr) -> Expr {
        let left = Box::new(numer);
        let right = Box::new(denom);
        Expr::BinOp(BinOp { op, left, right })
    }

    pub fn new_add(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Add, numer, denom)
    }

    pub fn new_sub(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Sub, numer, denom)
    }

    pub fn new_frac(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Frac, numer, denom)
    }

    pub fn new_pow(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Pow, numer, denom)
    }

    pub fn new_equals(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Equals, numer, denom)
    }

    pub fn new_of(property: &str, expr: Expr) -> Expr {
        let property = property.to_owned();
        let expr = Box::new(expr);
        Expr::Of { property, expr }
    }

    pub fn new_suffix(suffix: Degree, expr: Expr) -> Expr {
        let expr = Box::new(expr);
        Expr::Suffix { suffix, expr }
    }
}

#[cfg(test)]
mod test {
    use super::Expr::{self, *};
    use super::Function;

    fn check<T: ::std::fmt::Display>(e: T, expected: &str) {
        assert_eq!(e.to_string(), expected);
    }

    impl From<i64> for Expr {
        fn from(x: i64) -> Self {
            Const(x.into())
        }
    }

    #[test]
    fn test_display_call() {
        check(
            Call {
                func: Function::Sin,
                args: vec![],
            },
            "sin()",
        );
        check(
            Call {
                func: Function::Sin,
                args: vec![1.into()],
            },
            "sin(1)",
        );
        check(
            Call {
                func: Function::Sin,
                args: vec![1.into(), 2.into()],
            },
            "sin(1, 2)",
        );
        check(
            Call {
                func: Function::Sin,
                args: vec![1.into(), 2.into(), 3.into()],
            },
            "sin(1, 2, 3)",
        );
    }
}
