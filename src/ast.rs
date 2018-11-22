// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::rc::Rc;
use std::fmt;
use num::Num;
use chrono_tz::Tz;

#[derive(Debug, Clone)]
pub enum Degree {
    Celsius,
    Fahrenheit,
    Reaumur,
    Romer,
    Delisle,
    Newton,
}

#[derive(Debug, Clone)]
pub enum DateToken {
    Literal(String),
    Number(String, Option<String>),
    Colon,
    Dash,
    Space,
    Plus,
    Error(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit(String),
    Quote(String),
    Const(Num),
    Date(Vec<DateToken>),
    Frac(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Plus(Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
    Suffix(Degree, Box<Expr>),
    Of(String, Box<Expr>),
    Call(Function, Vec<Expr>),
    Error(String),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Conversion {
    None,
    Expr(Expr),
    Degree(Degree),
    List(Vec<String>),
    Offset(i64),
    Timezone(Tz),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Digits {
    Default,
    FullInt,
    Digits(u64),
}

#[derive(Debug, Clone)]
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
        properties: Vec<Property>
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
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
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
            },
            Conversion::Offset(off) =>
                write!(fmt, "{:02}:{:02}", off / 3600, (off / 60) % 60),
            Conversion::Timezone(ref tz) =>
                write!(fmt, "{:?}", tz),
        }
    }
}

impl fmt::Display for Degree {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
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

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        #[derive(PartialOrd, Ord, PartialEq, Eq)]
        enum Prec {
            Term, Plus, Pow, Mul, Div, Add, Equals
        }

        fn recurse(expr: &Expr, fmt: &mut fmt::Formatter, prec: Prec) -> fmt::Result {
            macro_rules! binop {
                ($left:expr, $right:expr, $prec:expr, $succ:expr, $sym:expr) => {{
                    if prec < $prec {
                        try!(write!(fmt, "("));
                    }
                    try!(recurse($left, fmt, $succ));
                    try!(write!(fmt, $sym));
                    try!(recurse($right, fmt, $prec));
                    if prec < $prec {
                        try!(write!(fmt, ")"));
                    }
                    Ok(())
                }}
            }
            match *expr {
                Expr::Unit(ref name) => write!(fmt, "{}", name),
                Expr::Quote(ref name) => write!(fmt, "'{}'", name),
                Expr::Const(ref num) => {
                    let (_exact, val) = ::number::to_string(num, 10, Digits::Default);
                    write!(fmt, "{}", val)
                },
                Expr::Date(ref _date) => write!(fmt, "NYI: date expr Display"),
                Expr::Mul(ref exprs) => {
                    if prec < Prec::Mul {
                        try!(write!(fmt, "("));
                    }
                    if let Some(first) = exprs.first() {
                        try!(recurse(first, fmt, Prec::Pow));
                    }
                    for expr in exprs.iter().skip(1) {
                        try!(write!(fmt, " "));
                        try!(recurse(expr, fmt, Prec::Pow));
                    }
                    if prec < Prec::Mul {
                        try!(write!(fmt, ")"));
                    }
                    Ok(())
                },
                Expr::Call(ref func, ref args) => {
                    try!(write!(fmt, "{}(", func.name()));
                    if let Some(first) = args.first() {
                        try!(recurse(first, fmt, Prec::Equals));
                    }
                    for arg in args.iter().skip(1) {
                        try!(write!(fmt, ", "));
                        try!(recurse(arg, fmt, Prec::Equals));
                    }
                    write!(fmt, ")")
                },
                Expr::Pow(ref left, ref right) => binop!(left, right, Prec::Pow, Prec::Term, "^"),
                Expr::Frac(ref left, ref right) => binop!(left, right, Prec::Div, Prec::Mul, " / "),
                Expr::Add(ref left, ref right) => binop!(left, right, Prec::Add, Prec::Div, " + "),
                Expr::Sub(ref left, ref right) => binop!(left, right, Prec::Add, Prec::Div, " - "),
                Expr::Plus(ref expr) => {
                    try!(write!(fmt, "+"));
                    recurse(expr, fmt, Prec::Plus)
                },
                Expr::Neg(ref expr) => {
                    try!(write!(fmt, "-"));
                    recurse(expr, fmt, Prec::Plus)
                },
                Expr::Equals(ref left, ref right) => binop!(left, right, Prec::Equals, Prec::Add, " = "),
                Expr::Suffix(ref op, ref expr) => {
                    if prec < Prec::Mul {
                        try!(write!(fmt, "("));
                    }
                    try!(recurse(expr, fmt, Prec::Mul));
                    try!(write!(fmt, " {}", op));
                    if prec < Prec::Mul {
                        try!(write!(fmt, ")"));
                    }
                    Ok(())
                },
                Expr::Of(ref field, ref expr) => {
                    if prec < Prec::Add {
                        try!(write!(fmt, "("));
                    }
                    try!(write!(fmt, "{} of ", field));
                    try!(recurse(expr, fmt, Prec::Div));
                    if prec < Prec::Add {
                        try!(write!(fmt, ")"));
                    }
                    Ok(())
                },
                Expr::Error(ref err) => write!(fmt, "<error: {}>", err)
            }
        }

        recurse(self, fmt, Prec::Equals)
    }
}

impl fmt::Display for DatePattern {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DatePattern::Literal(ref l) => write!(fmt, "'{}'", l),
            DatePattern::Match(ref n) => write!(fmt, "{}", n),
            DatePattern::Optional(ref pats) => {
                try!(write!(fmt, "["));
                for p in pats {
                    try!(p.fmt(fmt));
                }
                write!(fmt, "]")
            },
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
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
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
        check(Call(Function::Sin, vec![]), "sin()");
        check(Call(Function::Sin, vec![1.into()]), "sin(1)");
        check(Call(Function::Sin, vec![1.into(), 2.into()]), "sin(1, 2)");
        check(Call(Function::Sin, vec![1.into(), 2.into(), 3.into()]), "sin(1, 2, 3)");
    }
}
