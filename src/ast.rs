// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::rc::Rc;
use std::fmt;
use num::Num;
use chrono_tz::Tz;

#[derive(Debug, Clone)]
pub enum SuffixOp {
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
    Suffix(SuffixOp, Box<Expr>),
    Of(String, Box<Expr>),
    Call(String, Vec<Expr>),
    Error(String),
}

#[derive(Debug, Clone)]
pub enum Conversion {
    None,
    Expr(Expr),
    DegC,
    DegF,
    DegRe,
    DegRo,
    DegDe,
    DegN,
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
            Conversion::DegC => write!(fmt, "°C"),
            Conversion::DegF => write!(fmt, "°F"),
            Conversion::DegRe => write!(fmt, "°Ré"),
            Conversion::DegRo => write!(fmt, "°Rø"),
            Conversion::DegDe => write!(fmt, "°De"),
            Conversion::DegN => write!(fmt, "°N"),
            Conversion::List(ref list) => {
                let list = list
                    .iter()
                    .map(|x| format!("{}", x))
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

impl fmt::Display for SuffixOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SuffixOp::Celsius => write!(fmt, "°C"),
            SuffixOp::Fahrenheit => write!(fmt, "°F"),
            SuffixOp::Newton => write!(fmt, "°N"),
            SuffixOp::Reaumur => write!(fmt, "°Ré"),
            SuffixOp::Romer => write!(fmt, "°Rø"),
            SuffixOp::Delisle => write!(fmt, "°De"),
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
                Expr::Call(ref name, ref args) => {
                    try!(write!(fmt, "{}(", name));
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
