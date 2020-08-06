use super::*;
use std::rc::Rc;

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

impl DatePattern {
    pub fn show(pat: &[DatePattern]) -> String {
        use std::io::Write;

        let mut buf = vec![];
        for p in pat {
            write!(buf, "{}", p).unwrap();
        }
        String::from_utf8(buf).unwrap()
    }
}
