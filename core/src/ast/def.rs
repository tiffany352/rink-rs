use super::*;
use crate::text_query::{parse_expr, Token, TokenIterator};
use std::convert::TryFrom;
use std::ops::Deref;
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(into = "String", try_from = "String")]
pub struct ExprString(pub Expr);

impl From<ExprString> for String {
    fn from(value: ExprString) -> String {
        format!("{}", value.0)
    }
}

impl TryFrom<String> for ExprString {
    type Error = String;

    fn try_from(input: String) -> Result<Self, Self::Error> {
        let mut iter = TokenIterator::new(&input).peekable();
        let expr = parse_expr(&mut iter);
        if let Some(Token::Eof) = iter.next() {
            Ok(ExprString(expr))
        } else {
            Err("Expected EOF".to_owned())
        }
    }
}

impl Deref for ExprString {
    type Target = Expr;

    fn deref(&self) -> &Expr {
        &self.0
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Property {
    pub name: String,
    pub input: ExprString,
    pub input_name: String,
    pub output: ExprString,
    pub output_name: String,
    pub doc: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
pub enum Def {
    Dimension,
    Canonicalization {
        of: String,
    },
    Prefix {
        expr: ExprString,
    },
    #[serde(rename = "sprefix")]
    SPrefix {
        expr: ExprString,
    },
    Unit {
        expr: ExprString,
    },
    Quantity {
        expr: ExprString,
    },
    Substance {
        symbol: Option<String>,
        properties: Vec<Property>,
    },
    Category {
        display_name: String,
    },
    Error {
        message: String,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DefEntry {
    pub name: String,
    #[serde(flatten)]
    pub def: Rc<Def>,
    pub doc: Option<String>,
    pub category: Option<String>,
}

impl DefEntry {
    pub fn new(name: &str, doc: Option<&str>, category: Option<&str>, def: Def) -> DefEntry {
        DefEntry {
            name: name.into(),
            doc: doc.map(Into::into),
            category: category.map(Into::into),
            def: Rc::new(def),
        }
    }

    pub fn new_unit(name: &str, doc: Option<&str>, category: Option<&str>, expr: Expr) -> DefEntry {
        Self::new(
            name,
            doc,
            category,
            Def::Unit {
                expr: ExprString(expr),
            },
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(transparent)]
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
