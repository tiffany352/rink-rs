// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::*;
use crate::parsing::text_query::{parse_expr, Token, TokenIterator};
use serde_derive::{Deserialize, Serialize};
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
    BaseUnit {
        #[serde(rename = "longName")]
        long_name: Option<String>,
    },
    Prefix {
        expr: ExprString,
        #[serde(rename = "isLong")]
        is_long: bool,
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
        #[serde(rename = "displayName")]
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

#[cfg(test)]
mod tests {
    use super::{DatePattern, ExprString};
    use crate::ast::Expr;
    use std::convert::TryFrom;

    #[test]
    fn test_try_from() {
        assert_eq!(
            ExprString::try_from("abc".to_owned()),
            Ok(ExprString(Expr::new_unit("abc".to_owned())))
        );
        assert_eq!(
            ExprString::try_from("".to_owned()),
            Ok(ExprString(Expr::new_error(
                "Expected term, got eof".to_owned()
            )))
        );
        assert_eq!(
            ExprString::try_from("a -> ->".to_owned()),
            Err("Expected EOF".to_owned())
        )
    }

    #[test]
    fn date_pattern_fmt() {
        assert_eq!(format!("{}", DatePattern::Dash), "-");
        assert_eq!(format!("{}", DatePattern::Colon), ":");
        assert_eq!(format!("{}", DatePattern::Space), " ");
        assert_eq!(format!("{}", DatePattern::Literal("a".to_owned())), "'a'");
        assert_eq!(format!("{}", DatePattern::Match("a".to_owned())), "a");

        assert_eq!(
            format!(
                "{}",
                DatePattern::Optional(vec![DatePattern::Literal("a".to_owned())])
            ),
            "['a']"
        );
    }
}
