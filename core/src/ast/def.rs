// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::*;
use crate::parsing::text_query::{parse_expr, Token, TokenIterator};
use serde_derive::{Deserialize, Serialize};
use std::convert::TryFrom;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub enum DateMatch {
    Day,
    Era,
    FullDay,
    FullHour12,
    FullHour24,
    FullYear,
    Hour12,
    Hour24,
    IsoWeek,
    IsoYear,
    Meridiem,
    Min,
    MonthName,
    MonthNum,
    Offset,
    Ordinal,
    Sec,
    WeekDay,
    Year,
    Today,
    Now,
    Relative,
}

impl DateMatch {
    pub(crate) fn from_str(input: &str) -> Option<DateMatch> {
        match input {
            "day" => Some(DateMatch::Day),
            "adbc" => Some(DateMatch::Era),
            "fullday" => Some(DateMatch::FullDay),
            "fullhour12" => Some(DateMatch::FullHour12),
            "fullhour24" => Some(DateMatch::FullHour24),
            "fullyear" => Some(DateMatch::FullYear),
            "hour12" => Some(DateMatch::Hour12),
            "hour24" => Some(DateMatch::Hour24),
            "isoweek" => Some(DateMatch::IsoWeek),
            "isoyear" => Some(DateMatch::IsoYear),
            "meridiem" => Some(DateMatch::Meridiem),
            "min" => Some(DateMatch::Min),
            "monthname" => Some(DateMatch::MonthName),
            "monthnum" => Some(DateMatch::MonthNum),
            "offset" => Some(DateMatch::Offset),
            "ordinal" => Some(DateMatch::Ordinal),
            "sec" => Some(DateMatch::Sec),
            "weekday" => Some(DateMatch::WeekDay),
            "year" => Some(DateMatch::Year),
            "today" => Some(DateMatch::Today),
            "now" => Some(DateMatch::Now),
            "relative" => Some(DateMatch::Relative),
            _ => None,
        }
    }

    fn name(self) -> &'static str {
        match self {
            DateMatch::Day => "day",
            DateMatch::Era => "adbc",
            DateMatch::FullDay => "fullday",
            DateMatch::FullHour12 => "fullhour12",
            DateMatch::FullHour24 => "fullhour24",
            DateMatch::FullYear => "fullyear",
            DateMatch::Hour12 => "hour12",
            DateMatch::Hour24 => "hour24",
            DateMatch::IsoWeek => "isoweek",
            DateMatch::IsoYear => "isoyear",
            DateMatch::Meridiem => "meridiem",
            DateMatch::Min => "min",
            DateMatch::MonthName => "monthname",
            DateMatch::MonthNum => "monthnum",
            DateMatch::Offset => "offset",
            DateMatch::Ordinal => "ordinal",
            DateMatch::Sec => "sec",
            DateMatch::WeekDay => "weekday",
            DateMatch::Year => "year",
            DateMatch::Today => "today",
            DateMatch::Now => "now",
            DateMatch::Relative => "relative",
        }
    }
}

impl fmt::Display for DateMatch {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.name())
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum DatePattern {
    Literal(String),
    Match(DateMatch),
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
    use crate::ast::{def::DateMatch, Expr};
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
        assert_eq!(
            format!("{}", DatePattern::Match(DateMatch::IsoYear)),
            "isoyear"
        );

        assert_eq!(
            format!(
                "{}",
                DatePattern::Optional(vec![DatePattern::Literal("a".to_owned())])
            ),
            "['a']"
        );
    }

    #[test]
    fn roundtrip() {
        let all_patterns = [
            DateMatch::Day,
            DateMatch::Era,
            DateMatch::FullDay,
            DateMatch::FullHour12,
            DateMatch::FullHour24,
            DateMatch::FullYear,
            DateMatch::Hour12,
            DateMatch::Hour24,
            DateMatch::IsoWeek,
            DateMatch::IsoYear,
            DateMatch::Meridiem,
            DateMatch::Min,
            DateMatch::MonthName,
            DateMatch::MonthNum,
            DateMatch::Offset,
            DateMatch::Ordinal,
            DateMatch::Sec,
            DateMatch::WeekDay,
            DateMatch::Year,
            DateMatch::Today,
            DateMatch::Now,
            DateMatch::Relative,
        ];

        for pattern in all_patterns {
            assert_eq!(DateMatch::from_str(pattern.name()), Some(pattern));
        }
    }
}
