// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::*;
use serde_derive::Serialize;

#[derive(Debug, Clone, Serialize)]
pub enum Conversion {
    None,
    Expr(Expr),
    Degree(Degree),
    List(Vec<String>),
    Offset(i64),
    #[serde(skip)]
    Timezone(Tz),
}

#[derive(Debug, Clone, Serialize)]
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

#[cfg(test)]
mod tests {
    use super::Conversion;
    use crate::ast::{Degree, Expr};
    use chrono_tz::Tz;

    #[test]
    fn conversion_display() {
        assert_eq!(Conversion::None.to_string(), "nothing");
        assert_eq!(
            Conversion::Expr(Expr::new_unit("a".to_owned())).to_string(),
            "a"
        );
        assert_eq!(Conversion::Degree(Degree::Celsius).to_string(), "°C");
        assert_eq!(
            Conversion::List(vec!["a".to_owned(), "b".to_owned()]).to_string(),
            "a, b"
        );
        assert_eq!(Conversion::Offset(3600 * 7).to_string(), "07:00");
        assert_eq!(
            Conversion::Timezone(Tz::US__Pacific).to_string(),
            "US/Pacific"
        );
    }
}
