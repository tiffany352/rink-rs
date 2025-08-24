// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use jiff::tz::{Offset, TimeZone};
use rink_core::ast::{
    Conversion, DateMatch, DatePattern, DateToken, Degree, Expr, ExprString, Function,
};
use rink_core::parsing::text_query::{parse_expr, TokenIterator};
use std::convert::TryFrom;

fn parse_then_pretty(input: &str) -> String {
    let mut iter = TokenIterator::new(&input).peekable();
    let expr = parse_expr(&mut iter);
    expr.to_string()
}

#[test]
fn expr_display() {
    assert_eq!(parse_then_pretty("meter"), "meter");
    assert_eq!(parse_then_pretty("'hello world'"), "'hello world'");
    assert_eq!(parse_then_pretty("234234"), "234234");
    assert_eq!(parse_then_pretty("1 + 2"), "1 + 2");
    assert_eq!(parse_then_pretty("speed of light"), "speed of light");
    assert_eq!(parse_then_pretty("1 + 2 * 3"), "1 + 2 3");
    assert_eq!(parse_then_pretty("(1 + 2) * 3"), "(1 + 2) 3");
    assert_eq!(parse_then_pretty("a = 2"), "a = 2");
}

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
        Conversion::Timezone(TimeZone::get("US/Pacific").unwrap()).to_string(),
        "US/Pacific"
    );
    assert_eq!(
        Conversion::Timezone(TimeZone::fixed(Offset::from_seconds(3600 * 7).unwrap())).to_string(),
        "+07"
    );
}

#[test]
fn test_display_call() {
    assert_eq!(
        Expr::Call {
            func: Function::Sin,
            args: vec![],
        }
        .to_string(),
        "sin()",
    );
    assert_eq!(
        Expr::Call {
            func: Function::Sin,
            args: vec![1.into()],
        }
        .to_string(),
        "sin(1)",
    );
    assert_eq!(
        Expr::Call {
            func: Function::Sin,
            args: vec![1.into(), 2.into()],
        }
        .to_string(),
        "sin(1, 2)",
    );
    assert_eq!(
        Expr::Call {
            func: Function::Sin,
            args: vec![1.into(), 2.into(), 3.into()],
        }
        .to_string(),
        "sin(1, 2, 3)",
    );
}
