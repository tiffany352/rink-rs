use std::rc::Rc;

use rink_core::ast::{Def, DefEntry, Defs, Expr, UnaryOpExpr, UnaryOpType};
use rink_core::loader::gnu_units::{parse, parse_expr, tokens, Token, TokenIterator};
use rink_core::types::{BigRat, Numeric};

fn do_parse_expr(s: &str) -> Expr {
    let mut iter = TokenIterator::new(s).peekable();
    parse_expr(&mut iter)
}

fn do_parse(s: &str) -> (Defs, Vec<String>) {
    let mut iter = TokenIterator::new(s).peekable();
    parse(&mut iter)
}

#[test]
fn test_parse_term_plus() {
    let expr = do_parse_expr("+1");

    if let Expr::UnaryOp(UnaryOpExpr {
        op: UnaryOpType::Positive,
        expr: x,
    }) = expr
    {
        if let Expr::Const { value: x } = *x {
            if x != 1.into() {
                panic!("number != 1");
            }
        } else {
            panic!("argument of x is not Expr::new_const");
        }
    } else {
        panic!("missing plus");
    }
}

#[test]
fn test_missing_bracket() {
    assert_eq!(
        do_parse_expr("("),
        Expr::Error {
            message: "Expected ), got Eof".into()
        }
    );
}

#[test]
fn test_escapes() {
    assert_eq!(
        do_parse_expr("\\\r"),
        Expr::Error {
            message: "Expected term, got Error(\"Expected LF or CRLF line endings\")".into()
        }
    );
    assert_eq!(
        do_parse_expr("\\\r\n1"),
        Expr::Const {
            value: Numeric::from(1)
        }
    );
    assert_eq!(
        do_parse_expr("\\a"),
        Expr::Error {
            message: "Expected term, got Error(\"Invalid escape: \\\\a\")".into(),
        },
    );
    assert_eq!(
        do_parse_expr("\\"),
        Expr::Error {
            message: "Expected term, got Error(\"Unexpected EOF\")".into()
        }
    );
}

#[test]
fn test_float_leading_dot() {
    assert_eq!(
        do_parse_expr(".123"),
        Expr::Const {
            value: Numeric::Rational(BigRat::small_ratio(123, 1000))
        }
    );
}

#[test]
fn test_escaped_quotes() {
    assert_eq!(
        do_parse_expr("\"ab\\\"\""),
        Expr::Unit {
            name: "ab\"".into()
        }
    );
}

#[test]
fn test_category_directive() {
    let (defs, errors) = do_parse("!category short long");
    assert_eq!(errors, Vec::<String>::new());
    assert_eq!(
        defs.defs,
        vec![DefEntry {
            name: "short".into(),
            def: Rc::new(Def::Category {
                display_name: "long".into()
            }),
            doc: None,
            category: None,
        }]
    );
    let (defs, errors) = do_parse("!category");
    assert_eq!(defs.defs, vec![]);
    assert_eq!(errors, vec!["Malformed category directive"]);

    let (_defs, errors) = do_parse("!endcategory");
    assert_eq!(errors, vec!["Stray endcategory directive"]);
}

#[test]
fn test_symbol_directive() {
    let (_defs, errors) = do_parse("!symbol");
    assert_eq!(errors, vec!["Malformed symbol directive"]);
}

#[test]
fn test_dependency_directive() {
    let (defs, errors) = do_parse("!dependency foo");
    assert_eq!(errors, Vec::<String>::new());
    assert_eq!(
        defs.defs,
        vec![DefEntry {
            name: "foo".into(),
            def: Rc::new(Def::Dependency { name: "foo".into() }),
            doc: None,
            category: None,
        }]
    );
    let (defs, errors) = do_parse("!dependency");
    assert_eq!(defs.defs, vec![]);
    assert_eq!(errors, vec!["Expected ident after `!dependency`"]);
}

#[test]
fn test_unknown_directive() {
    let (defs, errors) = do_parse("!foo asdf xyz");
    assert_eq!(errors, vec!["Unknown directive !foo"]);
    assert_eq!(defs.defs, vec![]);
}

#[test]
fn test_invalid_directive() {
    let (defs, errors) = do_parse("!4 xyz");
    assert_eq!(errors, vec!["syntax error: expected ident after !"]);
    assert_eq!(defs.defs, vec![]);
}

#[test]
fn test_invalid() {
    let (defs, errors) = do_parse("4");
    assert_eq!(
        errors,
        vec!["Expected definition on line 1, got Number(\"4\", None, None)"]
    );
    assert_eq!(defs.defs, vec![]);
}

#[test]
fn test_substances() {
    let (defs, errors) = do_parse("foo {\nconst 4");
    assert_eq!(
        defs.defs,
        vec![DefEntry {
            name: "foo".into(),
            def: Rc::new(Def::Substance {
                symbol: None,
                properties: vec![],
            }),
            doc: None,
            category: None,
        }]
    );
    assert_eq!(
        errors,
        vec!["Expected property input name, got Number(\"4\", None, None)"]
    );
}

#[test]
fn currency_units_parses() {
    let (defs, errors) = do_parse(include_str!("../currency.units"));
    assert!(defs.defs.len() != 0);
    assert_eq!(errors, Vec::<String>::new());
}

#[test]
fn test_tokenize_newlines() {
    let mut iter = TokenIterator::new("foo\r\nbar\rbaz").peekable();
    let res = tokens(&mut iter);
    assert_eq!(
        res,
        vec![
            Token::Ident("foo".into()),
            Token::Newline,
            Token::Ident("bar".into()),
            Token::Newline,
            Token::Ident("baz".into()),
        ]
    );
}

#[test]
fn test_tokenize_numbers() {
    let mut iter = TokenIterator::new("1.0\n1e5\n1e+5\n1e-4\n1.").peekable();
    let res = tokens(&mut iter);
    assert_eq!(
        res,
        vec![
            Token::Number("1".into(), Some("0".into()), None),
            Token::Newline,
            Token::Number("1".into(), None, Some("5".into())),
            Token::Newline,
            Token::Number("1".into(), None, Some("5".into())),
            Token::Newline,
            Token::Number("1".into(), None, Some("-4".into())),
            Token::Newline,
            Token::Number("1".into(), None, None)
        ]
    );
}
