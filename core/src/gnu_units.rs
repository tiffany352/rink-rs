// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::ast::*;
use crate::numeric::Numeric;
use std::collections::BTreeMap;
use std::iter::Peekable;
use std::rc::Rc;
use std::str::Chars;

#[derive(Debug, Clone)]
pub enum Token {
    Eof,
    Newline,
    Doc(String),
    Ident(String),
    Number(String, Option<String>, Option<String>),
    LPar,
    RPar,
    Bang,
    Slash,
    Pipe,
    Caret,
    Plus,
    Dash,
    Asterisk,
    Question,
    LeftBrace,
    RightBrace,
    Error(String),
}

#[derive(Clone)]
pub struct TokenIterator<'a>(Peekable<Chars<'a>>);

impl<'a> TokenIterator<'a> {
    pub fn new(input: &'a str) -> TokenIterator<'a> {
        TokenIterator(input.chars().peekable())
    }
}

fn is_ident(c: char) -> bool {
    match c {
        //c if c.is_alphabetic() => true,
        //'_' | '$' | '-' | '\'' | '"' | '%' | ',' => true,
        ' ' | '\t' | '\n' | '\r' | '(' | ')' | '/' | '|' | '^' | '+' | '*' | '\\' | '#' => false,
        _ => true,
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.0.peek().is_none() {
            return Some(Token::Eof);
        }
        let res = match self.0.next().unwrap() {
            ' ' | '\t' => return self.next(),
            '\r' => {
                if self.0.peek() == Some(&'\n') {
                    self.0.next();
                    Token::Newline
                } else {
                    Token::Newline
                }
            }
            '\n' => Token::Newline,
            '!' => Token::Bang,
            '(' => Token::LPar,
            ')' => Token::RPar,
            '/' => Token::Slash,
            '|' => Token::Pipe,
            '^' => Token::Caret,
            '-' => Token::Dash,
            '+' => Token::Plus,
            '*' => Token::Asterisk,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '?' => {
                if self.0.peek() == Some(&'?') {
                    self.0.next();
                    let mut out = String::new();
                    loop {
                        match self.0.next() {
                            Some('\n') | None => break,
                            Some(x) => out.push(x),
                        }
                    }
                    Token::Doc(out)
                } else {
                    Token::Question
                }
            }
            '\\' => match self.0.next() {
                Some('\r') => match self.0.next() {
                    Some('\n') => self.next().unwrap(),
                    _ => Token::Error("Expected LF or CRLF line endings".to_string()),
                },
                Some('\n') => self.next().unwrap(),
                Some(x) => Token::Error(format!("Invalid escape: \\{}", x)),
                None => Token::Error("Unexpected EOF".to_string()),
            },
            '#' => {
                while let Some(c) = self.0.next() {
                    if c == '\n' {
                        break;
                    }
                }
                Token::Newline
            }
            x @ '0'..='9' | x @ '.' => {
                let mut integer = String::new();
                let mut frac = None;
                let mut exp = None;

                // integer component
                if x != '.' {
                    integer.push(x);
                    while let Some('0'..='9') = self.0.peek().cloned() {
                        integer.push(self.0.next().unwrap());
                    }
                } else {
                    integer.push('0');
                }
                // fractional component
                if x == '.' || Some('.') == self.0.peek().cloned() {
                    let mut buf = String::new();
                    if x != '.' {
                        self.0.next();
                    }
                    while let Some('0'..='9') = self.0.peek().cloned() {
                        buf.push(self.0.next().unwrap());
                    }
                    if !buf.is_empty() {
                        frac = Some(buf)
                    }
                }
                // exponent
                if let Some('e') = self.0.peek().cloned().map(|x| x.to_ascii_lowercase()) {
                    let mut buf = String::new();
                    self.0.next();
                    if let Some(c) = self.0.peek().cloned() {
                        match c {
                            '-' => {
                                buf.push(self.0.next().unwrap());
                            }
                            '+' => {
                                self.0.next();
                            }
                            _ => (),
                        }
                    }
                    while let Some('0'..='9') = self.0.peek().cloned() {
                        buf.push(self.0.next().unwrap());
                    }
                    if !buf.is_empty() {
                        exp = Some(buf)
                    }
                }
                Token::Number(integer, frac, exp)
            }
            '"' => {
                let mut buf = String::new();
                while let Some(c) = self.0.next() {
                    if c == '\\' {
                        if let Some(c) = self.0.next() {
                            buf.push(c);
                        }
                    } else if c == '"' {
                        break;
                    } else {
                        buf.push(c);
                    }
                }
                Token::Ident(buf)
            }
            x if is_ident(x) => {
                let mut buf = String::new();
                buf.push(x);
                while let Some(c) = self.0.peek().cloned() {
                    if is_ident(c) || c.is_numeric() {
                        buf.push(self.0.next().unwrap());
                    } else {
                        break;
                    }
                }
                Token::Ident(buf)
            }
            x => Token::Error(format!("Unknown character: '{}'", x)),
        };
        Some(res)
    }
}

pub type Iter<'a> = Peekable<TokenIterator<'a>>;

fn parse_term(iter: &mut Iter<'_>) -> Expr {
    match iter.next().unwrap() {
        Token::Ident(name) => match iter.peek().cloned().unwrap() {
            Token::Ident(ref s) if s == "of" => {
                iter.next();
                Expr::Of {
                    property: name,
                    expr: Box::new(parse_mul(iter)),
                }
            }
            _ => Expr::new_unit(name),
        },
        Token::Number(num, frac, exp) => crate::number::Number::from_parts(
            &*num,
            frac.as_ref().map(|x| &**x),
            exp.as_ref().map(|x| &**x),
        )
        .map(Expr::new_const)
        .unwrap_or_else(Expr::new_error),
        Token::Plus => Expr::new_plus(parse_term(iter)),
        Token::Dash => Expr::new_negate(parse_term(iter)),
        Token::Slash => Expr::new_frac(Expr::new_const(Numeric::one()), parse_term(iter)),
        Token::LPar => {
            let res = parse_expr(iter);
            match iter.next().unwrap() {
                Token::RPar => res,
                x => Expr::new_error(format!("Expected ), got {:?}", x)),
            }
        }
        x => Expr::new_error(format!("Expected term, got {:?}", x)),
    }
}

fn parse_pow(iter: &mut Iter<'_>) -> Expr {
    let left = parse_term(iter);
    match *iter.peek().unwrap() {
        Token::Caret => {
            iter.next();
            let right = parse_pow(iter);
            Expr::BinOp(BinOpExpr {
                op: BinOpType::Pow,
                left: Box::new(left),
                right: Box::new(right),
            })
        }
        Token::Pipe => {
            iter.next();
            let right = parse_pow(iter);
            Expr::BinOp(BinOpExpr {
                op: BinOpType::Frac,
                left: Box::new(left),
                right: Box::new(right),
            })
        }
        _ => left,
    }
}

fn parse_mul(iter: &mut Iter<'_>) -> Expr {
    let mut terms = vec![parse_pow(iter)];
    loop {
        match iter.peek().cloned().unwrap() {
            Token::Slash
            | Token::Plus
            | Token::Dash
            | Token::RPar
            | Token::Newline
            | Token::Eof => break,
            Token::Asterisk => {
                iter.next();
            }
            _ => terms.push(parse_pow(iter)),
        }
    }
    if terms.len() == 1 {
        terms.pop().unwrap()
    } else {
        Expr::new_mul(terms)
    }
}

fn parse_div(iter: &mut Iter<'_>) -> Expr {
    let mut left = parse_mul(iter);
    while let Token::Slash = *iter.peek().unwrap() {
        iter.next();
        let right = parse_mul(iter);
        left = Expr::new_frac(left, right);
    }
    left
}

fn parse_add(iter: &mut Iter<'_>) -> Expr {
    let left = parse_div(iter);
    match *iter.peek().unwrap() {
        Token::Plus => {
            iter.next();
            let right = parse_add(iter);
            Expr::new_add(left, right)
        }
        Token::Dash => {
            iter.next();
            let right = parse_add(iter);
            Expr::new_sub(left, right)
        }
        _ => left,
    }
}

pub fn parse_expr(iter: &mut Iter<'_>) -> Expr {
    parse_add(iter)
}

pub fn parse(iter: &mut Iter<'_>) -> Defs {
    let mut map = vec![];
    let mut line = 1;
    let mut doc: Option<String> = None;
    let mut category: Option<String> = None;
    let mut symbols = BTreeMap::new();
    loop {
        match iter.next().unwrap() {
            Token::Newline => line += 1,
            Token::Eof => break,
            Token::Bang => match iter.next().unwrap() {
                Token::Ident(ref s) if s == "category" => {
                    match (iter.next().unwrap(), iter.next().unwrap()) {
                        (Token::Ident(short), Token::Ident(display_name)) => {
                            map.push(DefEntry {
                                name: short.clone(),
                                def: Rc::new(Def::Category { display_name }),
                                doc: None,
                                category: None,
                            });
                            category = Some(short);
                        }
                        _ => println!("Malformed category directive"),
                    }
                }
                Token::Ident(ref s) if s == "endcategory" => {
                    if category.is_none() {
                        println!("Stray endcategory directive");
                    }
                    category = None
                }
                Token::Ident(ref s) if s == "symbol" => {
                    match (iter.next().unwrap(), iter.next().unwrap()) {
                        (Token::Ident(subst), Token::Ident(sym)) => {
                            symbols.insert(subst, sym);
                        }
                        _ => println!("Malformed symbol directive"),
                    }
                }
                _ => loop {
                    match iter.peek().cloned().unwrap() {
                        Token::Newline | Token::Eof => break,
                        _ => {
                            iter.next();
                        }
                    }
                },
            },
            Token::Doc(line) => {
                doc = match doc.take() {
                    None => Some(line.trim().to_owned()),
                    Some(old) => Some(format!("{} {}", old.trim(), line.trim())),
                };
            }
            Token::Ident(name) => {
                if name.ends_with('-') {
                    // prefix
                    let expr = parse_expr(iter);
                    let mut name = name;
                    name.pop();
                    if name.ends_with('-') {
                        name.pop();
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::Prefix {
                                expr: ExprString(expr),
                            }),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    } else {
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::SPrefix {
                                expr: ExprString(expr),
                            }),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    }
                } else {
                    // unit
                    if let Some(&Token::Bang) = iter.peek() {
                        // dimension
                        iter.next();
                        if let Some(Token::Ident(ref long)) = iter.peek().cloned() {
                            iter.next();
                            map.push(DefEntry {
                                name: name.clone(),
                                def: Rc::new(Def::Dimension),
                                doc: doc.take(),
                                category: category.clone(),
                            });
                            map.push(DefEntry {
                                name: long.clone(),
                                def: Rc::new(Def::Canonicalization { of: name.clone() }),
                                doc: doc.take(),
                                category: category.clone(),
                            });
                        } else {
                            map.push(DefEntry {
                                name: name.clone(),
                                def: Rc::new(Def::Dimension),
                                doc: doc.take(),
                                category: category.clone(),
                            });
                        }
                    } else if let Some(&Token::Question) = iter.peek() {
                        // quantity
                        iter.next();
                        let expr = parse_expr(iter);
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::Quantity {
                                expr: ExprString(expr),
                            }),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    } else if let Some(&Token::LeftBrace) = iter.peek() {
                        // substance
                        iter.next();
                        let mut props = vec![];
                        let mut prop_doc = None;
                        loop {
                            let name = match iter.next().unwrap() {
                                Token::Ident(name) => name,
                                Token::Newline => {
                                    line += 1;
                                    continue;
                                }
                                Token::Eof => break,
                                Token::Doc(line) => {
                                    prop_doc = match prop_doc.take() {
                                        None => Some(line.trim().to_owned()),
                                        Some(old) => {
                                            Some(format!("{} {}", old.trim(), line.trim()))
                                        }
                                    };
                                    continue;
                                }
                                Token::RightBrace => break,
                                x => {
                                    println!("Expected property, got {:?}", x);
                                    break;
                                }
                            };
                            let output_name = match iter.next().unwrap() {
                                Token::Ident(ref s) if s == "const" => {
                                    let input_name = match iter.next().unwrap() {
                                        Token::Ident(name) => name,
                                        x => {
                                            println!(
                                                "Expected property input \
                                                 name, got {:?}",
                                                x
                                            );
                                            break;
                                        }
                                    };
                                    let output = parse_div(iter);
                                    props.push(Property {
                                        output_name: name.clone(),
                                        name,
                                        input: ExprString(Expr::new_const(Numeric::one())),
                                        input_name,
                                        output: ExprString(output),
                                        doc: prop_doc.take(),
                                    });
                                    continue;
                                }
                                Token::Ident(name) => name,
                                x => {
                                    println!("Expected property input name, got {:?}", x);
                                    break;
                                }
                            };
                            let output = parse_mul(iter);
                            match iter.next().unwrap() {
                                Token::Slash => (),
                                x => {
                                    println!("Expected /, got {:?}", x);
                                    break;
                                }
                            }
                            let input_name = match iter.next().unwrap() {
                                Token::Ident(name) => name,
                                x => {
                                    println!("Expected property input name, got {:?}", x);
                                    break;
                                }
                            };
                            let input = parse_mul(iter);
                            props.push(Property {
                                name,
                                input: ExprString(input),
                                input_name,
                                output: ExprString(output),
                                output_name,
                                doc: prop_doc.take(),
                            });
                        }
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::Substance {
                                symbol: None,
                                properties: props,
                            }),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    } else {
                        // derived
                        let expr = parse_expr(iter);
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::Unit {
                                expr: ExprString(expr),
                            }),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    }
                }
            }
            x => println!("Expected definition on line {}, got {:?}", line, x),
        };
    }

    for entry in map.iter_mut() {
        if let Def::Substance { ref mut symbol, .. } = *Rc::get_mut(&mut entry.def).unwrap() {
            *symbol = symbols.get(&entry.name).map(|x| x.to_owned())
        }
    }

    Defs { defs: map }
}

pub fn parse_str(input: &str) -> Defs {
    let mut iter = TokenIterator::new(&*input).peekable();
    parse(&mut iter)
}

pub fn tokens(iter: &mut Iter<'_>) -> Vec<Token> {
    let mut out = vec![];
    loop {
        match iter.next().unwrap() {
            Token::Eof => break,
            x => out.push(x),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;

    fn do_parse(s: &str) -> Expr {
        let mut iter = TokenIterator::new(s).peekable();
        parse_term(&mut iter)
    }

    macro_rules! expect {
        ($expr:expr, $pattern:pat, $var:ident, $expected:expr) => {
            match do_parse($expr) {
                $pattern => assert_eq!($var, $expected),
                x => panic!("{}", x),
            }
        };
    }

    #[test]
    fn test_parse_term_plus() {
        let expr = do_parse("+1");

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
        expect!("(", Expr::Error { ref message }, message, "Expected ), got Eof");
    }

    #[test]
    fn test_escapes() {
        expect!(
            "\\\r",
            Expr::Error { ref message },
            message,
            "Expected term, got Error(\"Expected LF or CRLF line endings\")"
        );
        expect!("\\\r\n1", Expr::Const { value }, value, Numeric::from(1));

        expect!(
            "\\a",
            Expr::Error { ref message },
            message,
            "Expected term, got Error(\"Invalid escape: \\\\a\")"
        );
        expect!(
            "\\",
            Expr::Error { ref message },
            message,
            "Expected term, got Error(\"Unexpected EOF\")"
        );
    }

    #[test]
    fn test_float_leading_dot() {
        use crate::bigrat::BigRat;
        expect!(
            ".123",
            Expr::Const { value },
            value,
            Numeric::Rational(BigRat::small_ratio(123, 1000))
        );
    }

    #[test]
    fn test_escaped_quotes() {
        expect!("\"ab\\\"\"", Expr::Unit { ref name }, name, "ab\"")
    }
}
