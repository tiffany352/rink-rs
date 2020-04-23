// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::str::Chars;
use std::iter::Peekable;
use std::rc::Rc;
use std::collections::BTreeMap;
use ast::*;
use num::Num;

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
        _ => true
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.0.peek().is_none() {
            return Some(Token::Eof)
        }
        let res = match self.0.next().unwrap() {
            ' ' | '\t' => return self.next(),
            '\r' => if self.0.peek() == Some(&'\n') {
                self.0.next();
                Token::Newline
            } else {
                Token::Newline
            },
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
            '?' => if self.0.peek() == Some(&'?') {
                self.0.next();
                let mut out = String::new();
                loop { match self.0.next() {
                    Some('\n') | None => break,
                    Some(x) => out.push(x),
                }}
                Token::Doc(out)
            } else {
                Token::Question
            },
            '\\' => match self.0.next() {
                Some('\r') => match self.0.next() {
                    Some('\n') => self.next().unwrap(),
                    _ => Token::Error("Expected LF or CRLF line endings".to_string())
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
            },
            x @ '0'..='9' | x @ '.' => {
                let mut integer = String::new();
                let mut frac = None;
                let mut exp = None;

                // integer component
                if x != '.' {
                    integer.push(x);
                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0'..='9' => integer.push(self.0.next().unwrap()),
                            _ => break
                        }
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
                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0'..='9' => buf.push(self.0.next().unwrap()),
                            _ => break
                        }
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
                            },
                            '+' => {
                                self.0.next();
                            },
                            _ => ()
                        }
                    }
                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0'..='9' => buf.push(self.0.next().unwrap()),
                            _ => break
                        }
                    }
                    if !buf.is_empty() {
                        exp = Some(buf)
                    }
                }
                Token::Number(integer, frac, exp)
            },
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
            },
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
                match &*buf {
                    _ => Token::Ident(buf)
                }
            },
            x => Token::Error(format!("Unknown character: '{}'", x))
        };
        Some(res)
    }
}

pub type Iter<'a> = Peekable<TokenIterator<'a>>;

fn parse_term(iter: &mut Iter) -> Expr {
    match iter.next().unwrap() {
        Token::Ident(name) => match iter.peek().cloned().unwrap() {
            Token::Ident(ref s) if s == "of" => {
                iter.next();
                Expr::Of(name, Box::new(parse_mul(iter)))
            },
            _ => Expr::Unit(name)
        },
        Token::Number(num, frac, exp) =>
            ::number::Number::from_parts(&*num, frac.as_ref().map(|x| &**x), exp.as_ref().map(|x| &**x))
            .map(Expr::Const)
            .unwrap_or_else(|e| Expr::Error(e.to_string())),
        Token::Plus => Expr::Plus(Box::new(parse_term(iter))),
        Token::Dash => Expr::Neg(Box::new(parse_term(iter))),
        Token::Slash => Expr::Frac(
            Box::new(Expr::Const(Num::one())),
            Box::new(parse_term(iter))),
        Token::LPar => {
            let res = parse_expr(iter);
            match iter.next().unwrap() {
                Token::RPar => res,
                x => Expr::Error(format!("Expected ), got {:?}", x))
            }
        },
        x => Expr::Error(format!("Expected term, got {:?}", x))
    }
}

fn parse_pow(iter: &mut Iter) -> Expr {
    let left = parse_term(iter);
    match *iter.peek().unwrap() {
        Token::Caret => {
            iter.next();
            let right = parse_pow(iter);
            Expr::Pow(Box::new(left), Box::new(right))
        },
        Token::Pipe => {
            iter.next();
            let right = parse_pow(iter);
            Expr::Frac(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

fn parse_mul(iter: &mut Iter) -> Expr {
    let mut terms = vec![parse_pow(iter)];
    loop { match iter.peek().cloned().unwrap() {
        Token::Slash | Token::Plus | Token::Dash | Token::RPar | Token::Newline | Token::Eof =>
            break,
        Token::Asterisk => {
            iter.next();
        },
        _ => terms.push(parse_pow(iter))
    }}
    if terms.len() == 1 {
        terms.pop().unwrap()
    } else {
        Expr::Mul(terms)
    }
}

fn parse_div(iter: &mut Iter) -> Expr {
    let mut left = parse_mul(iter);
    loop { match *iter.peek().unwrap() {
        Token::Slash => {
            iter.next();
            let right = parse_mul(iter);
            left = Expr::Frac(Box::new(left), Box::new(right));
        },
        _ => break
    }}
    left
}

fn parse_add(iter: &mut Iter) -> Expr {
    let left = parse_div(iter);
    match *iter.peek().unwrap() {
        Token::Plus => {
            iter.next();
            let right = parse_add(iter);
            Expr::Add(Box::new(left), Box::new(right))
        },
        Token::Dash => {
            iter.next();
            let right = parse_add(iter);
            Expr::Sub(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

pub fn parse_expr(iter: &mut Iter) -> Expr {
    parse_add(iter)
}

pub fn parse(iter: &mut Iter) -> Defs {
    let mut map = vec![];
    let mut line = 1;
    let mut doc = None;
    let mut category = None;
    let mut symbols = BTreeMap::new();
    loop {
        match iter.next().unwrap() {
            Token::Newline => line += 1,
            Token::Eof => break,
            Token::Bang => {
                match iter.next().unwrap() {
                    Token::Ident(ref s) if s == "category" => {
                        match (iter.next().unwrap(), iter.next().unwrap()) {
                            (Token::Ident(s), Token::Ident(d)) => {
                                map.push(DefEntry {
                                    name: s.clone(),
                                    def: Rc::new(Def::Category(d)),
                                    doc: None,
                                    category: None
                                });
                                category = Some(s);
                            },
                            _ => println!("Malformed category directive"),
                        }
                    },
                    Token::Ident(ref s) if s == "endcategory" => {
                        if category.is_none() {
                            println!("Stray endcategory directive");
                        }
                        category = None
                    },
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
                }
            },
            Token::Doc(line) => {
                doc = match doc.take() {
                    None => Some(line.trim().to_owned()),
                    Some(old) => Some(format!("{} {}", old.trim(), line.trim())),
                };
            },
            Token::Ident(name) => {
                if name.ends_with("-") {
                    // prefix
                    let expr = parse_expr(iter);
                    let mut name = name;
                    name.pop();
                    if name.ends_with("-") {
                        name.pop();
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::Prefix(expr)),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    } else {
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::SPrefix(expr)),
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
                                def: Rc::new(Def::Canonicalization(name.clone())),
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
                            def: Rc::new(Def::Quantity(expr)),
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
                                    continue
                                },
                                Token::Eof => break,
                                Token::Doc(line) => {
                                    prop_doc = match prop_doc.take() {
                                        None => Some(line.trim().to_owned()),
                                        Some(old) => Some(format!(
                                            "{} {}", old.trim(), line.trim())),
                                    };
                                    continue
                                },
                                Token::RightBrace =>
                                    break,
                                x => {
                                    println!("Expected property, got {:?}", x);
                                    break
                                },
                            };
                            let output_name = match iter.next().unwrap() {
                                Token::Ident(ref s) if s == "const" => {
                                    let input_name = match iter.next().unwrap() {
                                        Token::Ident(name) => name,
                                        x => {
                                            println!("Expected property input \
                                                      name, got {:?}", x);
                                            break
                                        },
                                    };
                                    let output = parse_div(iter);
                                    props.push(Property {
                                        output_name: name.clone(),
                                        name,
                                        input: Expr::Const(Num::one()),
                                        input_name,
                                        output,
                                        doc: prop_doc.take()
                                    });
                                    continue
                                },
                                Token::Ident(name) => name,
                                x => {
                                    println!("Expected property input name, got {:?}", x);
                                    break
                                },
                            };
                            let output = parse_mul(iter);
                            match iter.next().unwrap() {
                                Token::Slash => (),
                                x => {
                                    println!("Expected /, got {:?}", x);
                                    break
                                }
                            }
                            let input_name = match iter.next().unwrap() {
                                Token::Ident(name) => name,
                                x => {
                                    println!("Expected property input name, got {:?}", x);
                                    break
                                },
                            };
                            let input = parse_mul(iter);
                            props.push(Property {
                                name,
                                input,
                                input_name,
                                output,
                                output_name,
                                doc: prop_doc.take()
                            });
                        }
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::Substance {
                                symbol: None,
                                properties: props
                            }),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    } else {
                        // derived
                        let expr = parse_expr(iter);
                        map.push(DefEntry {
                            name,
                            def: Rc::new(Def::Unit(expr)),
                            doc: doc.take(),
                            category: category.clone(),
                        });
                    }
                }
            },
            x => println!("Expected definition on line {}, got {:?}", line, x),
        };
    }

    for entry in map.iter_mut() {
        match Rc::get_mut(&mut entry.def).unwrap() {
            &mut Def::Substance { ref mut symbol, .. } => {
                *symbol = symbols.get(&entry.name).map(|x| x.to_owned())
            }
            _ => ()
        }
    }

    Defs {
        defs: map
    }
}

pub fn tokens(iter: &mut Iter) -> Vec<Token> {
    let mut out = vec![];
    loop {
        match iter.next().unwrap() {
            Token::Eof => break,
            x => out.push(x)
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::Expr;

    fn do_parse(s: &str) -> Expr {
        let mut iter = TokenIterator::new(s).peekable();
        parse_term(&mut iter)
    }

    macro_rules! expect {
        ($expr:expr, $pattern:path, $expected:expr) => {
            match do_parse($expr) {
                $pattern(s) => assert_eq!(s, $expected),
                x => panic!("{}", x),
            }
        };
    }

    #[test]
    fn test_parse_term_plus() {
        let expr = do_parse("+1");

        if let Expr::Plus(x) = expr {
            if let Expr::Const(x) = *x {
                if x != 1.into() {
                    panic!("number != 1");
                }
            } else {
                panic!("argument of x is not Expr::Const");
            }
        } else {
            panic!("missing plus");
        }
    }

    #[test]
    fn test_missing_bracket() {
        match do_parse("(") {
            Expr::Error(ref s) => assert_eq!(s, "Expected ), got Eof"),
            x => panic!("Wrong result: {}", x),
        }
    }

    #[test]
    fn test_escapes() {
        expect!(
            "\\\r",
            Expr::Error,
            "Expected term, got Error(\"Expected LF or CRLF line endings\")"
        );
        expect!("\\\r\n1", Expr::Const, 1.into());

        expect!(
            "\\a",
            Expr::Error,
            "Expected term, got Error(\"Invalid escape: \\\\a\")"
        );
        expect!(
            "\\",
            Expr::Error,
            "Expected term, got Error(\"Unexpected EOF\")"
        );
    }

    #[test]
    fn test_float_leading_dot() {
        use gmp::mpq::Mpq;
        use gmp::mpz::Mpz;
        let num = Mpz::from(123);
        let den = Mpz::from(1000);
        expect!(".123", Expr::Const, Num::Mpq(Mpq::ratio(&num, &den)));
    }

    #[test]
    fn test_escaped_quotes() {
        expect!("\"ab\\\"\"", Expr::Unit, "ab\"")
    }
}
