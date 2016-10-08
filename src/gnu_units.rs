// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::str::Chars;
use std::iter::Peekable;
use std::rc::Rc;
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
        ' ' | '\t' | '\n' | '(' | ')' | '/' | '|' | '^' | '+' | '*' | '\\' | '#' => false,
        _ => true
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.0.peek() == None {
            return Some(Token::Eof)
        }
        let res = match self.0.next().unwrap() {
            ' ' | '\t' => return self.next(),
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
                Some('\n') => self.next().unwrap(),
                Some(x) => Token::Error(format!("Invalid escape: \\{}", x)),
                None => Token::Error(format!("Unexpected EOF")),
            },
            '#' => {
                while let Some(c) = self.0.next() {
                    match c {
                        '\n' => break,
                        _ => ()
                    }
                }
                Token::Newline
            },
            x @ '0'...'9' | x @ '.' => {
                use std::ascii::AsciiExt;

                let mut integer = String::new();
                let mut frac = None;
                let mut exp = None;

                // integer component
                if x != '.' {
                    integer.push(x);
                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0'...'9' => integer.push(self.0.next().unwrap()),
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
                            '0'...'9' => buf.push(self.0.next().unwrap()),
                            _ => break
                        }
                    }
                    if buf.len() > 0 {
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
                            '0'...'9' => buf.push(self.0.next().unwrap()),
                            _ => break
                        }
                    }
                    if buf.len() > 0 {
                        exp = Some(buf)
                    }
                }
                Token::Number(integer, frac, exp)
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

fn parse_term(mut iter: &mut Iter) -> Expr {
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
            .unwrap_or_else(|e| Expr::Error(format!("{}", e))),
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

fn parse_pow(mut iter: &mut Iter) -> Expr {
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

fn parse_mul(mut iter: &mut Iter) -> Expr {
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

fn parse_div(mut iter: &mut Iter) -> Expr {
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

fn parse_add(mut iter: &mut Iter) -> Expr {
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

pub fn parse_expr(mut iter: &mut Iter) -> Expr {
    parse_add(iter)
}

pub fn parse(mut iter: &mut Iter) -> Defs {
    let mut map = vec![];
    let mut line = 1;
    let mut doc = None;
    loop {
        match iter.next().unwrap() {
            Token::Newline => line += 1,
            Token::Eof => break,
            Token::Bang => loop {
                match iter.next().unwrap() {
                    Token::Eof | Token::Newline => break,
                    _ => ()
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
                        map.push((name, Rc::new(Def::Prefix(expr)), doc.take()));
                    } else {
                        map.push((name, Rc::new(Def::SPrefix(expr)), doc.take()));
                    }
                } else {
                    // unit
                    if let Some(&Token::Bang) = iter.peek() {
                        // dimension
                        iter.next();
                        if let Some(Token::Ident(ref long)) = iter.peek().cloned() {
                            iter.next();
                            map.push((name.clone(), Rc::new(Def::Dimension), doc.take()));
                            map.push((long.clone(), Rc::new(Def::Canonicalization(name.clone())),
                                      doc.take()));
                        } else {
                            map.push((name.clone(), Rc::new(Def::Dimension), doc.take()));
                        }
                    } else if let Some(&Token::Question) = iter.peek() {
                        // quantity
                        iter.next();
                        let expr = parse_expr(iter);
                        map.push((name, Rc::new(Def::Quantity(expr)), doc.take()));
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
                            let input_name = match iter.next().unwrap() {
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
                                        name: name,
                                        input: Expr::Const(Num::one()),
                                        input_name: input_name,
                                        output: output,
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
                            let input = parse_mul(iter);
                            match iter.next().unwrap() {
                                Token::Slash => (),
                                x => {
                                    println!("Expected /, got {:?}", x);
                                    break
                                }
                            }
                            let output_name = match iter.next().unwrap() {
                                Token::Ident(name) => name,
                                x => {
                                    println!("Expected property input name, got {:?}", x);
                                    break
                                },
                            };
                            let output = parse_mul(iter);
                            props.push(Property {
                                name: name,
                                input: input,
                                input_name: input_name,
                                output: output,
                                output_name: output_name,
                                doc: prop_doc.take()
                            });
                        }
                        map.push((
                            name,
                            Rc::new(Def::Substance(props)),
                            doc.take()));
                    } else {
                        // derived
                        let expr = parse_expr(iter);
                        map.push((name, Rc::new(Def::Unit(expr)), doc.take()));
                    }
                }
            },
            x => println!("Expected definition on line {}, got {:?}", line, x),
        };
    }
    Defs {
        defs: map,
    }
}

pub fn tokens(mut iter: &mut Iter) -> Vec<Token> {
    let mut out = vec![];
    loop {
        match iter.next().unwrap() {
            Token::Eof => break,
            x => out.push(x)
        }
    }
    out
}
