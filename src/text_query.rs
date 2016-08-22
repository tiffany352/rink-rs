// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::str::Chars;
use std::iter::Peekable;
use ast::*;

#[derive(Debug, Clone)]
pub enum Token {
    Newline,
    Comment(usize),
    Ident(String),
    Number(String, Option<String>, Option<String>),
    Quote(String),
    Slash,
    Colon,
    ColonDash,
    DColonDash,
    TriplePipe,
    ColonEq,
    EqBangEq,
    Equals,
    Carot,
    Eof,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    LPar,
    RPar,
    Plus,
    Minus,
    Asterisk,
    DashArrow,
    Date(Vec<DateToken>),
    Comma,
    ImaginaryUnit,
    DegC,
    DegF,
    DegRe,
    DegRo,
    DegDe,
    DegN,
    Error(String),
}

#[derive(Clone)]
pub struct TokenIterator<'a>(Peekable<Chars<'a>>);

impl<'a> TokenIterator<'a> {
    pub fn new(input: &'a str) -> TokenIterator<'a> {
        TokenIterator(input.chars().peekable())
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
            '[' => Token::LBrack,
            ']' => Token::RBrack,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '(' => Token::LPar,
            ')' => Token::RPar,
            '+' => Token::Plus,
            '%' => Token::Ident("percent".to_owned()),
            '-' => match self.0.peek().cloned().unwrap() {
                '>' => {
                    self.0.next();
                    Token::DashArrow
                },
                _ => Token::Minus
            },
            '*' => Token::Asterisk,
            ',' => Token::Comma,
            '/' => match self.0.peek() {
                Some(&'/') => loop {
                    match self.0.next() {
                        None | Some('\n') => return Some(Token::Comment(1)),
                        _ => ()
                    }
                },
                Some(&'*') => {
                    let mut lines = 0;
                    loop {
                        if let Some(&'\n') = self.0.peek() {
                            lines += 1;
                        }
                        if let Some('*') = self.0.next() {
                            if let Some(&'/') = self.0.peek() {
                                return Some(Token::Comment(lines))
                            }
                        }
                        if self.0.peek() == None {
                            return Some(Token::Error(format!("Expected `*/`, got EOF")))
                        }
                    }
                },
                _ => Token::Slash
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
                    if let Some('e') = self.0.peek().cloned().map(|x| x.to_ascii_lowercase()) {
                        self.0.next();
                    }
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
            ':' => match self.0.peek().cloned() {
                Some(':') => { self.0.next(); match self.0.next() {
                    Some('-') => Token::DColonDash,
                    x => Token::Error(format!("Unexpected {:?}", x)),
                }},
                Some('-') => {self.0.next(); Token::ColonDash},
                Some('=') => {self.0.next(); Token::ColonEq},
                _ => Token::Colon,
            },
            '|' => if let Some('|') = self.0.next() {
                if let Some('|') = self.0.next() {
                    Token::TriplePipe
                } else {
                    Token::Error(format!("Unknown symbol"))
                }
            } else {
                Token::Error(format!("Unknown symbol"))
            },
            '=' => if let Some('!') = self.0.peek().cloned() {
                self.0.next();
                if let Some('=') = self.0.next() {
                    Token::EqBangEq
                } else {
                    Token::Error(format!("Unknown symbol"))
                }
            } else {
                Token::Equals
            },
            '^' => Token::Carot,
            '\\' => match self.0.next() {
                Some('u') => {
                    let mut buf = String::new();
                    while let Some(c) = self.0.peek().cloned() {
                        if c.is_digit(16) {
                            buf.push(self.0.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    let v = u32::from_str_radix(&*buf, 16).unwrap();
                    if let Some(c) = ::std::char::from_u32(v) {
                        let mut buf = String::new();
                        buf.push(c);
                        Token::Ident(buf)
                    } else {
                        Token::Error(format!("Invalid unicode scalar: {:x}", v))
                    }
                },
                _ => Token::Error(format!("Unexpected \\"))
            },
            '<' => {
                let mut string = "<IMAGINARY_UNIT>>".chars();
                while self.0.peek().is_some() && self.0.next() == string.next() {}
                if string.next() == None {
                    Token::ImaginaryUnit
                } else {
                    Token::Error(format!("Unexpected <"))
                }
            },
            '\'' => {
                let mut buf = String::new();
                loop {
                    match self.0.next() {
                        None | Some('\n') => return Some(Token::Error(format!("Unexpected newline or EOF"))),
                        Some('\\') => match self.0.next() {
                            Some('\'') => buf.push('\''),
                            Some('n') => buf.push('\n'),
                            Some('t') => buf.push('\t'),
                            Some(c) => return Some(Token::Error(format!("Invalid escape sequence \\{}", c))),
                            None => return Some(Token::Error(format!("Unexpected EOF"))),
                        },
                        Some('\'') => break,
                        Some(c) => buf.push(c),
                    }
                }
                Token::Quote(buf)
            },
            '#' => {
                let mut toks = vec![];
                while self.0.peek().is_some() {
                    let res = match self.0.next().unwrap() {
                        '#' => break,
                        ':' => DateToken::Colon,
                        '-' => DateToken::Dash,
                        '+' => DateToken::Plus,
                        x if x.is_whitespace() => {
                            while self.0.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                                self.0.next();
                            }
                            DateToken::Space
                        },
                        x if x.is_alphabetic() => {
                            let mut buf = String::new();
                            buf.push(x);
                            while let Some(c) = self.0.peek().cloned() {
                                if c.is_alphabetic() {
                                    self.0.next();
                                    buf.push(c);
                                } else {
                                    break;
                                }
                            }
                            DateToken::Literal(buf)
                        },
                        x if x.is_digit(10) => {
                            let mut integer = String::new();
                            integer.push(x);
                            while let Some(c) = self.0.peek().cloned() {
                                if c.is_digit(10) {
                                    self.0.next();
                                    integer.push(c);
                                } else {
                                    break;
                                }
                            }
                            let frac = if let Some('.') = self.0.peek().cloned() {
                                let mut frac = String::new();
                                self.0.next();
                                while let Some(c) = self.0.peek().cloned() {
                                    if x.is_digit(10) {
                                        self.0.next();
                                        frac.push(c);
                                    } else {
                                        break;
                                    }
                                }
                                Some(frac)
                            } else {
                                None
                            };
                            DateToken::Number(integer, frac)
                        },
                        x => DateToken::Error(format!("Unexpected character '{}'", x))
                    };
                    toks.push(res);
                }
                if let Some(&DateToken::Space) = toks.first() {
                    toks.remove(0);
                }
                if let Some(&DateToken::Space) = toks.last() {
                    toks.pop();
                }
                Token::Date(toks)
            },
            x => {
                let mut buf = String::new();
                buf.push(x);
                while let Some(c) = self.0.peek().cloned() {
                    if c.is_alphanumeric() || c == '_' {
                        buf.push(self.0.next().unwrap());
                    } else {
                        break;
                    }
                }
                match &*buf {
                    "degC" | "°C" | "celsius" => Token::DegC,
                    "degF" | "°F" | "fahrenheit" => Token::DegF,
                    "degRé" | "°Ré" | "degRe" | "°Re" | "réaumur" | "reaumur" => Token::DegRe,
                    "degRø" | "°Rø" | "degRo" | "°Ro" | "rømer" | "romer" => Token::DegRo,
                    "degDe" | "°De" | "delisle" => Token::DegDe,
                    "degN" | "°N" | "degnewton" => Token::DegN,
                    "per" => Token::Slash,
                    "to" => Token::DashArrow,
                    _ => Token::Ident(buf)
                }
            }
        };
        Some(res)
    }
}

pub type Iter<'a> = Peekable<TokenIterator<'a>>;

fn is_func(name: &str) -> bool {
    match name {
        "sqrt" => true,
        _ => false
    }
}

fn parse_term(mut iter: &mut Iter) -> Expr {
    match iter.next().unwrap() {
        Token::Ident(ref name) if is_func(name) => {
            match iter.peek().cloned().unwrap() {
                Token::LPar => {
                    iter.next();
                    let mut args = vec![];
                    loop {
                        if let Some(&Token::RPar) = iter.peek() {
                            iter.next();
                            break;
                        }
                        args.push(parse_expr(iter));
                        match iter.peek().cloned().unwrap() {
                            Token::Comma => {
                                iter.next();
                            },
                            Token::RPar => (),
                            x => return Expr::Error(format!("Expected , or ), got {:?}", x))
                        }
                    }
                    Expr::Call(name.clone(), args)
                },
                _ => Expr::Call(name.clone(), vec![parse_pow(iter)]),
            }
        },
        Token::Ident(name) => Expr::Unit(name),
        Token::Quote(name) => Expr::Quote(name),
        Token::Number(num, frac, exp) => Expr::Const(num, frac, exp),
        Token::Plus => Expr::Plus(Box::new(parse_term(iter))),
        Token::Minus => Expr::Neg(Box::new(parse_term(iter))),
        // NYI: Imaginary numbers
        Token::ImaginaryUnit => Expr::Const("0".to_owned(), None, None),
        Token::LPar => {
            let res = parse_expr(iter);
            match iter.next().unwrap() {
                Token::RPar => res,
                x => Expr::Error(format!("Expected ), got {:?}", x))
            }
        },
        Token::Date(toks) => Expr::Date(toks),
        x => Expr::Error(format!("Expected term, got {:?}", x))
    }
}

fn parse_pow(mut iter: &mut Iter) -> Expr {
    let left = parse_term(iter);
    match *iter.peek().unwrap() {
        Token::Carot => {
            iter.next();
            let right = parse_pow(iter);
            Expr::Pow(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

fn parse_mul(mut iter: &mut Iter) -> Expr {
    let mut terms = vec![parse_pow(iter)];
    loop { match iter.peek().cloned().unwrap() {
        Token::DegC | Token::DegF | Token::DegRe | Token::DegRo | Token::DegDe | Token::DegN |
        Token::Comma | Token::Equals | Token::Plus | Token::Minus | Token::DashArrow |
        Token::TriplePipe | Token::RPar | Token::Newline | Token::Comment(_) | Token::Eof => break,
        Token::Slash => {
            iter.next();
            let right = parse_pow(iter);
            let left = if terms.len() == 1 {
                terms.pop().unwrap()
            } else {
                Expr::Mul(terms)
            };
            terms = vec![Expr::Frac(Box::new(left), Box::new(right))]
        },
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

fn parse_suffix(mut iter: &mut Iter) -> Expr {
    let left = parse_mul(iter);
    match iter.peek().cloned().unwrap() {
        Token::DegC => {
            iter.next();
            Expr::Suffix(SuffixOp::Celsius, Box::new(left))
        },
        Token::DegF => {
            iter.next();
            Expr::Suffix(SuffixOp::Fahrenheit, Box::new(left))
        },
        Token::DegRe => {
            iter.next();
            Expr::Suffix(SuffixOp::Reaumur, Box::new(left))
        },
        Token::DegRo => {
            iter.next();
            Expr::Suffix(SuffixOp::Romer, Box::new(left))
        },
        Token::DegDe => {
            iter.next();
            Expr::Suffix(SuffixOp::Delisle, Box::new(left))
        },
        Token::DegN => {
            iter.next();
            Expr::Suffix(SuffixOp::Newton, Box::new(left))
        },
        _ => left
    }
}

fn parse_add(mut iter: &mut Iter) -> Expr {
    let mut left = parse_suffix(iter);
    loop { match *iter.peek().unwrap() {
        Token::Plus => {
            iter.next();
            let right = parse_suffix(iter);
            left = Expr::Add(Box::new(left), Box::new(right))
        },
        Token::Minus => {
            iter.next();
            let right = parse_suffix(iter);
            left = Expr::Sub(Box::new(left), Box::new(right))
        },
        _ => return left
    }}
}

fn parse_eq(mut iter: &mut Iter) -> Expr {
    let left = parse_add(iter);
    match iter.peek().cloned().unwrap() {
        Token::Equals => {
            iter.next();
            let right = parse_add(iter);
            Expr::Equals(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

pub fn parse_expr(mut iter: &mut Iter) -> Expr {
    match iter.peek().cloned() {
        Some(Token::Ident(ref s)) if s == "factorize" => {
            iter.next();
            return Expr::Factorize(Box::new(parse_eq(iter)))
        },
        _ => ()
    }
    let left = parse_eq(iter);
    match iter.peek().cloned().unwrap() {
        Token::DashArrow => {
            iter.next();
            let right = match iter.peek().cloned().unwrap() {
                Token::DegC => Expr::DegC,
                Token::DegF => Expr::DegF,
                Token::DegRe => Expr::DegRe,
                Token::DegRo => Expr::DegRo,
                Token::DegDe => Expr::DegDe,
                Token::DegN => Expr::DegN,
                _ => parse_eq(iter)
            };
            Expr::Convert(Box::new(left), Box::new(right))
        },
        _ => left
    }
}
