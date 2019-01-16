// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::str::Chars;
use std::iter::Peekable;
use ast::*;
use gmp::mpz::Mpz;
use gmp::mpq::Mpq;
use num::Num;
use chrono_tz::Tz;

#[derive(Debug, Clone)]
pub enum Token {
    Newline,
    Comment(usize),
    Ident(String),
    Decimal(String, Option<String>, Option<String>),
    Hex(String),
    Oct(String),
    Bin(String),
    Quote(String),
    Slash,
    Pipe,
    Semicolon,
    Equals,
    Caret,
    Eof,
    LPar,
    RPar,
    Plus,
    Minus,
    Asterisk,
    DashArrow,
    Colon,
    Date(Vec<DateToken>),
    Comma,
    DegC,
    DegF,
    DegRe,
    DegRo,
    DegDe,
    DegN,
    Percent,
    Error(String),
}

fn describe(token: &Token) -> String {
    match *token {
        Token::Newline | Token::Comment(_) => "\\n".to_owned(),
        Token::Ident(_) => "ident".to_owned(),
        Token::Decimal(_, _, _) => "number".to_owned(),
        Token::Hex(_) => "hex".to_owned(),
        Token::Oct(_) => "octal".to_owned(),
        Token::Bin(_) => "binary".to_owned(),
        Token::Quote(_) => "quote".to_owned(),
        Token::Slash => "`/`".to_owned(),
        Token::Pipe => "`|`".to_owned(),
        Token::Semicolon => "`;`".to_owned(),
        Token::Equals => "`=`".to_owned(),
        Token::Caret => "`^`".to_owned(),
        Token::Eof => "eof".to_owned(),
        Token::LPar => "`(`".to_owned(),
        Token::RPar => "`)`".to_owned(),
        Token::Plus => "`+`".to_owned(),
        Token::Minus => "`-`".to_owned(),
        Token::Asterisk => "`*`".to_owned(),
        Token::DashArrow => "`->`".to_owned(),
        Token::Colon => "`:`".to_owned(),
        Token::Date(_) => "date literal".to_owned(),
        Token::Comma => "`,`".to_owned(),
        Token::DegC => "`°C`".to_owned(),
        Token::DegF => "`°F`".to_owned(),
        Token::DegRe => "`°Ré`".to_owned(),
        Token::DegRo => "`°Rø`".to_owned(),
        Token::DegDe => "`°De`".to_owned(),
        Token::DegN => "`°N`".to_owned(),
        Token::Percent => "%".to_owned(),
        Token::Error(ref e) => format!("<{}>", e)
    }
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
            '(' => Token::LPar,
            ')' => Token::RPar,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '%' => Token::Percent,
            '=' => Token::Equals,
            '^' => Token::Caret,
            ',' => Token::Comma,
            '|' => Token::Pipe,
            ':' => Token::Colon,
            '→' => Token::DashArrow,
            '*' => if self.0.peek().cloned() == Some('*') {
                self.0.next();
                Token::Caret
            } else {
                Token::Asterisk
            },
            '-' => match self.0.peek().cloned() {
                Some('>') => {
                    self.0.next();
                    Token::DashArrow
                },
                _ => Token::Minus
            },
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
                                self.0.next();
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

                if x == '0' && self.0.peek() == Some(&'x') {
                    self.0.next();
                    let mut hex = String::new();

                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0'...'9' | 'a'...'f' | 'A'...'F' =>
                                hex.push(self.0.next().unwrap()),
                            '\u{2009}' | '_' => {
                                self.0.next();
                            },
                            _ => break
                        }
                    }
                    if hex.len() == 0 {
                        return Some(Token::Error(
                            "Malformed hexadecimal literal: No digits after 0x".to_owned()))
                    }
                    return Some(Token::Hex(hex))
                }

                if x == '0' && self.0.peek() == Some(&'o') {
                    self.0.next();
                    let mut oct = String::new();

                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0'...'7' =>
                                oct.push(self.0.next().unwrap()),
                            '\u{2009}' | '_' => {
                                self.0.next();
                            },
                            _ => break
                        }
                    }
                    if oct.len() == 0 {
                        return Some(Token::Error(
                            "Malformed octal literal: No digits after 0o".to_owned()))
                    }
                    return Some(Token::Oct(oct))
                }

                if x == '0' && self.0.peek() == Some(&'b') {
                    self.0.next();
                    let mut bin = String::new();

                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0' | '1' =>
                                bin.push(self.0.next().unwrap()),
                            '\u{2009}' | '_' => {
                                self.0.next();
                            },
                            _ => break
                        }
                    }
                    if bin.len() == 0 {
                        return Some(Token::Error(
                            "Malformed binary literal: No digits after 0b".to_owned()))
                    }
                    return Some(Token::Bin(bin))
                }

                let mut integer = String::new();
                let mut frac = None;
                let mut exp = None;

                // integer component
                if x != '.' {
                    integer.push(x);
                    while let Some(c) = self.0.peek().cloned() {
                        match c {
                            '0'...'9' => integer.push(self.0.next().unwrap()),
                            '\u{2009}' | '_' => {
                                self.0.next();
                            },
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
                            '\u{2009}' | '_' => {
                                self.0.next();
                            },
                            _ => break
                        }
                    }
                    if buf.len() == 0 {
                        return Some(Token::Error(
                            "Malformed number literal: No digits after decimal point".to_owned()))
                    }
                    frac = Some(buf)
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
                            '\u{2009}' | '_' => {
                                self.0.next();
                            },
                            _ => break
                        }
                    }
                    if buf.len() == 0 {
                        return Some(Token::Error(
                            "Malformed number literal: No digits after exponent".to_owned()))
                    }
                    exp = Some(buf)
                }
                Token::Decimal(integer, frac, exp)
            },
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
                                    if c.is_digit(10) {
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
                        x => {
                            let mut buf = String::new();
                            buf.push(x);
                            while let Some(c) = self.0.peek().cloned() {
                                if !"#:-+ ".contains(c) && !c.is_digit(10) {
                                    self.0.next();
                                    buf.push(c);
                                } else {
                                    break;
                                }
                            }
                            DateToken::Literal(buf)
                        },
                        //x => DateToken::Error(format!("Unexpected character '{}'", x))
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
            x => {
                let mut buf = String::new();
                buf.push(x);
                while let Some(c) = self.0.peek().cloned() {
                    if c.is_alphanumeric() || c == '_' || c == '$' {
                        buf.push(self.0.next().unwrap());
                    } else {
                        break;
                    }
                }
                match &*buf {
                    "degC" | "°C" | "celsius" | "℃" => Token::DegC,
                    "degF" | "°F" | "fahrenheit" | "℉" => Token::DegF,
                    "degRé" | "°Ré" | "degRe" | "°Re" | "réaumur" | "reaumur" => Token::DegRe,
                    "degRø" | "°Rø" | "degRo" | "°Ro" | "rømer" | "romer" => Token::DegRo,
                    "degDe" | "°De" | "delisle" => Token::DegDe,
                    "degN" | "°N" | "degnewton" => Token::DegN,
                    "per" => Token::Slash,
                    "to" | "in" => Token::DashArrow,
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
        "exp" => true,
        "ln" => true,
        "log" => true,
        "log2" => true,
        "log10" => true,
        "hypot" => true,
        "sin" => true,
        "cos" => true,
        "tan" => true,
        "asin" => true,
        "acos" => true,
        "atan" => true,
        "atan2" => true,
        "sinh" => true,
        "cosh" => true,
        "tanh" => true,
        "asinh" => true,
        "acosh" => true,
        "atanh" => true,
        _ => false
    }
}

fn is_attr(name: &str) -> Option<&'static str> {
    match name {
        "int" | "international" => Some("int"),
        "UKSJJ" => Some("UKSJJ"),
        "UKB" => Some("UKB"),
        "UKC" => Some("UKC"),
        "UKK" => Some("UKK"),
        "imperial" | "british" | "UK" => Some("br"),
        "survey" | "geodetic" => Some("survey"),
        "irish" => Some("irish"),
        "aust" | "australian" => Some("aust"),
        "roman" => Some("roman"),
        "egyptian" => Some("egyptian"),
        "greek" => Some("greek"),
        "olympic" => Some("olympic"),
        _ => None,
    }
}

fn parse_term(iter: &mut Iter) -> Expr {
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
                            x => return Expr::Error(format!("Expected `,` or `)`, got {}",
                                                            describe(&x)))
                        }
                    }
                    Expr::Call(name.clone(), args)
                },
                _ => Expr::Call(name.clone(), vec![parse_pow(iter)]),
            }
        },
        Token::Ident(ref attr) if is_attr(attr).is_some() => {
            match iter.peek().cloned().unwrap() {
                Token::Ident(ref name) => {
                    let attr = is_attr(attr).unwrap();
                    iter.next();
                    Expr::Unit(format!("{}{}", attr, name))
                },
                x => Expr::Error(format!("Attribute must be followed by ident, got {}",
                                         describe(&x)))
            }
        },
        Token::Ident(name) => match iter.peek().cloned().unwrap() {
            Token::Ident(ref s) if s == "of" => {
                iter.next();
                Expr::Of(name.clone(), Box::new(parse_juxt(iter)))
            },
            _ => Expr::Unit(name)
        },
        Token::Quote(name) => Expr::Quote(name),
        Token::Decimal(num, frac, exp) =>
            ::number::Number::from_parts(&*num, frac.as_ref().map(|x| &**x), exp.as_ref().map(|x| &**x))
            .map(Expr::Const)
            .unwrap_or_else(|e| Expr::Error(format!("{}", e))),
        Token::Hex(num) =>
            Mpz::from_str_radix(&*num, 16)
            .map(|x| Mpq::ratio(&x, &Mpz::one()))
            .map(Num::Mpq)
            .map(Expr::Const)
            .unwrap_or_else(|_| Expr::Error(format!("Failed to parse hex"))),
        Token::Oct(num) =>
            Mpz::from_str_radix(&*num, 8)
            .map(|x| Mpq::ratio(&x, &Mpz::one()))
            .map(Num::Mpq)
            .map(Expr::Const)
            .unwrap_or_else(|_| Expr::Error(format!("Failed to parse octal"))),
        Token::Bin(num) =>
            Mpz::from_str_radix(&*num, 2)
            .map(|x| Mpq::ratio(&x, &Mpz::one()))
            .map(Num::Mpq)
            .map(Expr::Const)
            .unwrap_or_else(|_| Expr::Error(format!("Failed to parse binary"))),
        Token::Plus => Expr::Plus(Box::new(parse_term(iter))),
        Token::Minus => Expr::Neg(Box::new(parse_term(iter))),
        Token::LPar => {
            let res = parse_expr(iter);
            match iter.next().unwrap() {
                Token::RPar => res,
                x => Expr::Error(format!("Expected `)`, got {}", describe(&x)))
            }
        },
        Token::Percent => Expr::Unit("percent".to_owned()),
        Token::Date(toks) => Expr::Date(toks),
        Token::Comment(_) => parse_term(iter),
        x => Expr::Error(format!("Expected term, got {}", describe(&x))),
    }
}

fn parse_suffix(iter: &mut Iter) -> Expr {
    let left = parse_term(iter);
    match *iter.peek().unwrap() {
        Token::Percent => {
            let mut left = left;
            while let Some(&Token::Percent) = iter.peek() {
                iter.next();
                left = Expr::Mul(vec![left, Expr::Unit("percent".to_owned())]);
            }
            left
        },
        _ => left
    }
}

fn parse_pow(iter: &mut Iter) -> Expr {
    let left = parse_suffix(iter);
    match *iter.peek().unwrap() {
        Token::Caret => {
            iter.next();
            let right = parse_pow(iter);
            Expr::Pow(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

fn parse_frac(iter: &mut Iter) -> Expr {
    let left = parse_pow(iter);
    match *iter.peek().unwrap() {
        Token::Pipe => {
            iter.next();
            let right = parse_pow(iter);
            Expr::Frac(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

fn parse_juxt(iter: &mut Iter) -> Expr {
    let mut terms = vec![parse_frac(iter)];
    loop { match iter.peek().cloned().unwrap() {
        Token::Asterisk | Token::Slash | Token::Comma | Token::Equals |
        Token::Plus | Token::Minus | Token::DashArrow |
        Token::RPar | Token::Newline |
        Token::Comment(_) | Token::Eof => break,
        Token::DegC => {
            iter.next();
            terms = vec![Expr::Suffix(SuffixOp::Celsius, Box::new(Expr::Mul(terms)))]
        },
        Token::DegF => {
            iter.next();
            terms = vec![Expr::Suffix(SuffixOp::Fahrenheit, Box::new(Expr::Mul(terms)))]
        },
        Token::DegRe => {
            iter.next();
            terms = vec![Expr::Suffix(SuffixOp::Reaumur, Box::new(Expr::Mul(terms)))]
        },
        Token::DegRo => {
            iter.next();
            terms = vec![Expr::Suffix(SuffixOp::Romer, Box::new(Expr::Mul(terms)))]
        },
        Token::DegDe => {
            iter.next();
            terms = vec![Expr::Suffix(SuffixOp::Delisle, Box::new(Expr::Mul(terms)))]
        },
        Token::DegN => {
            iter.next();
            terms = vec![Expr::Suffix(SuffixOp::Newton, Box::new(Expr::Mul(terms)))]
        },
        _ => terms.push(parse_frac(iter))
    }}
    if terms.len() == 1 {
        terms.pop().unwrap()
    } else {
        Expr::Mul(terms)
    }
}

fn parse_div(iter: &mut Iter) -> Expr {
    let mut terms = vec![parse_juxt(iter)];
    loop { match iter.peek().cloned().unwrap() {
        Token::Slash => {
            iter.next();
            let left = if terms.len() == 1 {
                terms.pop().unwrap()
            } else {
                Expr::Mul(terms.drain(..).collect())
            };
            terms = vec![Expr::Frac(Box::new(left), Box::new(parse_juxt(iter)))];
        },
        Token::Asterisk => {
            iter.next();
            terms.push(parse_juxt(iter));
        },
        _ => break
    }}
    if terms.len() == 1 {
        terms.pop().unwrap()
    } else {
        Expr::Mul(terms)
    }
}

fn parse_add(iter: &mut Iter) -> Expr {
    let mut left = parse_div(iter);
    loop { match *iter.peek().unwrap() {
        Token::Plus => {
            iter.next();
            let right = parse_div(iter);
            left = Expr::Add(Box::new(left), Box::new(right))
        },
        Token::Minus => {
            iter.next();
            let right = parse_div(iter);
            left = Expr::Sub(Box::new(left), Box::new(right))
        },
        _ => return left
    }}
}

fn parse_eq(iter: &mut Iter) -> Expr {
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

pub fn parse_expr(iter: &mut Iter) -> Expr {
    parse_eq(iter)
}

pub fn parse_unitlist(iter: &mut Iter) -> Option<Vec<String>> {
    let mut expecting_term = true;
    let mut res = vec![];
    loop { match iter.next().unwrap() {
        Token::Ident(ref ident) if expecting_term => {
            res.push(ident.clone());
            expecting_term = false;
        },
        Token::Comma | Token::Semicolon if !expecting_term => {
            expecting_term = true;
        },
        Token::Eof | Token::Newline | Token::Comment(_) if !expecting_term => break,
        _ => return None
    }}
    if res.len() > 1 {
        Some(res)
    } else {
        None
    }
}

pub fn parse_offset(iter: &mut Iter) -> Option<i64> {
    use std::str::FromStr;

    let sign = match iter.next().unwrap() {
        Token::Plus => 1,
        Token::Minus => -1,
        _ => return None
    };
    let hour = match iter.next().unwrap() {
        Token::Decimal(ref i, None, None) if i.len() == 2 => i.clone(),
        _ => return None
    };
    let _col = match iter.next().unwrap() {
        Token::Colon => (),
        _ => return None
    };
    let min = match iter.next().unwrap() {
        Token::Decimal(ref i, None, None) if i.len() == 2 => i.clone(),
        _ => return None
    };
    Some(sign * (i64::from_str(&*hour).unwrap() * 3600 + i64::from_str(&*min).unwrap() * 60))
}

pub fn parse_query(iter: &mut Iter) -> Query {
    match iter.peek().cloned() {
        Some(Token::Ident(ref s)) if s == "factorize" => {
            iter.next();
            return Query::Factorize(parse_eq(iter))
        },
        Some(Token::Ident(ref s)) if s == "units" => {
            iter.next();
            if let Some(Token::Ident(ref s)) = iter.peek().cloned() {
                if s == "for" || s == "of" {
                    iter.next();
                }
            }
            return Query::UnitsFor(parse_eq(iter))
        },
        Some(Token::Ident(ref s)) if s == "search" => {
            iter.next();
            if let Some(Token::Ident(ref s)) = iter.peek().cloned() {
                return Query::Search(s.clone())
            }
        },
        _ => ()
    }
    let left = parse_eq(iter);
    match iter.peek().cloned().unwrap() {
        Token::DashArrow => {
            use std::str::FromStr;
            iter.next();
            let mut copy = iter.clone();
            if let Some(res) = parse_unitlist(&mut copy) {
                *iter = copy;
                return Query::Convert(
                    left, Conversion::List(res), None, Digits::Default
                )
            }
            let digits = match iter.peek().cloned().unwrap() {
                Token::Ident(ref s) if s == "digits" => {
                    iter.next();
                    match iter.peek().cloned() {
                        Some(Token::Decimal(int, None, None)) => {
                            iter.next();
                            match u64::from_str_radix(&*int, 10) {
                                Ok(v) => Digits::Digits(v),
                                Err(e) => return Query::Error(format!(
                                    "Failed to parse digits: {}", e
                                ))
                            }
                        },
                        _ => Digits::FullInt,
                    }
                },
                _ => Digits::Default
            };
            let base = match iter.peek().cloned().unwrap() {
                Token::Ident(ref s) if s == "base" => {
                    iter.next();
                    match iter.next() {
                        Some(Token::Decimal(int, None, None)) => match u64::from_str_radix(&*int, 10) {
                            Ok(v) if v >= 2 && v <= 36 => {
                                Some(v as u8)
                            },
                            Ok(v) => return Query::Error(format!(
                                "Unsupported base {}, must be from 2 to 36", v)),
                            Err(e) => return Query::Error(format!(
                                "Failed to parse base: {}", e))
                        },
                        Some(x) => return Query::Error(format!(
                            "Expected decimal base, got {}", describe(&x))),
                        None => return Query::Error(format!(
                            "Expected decimal base, got eof"))
                    }
                },
                Token::Ident(ref s) if s == "hex" || s == "hexadecimal" || s == "base16" => {
                    iter.next();
                    Some(16)
                },
                Token::Ident(ref s) if s == "oct" || s == "octal" || s == "base8" => {
                    iter.next();
                    Some(8)
                },
                Token::Ident(ref s) if s == "bin" || s == "binary" || s == "base2" => {
                    iter.next();
                    Some(2)
                },
                _ => None
            };
            let right = match iter.peek().cloned().unwrap() {
                Token::Eof => Conversion::None,
                Token::DegC => Conversion::DegC,
                Token::DegF => Conversion::DegF,
                Token::DegRe => Conversion::DegRe,
                Token::DegRo => Conversion::DegRo,
                Token::DegDe => Conversion::DegDe,
                Token::DegN => Conversion::DegN,
                Token::Plus | Token::Minus => {
                    let mut old = iter.clone();
                    if let Some(off) = parse_offset(iter) {
                        Conversion::Offset(off)
                    } else {
                        Conversion::Expr(parse_eq(&mut old))
                    }
                },
                Token::Ident(ref s) if Tz::from_str(s).is_ok() => {
                    Conversion::Timezone(Tz::from_str(s).expect(
                        "Running from_str a second time failed"
                    ))
                },
                _ => Conversion::Expr(parse_eq(iter))
            };
            Query::Convert(left, right, base, digits)
        },
        _ => Query::Expr(left)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse(input: &str) -> String {
        parse_expr(&mut TokenIterator::new(input).peekable()).to_string()
    }

    #[test]
    fn add_assoc() {
        assert_eq!(parse("a + b - c + d - e"),
                   "(((a + b) - c) + d) - e");
    }

    #[test]
    fn sub_crash_regression() {
        assert_eq!(parse("-"),
                   "-<error: Expected term, got eof>");
    }

    #[test]
    fn mul_assoc() {
        assert_eq!(parse("a b * c / d / e f * g h"),
                   "(((a b) c / d) / e f) (g h)");
        assert_eq!(parse("a|b c / g e|f"),
                   "(a / b) c / g (e / f)");
        assert_eq!(parse("a / b / c"),
                   "(a / b) / c");
    }

    #[test]
    fn suffix_prec() {
        assert_eq!(parse("a b °C + x y °F"),
                   "a b °C + x y °F");
        assert_eq!(parse("a b °C c"),
                   "(a b °C) c");
        assert_eq!(parse("a °C / x"),
                   "a °C / x");
        assert_eq!(parse("a °C * x"),
                   "(a °C) x");
    }

    #[test]
    fn number_lex() {
        assert_eq!(parse("1e"),
                   "<error: Expected term, got <Malformed number literal: No digits after exponent>>");
        assert_eq!(parse("1."),
                   "<error: Expected term, got <Malformed number literal: No digits after decimal point>>");
    }

    #[test]
    fn mono_unit_list() {
        use ast::*;
        match parse_query(&mut TokenIterator::new("foo -> bar").peekable()) {
            Query::Convert(_, Conversion::Expr(_), _, _) => (),
            x => panic!("Expected Convert(_, Expr(_), _), got {:?}", x),
        }
    }

    #[test]
    fn test_of() {
        assert_eq!(parse("foo of 1 abc def / 12"),
                   "(foo of 1 abc def) / 12");
    }
}
