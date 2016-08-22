use std::str::Chars;
use std::iter::Peekable;
use std::rc::Rc;
use ast::*;

#[derive(Debug, Clone)]
pub enum Token {
    Eof,
    Newline,
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
            '?' => Token::Question,
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
        Token::Ident(name) => Expr::Unit(name),
        Token::Number(num, frac, exp) => Expr::Const(num, frac, exp),
        Token::Plus => Expr::Plus(Box::new(parse_term(iter))),
        Token::Dash => Expr::Neg(Box::new(parse_term(iter))),
        Token::Slash => Expr::Frac(
            Box::new(Expr::Const("1".to_owned(), None, None)),
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
            Token::Ident(name) => {
                if name.ends_with("-") {
                    // prefix
                    let expr = parse_expr(iter);
                    let mut name = name;
                    name.pop();
                    if name.len() > 1 {
                        map.push((name, Rc::new(Def::SPrefix(expr))));
                    } else {
                        map.push((name, Rc::new(Def::Prefix(expr))));
                    }
                } else {
                    // unit
                    if let Some(&Token::Bang) = iter.peek() {
                        // dimension
                        iter.next();
                        if let Some(Token::Ident(ref _n)) = iter.peek().cloned() {
                            iter.next();
                            // dimensionless primitive unit
                            map.push((name.clone(), Rc::new(Def::Dimension(name))));
                        } else {
                            map.push((name.clone(), Rc::new(Def::Dimension(name))));
                        }
                    } else if let Some(&Token::Question) = iter.peek() {
                        // quantity
                        iter.next();
                        let expr = parse_expr(iter);
                        map.push((name, Rc::new(Def::Quantity(expr))));
                    } else {
                        // derived
                        let expr = parse_expr(iter);
                        map.push((name, Rc::new(Def::Unit(expr))));
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
