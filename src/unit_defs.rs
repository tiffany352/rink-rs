use std::collections::HashMap;
use std::str::Chars;
use std::iter::Peekable;
use std::str::FromStr;
use std::rc::Rc;

#[derive(Debug)]
pub enum Token {
    Newline,
    Comment,
    Ident(String),
    Number(f64),
    Slash,
    ColonDash,
    DColonDash,
    TriplePipe,
    ColonEq,
    EqBangEq,
    Carot,
    Eof,
    Error,
}

pub type Iter<'a> = Peekable<Chars<'a>>;

fn token(mut iter: &mut Iter) -> Token {
    if iter.peek() == None {
        return Token::Eof
    }
    match iter.next().unwrap() {
        ' ' | '\t' => token(iter),
        '\n' => Token::Newline,
        '/' => match iter.peek() {
            Some(&'/') => loop {
                match iter.next() {
                    Some('\n') => return Token::Comment,
                    _ => ()
                }
            },
            Some(&'*') => loop {
                if let Some('*') = iter.next() {
                    if let Some(&'/') = iter.peek() {
                        return Token::Comment
                    }
                }
                if iter.peek() == None {
                    return Token::Error
                }
            },
            _ => Token::Slash
        },
        x @ '0'...'9' => {
            let mut buf = String::new();
            buf.push(x);
            while let Some(c) = iter.peek().cloned() {
                match c {
                    '0'...'9' | 'e' | 'E' | '.' => buf.push(iter.next().unwrap()),
                    _ => break
                }
            }
            FromStr::from_str(&*buf).map(|x| Token::Number(x)).unwrap_or(Token::Error)
        },
        ':' => match iter.next() {
            Some(':') => match iter.next() {
                Some('-') => Token::DColonDash,
                _ => Token::Error
            },
            Some('-') => Token::ColonDash,
            Some('=') => Token::ColonEq,
            _ => Token::Error
        },
        '|' => if let Some('|') = iter.next() {
            if let Some('|') = iter.next() {
                Token::TriplePipe
            } else {
                Token::Error
            }
        } else {
            Token::Error
        },
        '=' => if let Some('!') = iter.next() {
            if let Some('=') = iter.next() {
                Token::EqBangEq
            } else {
                Token::Error
            }
        } else {
            Token::Error
        },
        '^' => Token::Carot,
        x => {
            let mut buf = String::new();
            buf.push(x);
            while let Some(c) = iter.peek().cloned() {
                match c {
                    ' ' | '\t' | '\n' | '/' | '^' | ':' | '=' | '-' | '0'...'9' => break,
                    _ => buf.push(iter.next().unwrap())
                }
            }
            Token::Ident(buf)
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Unit(String),
    Const(f64),
    Frac(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Pow(Box<Expr>, f64),
    Error(String),
}

#[derive(Debug)]
pub enum Def {
    Prefix(Expr),
    SPrefix(Expr),
    Dimension(String),
    Unit(Expr),
    Error(String),
}

fn parse_term(mut iter: &mut Iter) -> Expr {
    match token(iter) {
        Token::Ident(name) => Expr::Unit(name),
        Token::Number(num) => Expr::Const(num),
        x => Expr::Error(format!("Expected term, got {:?}", x))
    }
}

fn parse_pow(mut iter: &mut Iter) -> Expr {
    let left = parse_term(iter);
    let mut copy = iter.clone();
    match token(&mut copy) {
        Token::Carot => {
            token(iter);
            let right = parse_pow(iter);
            match right {
                Expr::Const(n) => Expr::Pow(Box::new(left), n),
                x => Expr::Error(format!("NYI: Pow requires const, got {:?}", x)) //unimplemented!()
            }
        },
        _ => left
    }
}

fn parse_frac(mut iter: &mut Iter) -> Expr {
    let left = parse_pow(iter);
    let mut copy = iter.clone();
    match token(&mut copy) {
        Token::Slash => {
            token(iter);
            let right = parse_frac(iter);
            Expr::Frac(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

fn parse_expr(mut iter: &mut Iter) -> Expr {
    let mut terms = vec![];
    loop {
        let mut copy = iter.clone();
        match token(&mut copy) {
            Token::Newline | Token::Comment | Token::Eof => {iter.next(); break},
            _ => terms.push(parse_frac(iter))
        }
    }
    if terms.len() == 1 {
        terms.pop().unwrap()
    } else {
        Expr::Mul(terms)
    }
}

pub fn parse(mut iter: &mut Iter) -> HashMap<String, Rc<Def>> {
    let mut map = HashMap::new();
    loop {
        match token(iter) {
            Token::Newline => {token(iter); continue},
            Token::Comment => {token(iter); continue},
            Token::Eof => break,
            Token::Ident(name) => {
                let def = match token(iter) {
                    Token::ColonDash => Def::Prefix(parse_expr(iter)),
                    Token::DColonDash => Def::SPrefix(parse_expr(iter)),
                    Token::EqBangEq => match token(iter) {
                        Token::Ident(val) => Def::Dimension(val),
                        _ => Def::Error(format!("Malformed dimensionless unit"))
                    },
                    Token::ColonEq => Def::Unit(parse_expr(iter)),
                    _ => {
                        loop {
                            match token(iter) {
                                Token::Newline | Token::Comment | Token::Eof => break,
                                _ => ()
                            }
                        }
                        Def::Error(format!("Unknown definition"))
                    }
                };
                map.insert(name.clone(), Rc::new(def));
            },
            _ => ()
        };
    }
    map
}

pub fn tokens(mut iter: &mut Iter) -> Vec<Token> {
    let mut out = vec![];
    loop {
        match token(iter) {
            Token::Eof => break,
            x => out.push(x)
        }
    }
    out
}
