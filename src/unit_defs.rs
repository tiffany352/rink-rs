use std::str::Chars;
use std::iter::Peekable;
use std::str::FromStr;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Token {
    Newline,
    Comment(usize),
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
    ImaginaryUnit,
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
            '-' => match self.0.peek().cloned().unwrap() {
                '>' => {
                    self.0.next();
                    Token::DashArrow
                },
                _ => Token::Minus
            },
            '*' => Token::Asterisk,
            '/' => match self.0.peek() {
                Some(&'/') => loop {
                    match self.0.next() {
                        Some('\n') => return Some(Token::Comment(1)),
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
                let mut buf = String::new();
                buf.push(x);
                while let Some(c) = self.0.peek().cloned() {
                    match c {
                        'e' | 'E' => {
                            buf.push(self.0.next().unwrap());
                            loop {
                                match self.0.peek().cloned() {
                                    Some('e') | Some('E') => self.0.next(),
                                    _ => break
                                };
                            }
                        },
                        '0'...'9' | '.' | '-' | '+' => buf.push(self.0.next().unwrap()),
                        _ => break
                    }
                }
                FromStr::from_str(&*buf).map(|x| Token::Number(x))
                    .unwrap_or(Token::Error(format!("Invalid number literal: `{}`", buf)))
            },
            ':' => match self.0.next() {
                Some(':') => match self.0.next() {
                    Some('-') => Token::DColonDash,
                    x => Token::Error(format!("Unexpected {:?}", x)),
                },
                Some('-') => Token::ColonDash,
                Some('=') => Token::ColonEq,
                x => Token::Error(format!("Unexpected {:?}", x))
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
            '=' => if let Some('!') = self.0.next() {
                if let Some('=') = self.0.next() {
                    Token::EqBangEq
                } else {
                    Token::Error(format!("Unknown symbol"))
                }
            } else {
                Token::Error(format!("Unknown symbol"))
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
                while self.0.next() == string.next() {}
                if string.next() == None {
                    Token::ImaginaryUnit
                } else {
                    Token::Error(format!("Unexpected <"))
                }
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
                Token::Ident(buf)
            }
        };
        Some(res)
    }
}

pub type Iter<'a> = Peekable<TokenIterator<'a>>;

#[derive(Debug, Clone)]
pub enum Expr {
    Unit(String),
    Const(f64),
    Frac(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Plus(Box<Expr>),
    Convert(Box<Expr>, Box<Expr>),
    Error(String),
}

#[derive(Debug)]
pub enum Def {
    Dimension(String),
    Prefix(Expr),
    SPrefix(Expr),
    Unit(Expr),
    Error(String),
}

#[derive(Debug)]
pub struct Defs {
    pub defs: Vec<(String, Rc<Def>)>,
    pub aliases: Vec<(Expr, String)>,
}

fn parse_term(mut iter: &mut Iter) -> Expr {
    match iter.next().unwrap() {
        Token::Ident(name) => Expr::Unit(name),
        Token::Number(num) => Expr::Const(num),
        Token::Plus => Expr::Plus(Box::new(parse_term(iter))),
        Token::Minus => Expr::Neg(Box::new(parse_term(iter))),
        // NYI: Imaginary numbers
        Token::ImaginaryUnit => Expr::Const(0.0),
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
    loop { match *iter.peek().unwrap() {
        Token::Plus | Token::DashArrow | Token::TriplePipe | Token::RPar | Token::Newline |
        Token::Comment(_) | Token::Eof => break,
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

fn parse_add(mut iter: &mut Iter) -> Expr {
    let left = parse_mul(iter);
    match *iter.peek().unwrap() {
        Token::Plus => {
            iter.next();
            let right = parse_add(iter);
            Expr::Add(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

pub fn parse_expr(mut iter: &mut Iter) -> Expr {
    let left = parse_add(iter);
    match iter.peek().cloned().unwrap() {
        Token::DashArrow => {
            iter.next();
            let right = parse_add(iter);
            Expr::Convert(Box::new(left), Box::new(right))
        },
        _ => left
    }
}

fn parse_unknown(mut iter: &mut Iter) {
    loop {
        match iter.peek().cloned().unwrap() {
            Token::Newline | Token::Comment(_) | Token::Eof => break,
            _ => iter.next()
        };
    }
}

fn parse_alias(mut iter: &mut Iter) -> Option<(Expr, String)> {
    match *iter.peek().unwrap() {
        Token::Newline | Token::Comment(_) | Token::Eof => return None,
        _ => ()
    }
    let expr = parse_expr(iter);
    match iter.next().unwrap() {
        Token::TriplePipe => (),
        _ => return None
    };
    let name = match iter.next().unwrap() {
        Token::Ident(name) => name,
        _ => return None
    };
    match iter.peek().cloned().unwrap() {
        Token::Newline | Token::Comment(_) | Token::Eof => (),
        _ => return None
    };
    Some((expr, name))
}

pub fn parse(mut iter: &mut Iter) -> Defs {
    let mut map = vec![];
    let mut aliases = vec![];
    let mut line = 1;
    loop {
        let mut copy = iter.clone();
        if let Some(a) = parse_alias(&mut copy) {
            aliases.push(a);
            *iter = copy;
            continue
        }
        match iter.next().unwrap() {
            Token::Newline => line += 1,
            Token::Comment(lines) => line += lines,
            Token::Eof => break,
            Token::Ident(name) => {
                let def = match iter.next().unwrap() {
                    Token::ColonDash => {
                        let expr = parse_expr(iter);
                        Def::Prefix(expr)
                    },
                    Token::DColonDash => {
                        let expr = parse_expr(iter);
                        Def::SPrefix(expr)
                    },
                    Token::EqBangEq => match iter.next().unwrap() {
                        Token::Ident(val) => Def::Dimension(val),
                        _ => Def::Error(format!("Line {}: Malformed dimensionless unit", line))
                    },
                    Token::ColonEq => Def::Unit(parse_expr(iter)),
                    Token::LBrack => {
                        // NYI
                        let mut n = 0;
                        let mut first = true;
                        loop {
                            match iter.peek().cloned().unwrap() {
                                Token::LBrace => n += 1,
                                Token::RBrace if n == 1 => {
                                    iter.next();
                                    break
                                },
                                Token::RBrace => n -= 1,
                                Token::Newline | Token::Comment(_) if !first && n == 0 => break,
                                Token::Newline if first => first = false,
                                Token::Eof => break,
                                Token::Comment(lines) => line += lines,
                                _ => ()
                            }
                            iter.next();
                        }
                        continue
                        //Def::Error(format!("NYI: functions"))
                    }
                    _ => {
                        parse_unknown(iter);
                        Def::Error(format!("Line {}: Unknown definition", line))
                    }
                };
                map.push((name.clone(), Rc::new(def)));
            },
            _ => parse_unknown(iter)
        };
    }
    Defs {
        defs: map,
        aliases: aliases,
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
