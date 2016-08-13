use std::str::Chars;
use std::iter::Peekable;
use std::rc::Rc;

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
    Hash,
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
                while self.0.next() == string.next() {}
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
            '#' => Token::Hash,
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
    Const(String, Option<String>, Option<String>),
    Date(Vec<Token>),
    Frac(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Plus(Box<Expr>),
    Convert(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
    Error(String),
}

#[derive(Debug, Clone)]
pub enum DatePattern {
    Literal(String),
    Match(String),
    Optional(Vec<DatePattern>),
    Dash,
    Colon,
    Error(String),
}

#[derive(Debug)]
pub enum Def {
    Dimension(String),
    Prefix(Expr),
    SPrefix(Expr),
    Unit(Expr),
    DatePattern(Vec<DatePattern>),
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
        Token::Quote(name) => Expr::Unit(name),
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
        Token::Hash => {
            let mut out = vec![];
            loop {
                match iter.next().unwrap() {
                    Token::Hash => break,
                    x => out.push(x),
                }
            }
            Expr::Date(out)
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
        Token::Equals | Token::Plus | Token::Minus | Token::DashArrow | Token::TriplePipe |
        Token::RPar | Token::Newline | Token::Comment(_) | Token::Eof => break,
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
        Token::Minus => {
            iter.next();
            let right = parse_add(iter);
            Expr::Sub(Box::new(left), Box::new(right))
        },
        _ => left
    }
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
    let left = parse_eq(iter);
    match iter.peek().cloned().unwrap() {
        Token::DashArrow => {
            iter.next();
            let right = parse_eq(iter);
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

fn parse_datepattern(mut iter: &mut Iter) -> Vec<DatePattern> {
    let mut out = vec![];
    loop {
        match iter.peek().cloned().unwrap() {
            Token::Newline | Token::Comment(_) | Token::Eof | Token::RBrack => break,
            Token::Ident(name) => out.push(DatePattern::Match(name)),
            Token::Quote(name) => out.push(DatePattern::Literal(name)),
            Token::Minus => out.push(DatePattern::Dash),
            Token::Colon => out.push(DatePattern::Colon),
            Token::LBrack => {
                iter.next();
                let res = DatePattern::Optional(parse_datepattern(iter));
                let res = match iter.next().unwrap() {
                    Token::RBrack => res,
                    x => DatePattern::Error(format!("Expected ], got {:?}", x))
                };
                out.push(res)
            },
            x => out.push(DatePattern::Error(format!("Unexpected token {:?} in date pattern", x)))
        }
        iter.next();
    }
    out
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
            Token::Ident(ref name) if name == "datepattern" =>
                map.push((name.clone(), Rc::new(Def::DatePattern(parse_datepattern(iter))))),
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
