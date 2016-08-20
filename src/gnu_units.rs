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

/*fn parse_term(mut iter: &mut Iter) -> Expr {
    match iter.next().unwrap() {
        Token::Ident(name) => Expr::Unit(name),
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
                    Token::Eof | Token::Comment(_) | Token::Newline =>
                        return Expr::Error(format!("Unterminated date literal")),
                    x => out.push(x),
                }
            }
            unimplemented!()
            //Expr::Date(out)
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
    let left = parse_suffix(iter);
    match *iter.peek().unwrap() {
        Token::Plus => {
            iter.next();
            let right = parse_suffix(iter);
            Expr::Add(Box::new(left), Box::new(right))
        },
        Token::Minus => {
            iter.next();
            let right = parse_suffix(iter);
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
*/
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
