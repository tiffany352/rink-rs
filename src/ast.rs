use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum SuffixOp {
    Celsius,
    Fahrenheit,
    Reaumur,
    Romer,
    Delisle,
    Newton,
}

#[derive(Debug, Clone)]
pub enum DateToken {
    Literal(String),
    Number(String, Option<String>, Option<String>),
    Colon,
    Dash,
    Space,
    Plus,
    Error(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit(String),
    Quote(String),
    Const(String, Option<String>, Option<String>),
    Date(Vec<DateToken>),
    Frac(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Plus(Box<Expr>),
    Convert(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
    Suffix(SuffixOp, Box<Expr>),
    Call(String, Vec<Expr>),
    Factorize(Box<Expr>),
    DegC,
    DegF,
    DegRe,
    DegRo,
    DegDe,
    DegN,
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
    Quantity(Expr),
    DatePattern(Vec<DatePattern>),
    Error(String),
}

#[derive(Debug)]
pub struct Defs {
    pub defs: Vec<(String, Rc<Def>)>,
}
