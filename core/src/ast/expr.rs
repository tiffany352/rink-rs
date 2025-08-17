// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::*;
use serde_derive::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "type")]
pub enum Expr {
    Unit { name: String },
    Dependency { name: String },
    Quote { string: String },
    Const { value: Numeric },
    Date { tokens: Vec<DateToken> },
    BinOp(BinOpExpr),
    UnaryOp(UnaryOpExpr),
    Mul { exprs: Vec<Expr> },
    Of { property: String, expr: Box<Expr> },
    Call { func: Function, args: Vec<Expr> },
    Error { message: String },
}

impl Expr {
    pub fn new_const(value: Numeric) -> Expr {
        Expr::Const { value }
    }

    pub fn new_error(message: String) -> Expr {
        Expr::Error { message }
    }

    pub fn new_unit(name: String) -> Expr {
        Expr::Unit { name }
    }

    pub fn new_dependency(name: String) -> Expr {
        Expr::Dependency { name }
    }

    pub fn new_call(func: Function, args: Vec<Expr>) -> Expr {
        Expr::Call { func, args }
    }

    pub fn new_mul(mut exprs: Vec<Expr>) -> Expr {
        if exprs.len() == 1 {
            exprs.pop().unwrap()
        } else {
            Expr::Mul { exprs }
        }
    }

    pub fn new_bin(op: BinOpType, numer: Expr, denom: Expr) -> Expr {
        let left = Box::new(numer);
        let right = Box::new(denom);
        Expr::BinOp(BinOpExpr { op, left, right })
    }

    pub fn new_add(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Add, numer, denom)
    }

    pub fn new_sub(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Sub, numer, denom)
    }

    pub fn new_frac(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Frac, numer, denom)
    }

    pub fn new_pow(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Pow, numer, denom)
    }

    pub fn new_equals(numer: Expr, denom: Expr) -> Expr {
        Expr::new_bin(BinOpType::Equals, numer, denom)
    }

    pub fn new_of(property: &str, expr: Expr) -> Expr {
        let property = property.to_owned();
        let expr = Box::new(expr);
        Expr::Of { property, expr }
    }

    pub fn new_unary(op: UnaryOpType, expr: Expr) -> Expr {
        let expr = Box::new(expr);
        Expr::UnaryOp(UnaryOpExpr { op, expr })
    }

    pub fn new_suffix(suffix: Degree, expr: Expr) -> Expr {
        let op = UnaryOpType::Degree(suffix);
        Expr::new_unary(op, expr)
    }

    pub fn new_plus(expr: Expr) -> Expr {
        Expr::new_unary(UnaryOpType::Positive, expr)
    }

    pub fn new_negate(expr: Expr) -> Expr {
        Expr::new_unary(UnaryOpType::Negative, expr)
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub enum Precedence {
    Term,
    Plus,
    Pow,
    Mul,
    Div,
    Add,
    Equals,
}

impl Precedence {
    pub fn from(binop_type: BinOpType) -> Precedence {
        match binop_type {
            BinOpType::Add => Precedence::Add,
            BinOpType::Sub => Precedence::Add,
            BinOpType::Pow => Precedence::Pow,
            BinOpType::Frac => Precedence::Div,
            BinOpType::ShiftL => Precedence::Div,
            BinOpType::ShiftR => Precedence::Div,
            BinOpType::Mod => Precedence::Div,
            BinOpType::And => Precedence::Div,
            BinOpType::Or => Precedence::Div,
            BinOpType::Xor => Precedence::Div,
            BinOpType::Equals => Precedence::Equals,
        }
    }

    pub fn next(binop_type: BinOpType) -> Precedence {
        match binop_type {
            BinOpType::Add => Precedence::Div,
            BinOpType::Sub => Precedence::Div,
            BinOpType::Pow => Precedence::Term,
            BinOpType::Frac => Precedence::Mul,
            BinOpType::ShiftL => Precedence::Mul,
            BinOpType::ShiftR => Precedence::Mul,
            BinOpType::Mod => Precedence::Mul,
            BinOpType::And => Precedence::Mul,
            BinOpType::Or => Precedence::Mul,
            BinOpType::Xor => Precedence::Mul,
            BinOpType::Equals => Precedence::Add,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn recurse(expr: &Expr, fmt: &mut fmt::Formatter<'_>, prec: Precedence) -> fmt::Result {
            match *expr {
                Expr::Unit { ref name } => write!(fmt, "{}", name),
                Expr::Dependency { ref name } => write!(fmt, "dependency {}", name),
                Expr::Quote { ref string } => write!(fmt, "'{}'", string),
                Expr::Const { ref value } => {
                    let (_exact, val) = value.to_string(10, Digits::Default);
                    write!(fmt, "{}", val)
                }
                Expr::Date { .. } => write!(fmt, "NYI: date expr Display"),
                Expr::BinOp(ref binop) => {
                    let op_prec = Precedence::from(binop.op);
                    let succ = Precedence::next(binop.op);
                    if prec < op_prec {
                        write!(fmt, "(")?;
                    }
                    recurse(&binop.left, fmt, succ)?;
                    write!(fmt, "{}", binop.op.symbol())?;
                    recurse(&binop.right, fmt, op_prec)?;
                    if prec < op_prec {
                        write!(fmt, ")")?;
                    }
                    Ok(())
                }
                Expr::UnaryOp(ref unaryop) => match unaryop.op {
                    UnaryOpType::Positive => {
                        write!(fmt, "+")?;
                        recurse(&unaryop.expr, fmt, Precedence::Plus)
                    }
                    UnaryOpType::Negative => {
                        write!(fmt, "-")?;
                        recurse(&unaryop.expr, fmt, Precedence::Plus)
                    }
                    UnaryOpType::Degree(ref suffix) => {
                        if prec < Precedence::Mul {
                            write!(fmt, "(")?;
                        }
                        recurse(&unaryop.expr, fmt, Precedence::Mul)?;
                        write!(fmt, " {}", suffix)?;
                        if prec < Precedence::Mul {
                            write!(fmt, ")")?;
                        }
                        Ok(())
                    }
                },
                Expr::Mul { ref exprs } => {
                    if prec < Precedence::Mul {
                        write!(fmt, "(")?;
                    }
                    if let Some(first) = exprs.first() {
                        recurse(first, fmt, Precedence::Pow)?;
                    }
                    for expr in exprs.iter().skip(1) {
                        write!(fmt, " ")?;
                        recurse(expr, fmt, Precedence::Pow)?;
                    }
                    if prec < Precedence::Mul {
                        write!(fmt, ")")?;
                    }
                    Ok(())
                }
                Expr::Call { ref func, ref args } => {
                    write!(fmt, "{}(", func.name())?;
                    if let Some(first) = args.first() {
                        recurse(first, fmt, Precedence::Equals)?;
                    }
                    for arg in args.iter().skip(1) {
                        write!(fmt, ", ")?;
                        recurse(arg, fmt, Precedence::Equals)?;
                    }
                    write!(fmt, ")")
                }
                Expr::Of {
                    ref property,
                    ref expr,
                } => {
                    if prec < Precedence::Add {
                        write!(fmt, "(")?;
                    }
                    write!(fmt, "{} of ", property)?;
                    recurse(expr, fmt, Precedence::Div)?;
                    if prec < Precedence::Add {
                        write!(fmt, ")")?;
                    }
                    Ok(())
                }
                Expr::Error { ref message } => write!(fmt, "<error: {}>", message),
            }
        }

        recurse(self, fmt, Precedence::Equals)
    }
}

impl From<i64> for Expr {
    fn from(x: i64) -> Self {
        Expr::new_const(x.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::text_query::{parse_expr, TokenIterator};

    fn parse_then_pretty(input: &str) -> String {
        let mut iter = TokenIterator::new(&input).peekable();
        let expr = parse_expr(&mut iter);
        expr.to_string()
    }

    #[test]
    fn expr_display() {
        assert_eq!(parse_then_pretty("meter"), "meter");
        assert_eq!(parse_then_pretty("'hello world'"), "'hello world'");
        assert_eq!(parse_then_pretty("234234"), "234234");
        assert_eq!(parse_then_pretty("1 + 2"), "1 + 2");
        assert_eq!(parse_then_pretty("speed of light"), "speed of light");
        assert_eq!(parse_then_pretty("1 + 2 * 3"), "1 + 2 3");
        assert_eq!(parse_then_pretty("(1 + 2) * 3"), "(1 + 2) 3");
        assert_eq!(parse_then_pretty("a = 2"), "a = 2");
    }
}
