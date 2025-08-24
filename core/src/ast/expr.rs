// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::output::fmt::{Span, TokenFmt};

use super::*;
use serde_derive::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "type")]
pub enum Expr {
    Unit { name: String },
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

fn to_spans_impl<'a>(out: &mut Vec<Span<'a>>, expr: &'a Expr, prec: Precedence) {
    match *expr {
        Expr::Unit { ref name } => out.push(Span::unit(name)),
        Expr::Quote { ref string } => out.push(Span::unit(format!("'{}'", string))),
        Expr::Const { ref value } => {
            let (_exact, val) = value.to_string(10, Digits::Default);
            out.push(Span::number(format!("{}", val)))
        }
        Expr::Date { .. } => todo!("NYI: date expr to_spans"),
        Expr::BinOp(ref binop) => {
            let op_prec = Precedence::from(binop.op);
            let succ = Precedence::next(binop.op);
            if prec < op_prec {
                out.push(Span::plain("("));
            }
            to_spans_impl(out, &binop.left, succ);
            out.push(Span::plain(binop.op.symbol()));
            to_spans_impl(out, &binop.right, op_prec);
            if prec < op_prec {
                out.push(Span::plain(")"));
            }
        }
        Expr::UnaryOp(ref unaryop) => match unaryop.op {
            UnaryOpType::Positive => {
                out.push(Span::plain("+"));
                to_spans_impl(out, &unaryop.expr, Precedence::Plus);
            }
            UnaryOpType::Negative => {
                out.push(Span::plain("-"));
                to_spans_impl(out, &unaryop.expr, Precedence::Plus);
            }
            UnaryOpType::Degree(ref suffix) => {
                if prec < Precedence::Mul {
                    out.push(Span::plain("("));
                }
                to_spans_impl(out, &unaryop.expr, Precedence::Mul);
                out.push(Span::plain(" "));
                out.push(Span::plain(suffix.as_str()));
                if prec < Precedence::Mul {
                    out.push(Span::plain(")"));
                }
            }
        },
        Expr::Mul { ref exprs } => {
            if prec < Precedence::Mul {
                out.push(Span::plain("("));
            }
            if let Some(first) = exprs.first() {
                to_spans_impl(out, first, Precedence::Pow);
            }
            for expr in exprs.iter().skip(1) {
                out.push(Span::plain(" "));
                to_spans_impl(out, expr, Precedence::Pow);
            }
            if prec < Precedence::Mul {
                out.push(Span::plain(")"));
            }
        }
        Expr::Call { ref func, ref args } => {
            out.push(Span::plain(func.name())); // TODO: keyword
            out.push(Span::plain("("));
            if let Some(first) = args.first() {
                to_spans_impl(out, first, Precedence::Equals);
            }
            for arg in args.iter().skip(1) {
                out.push(Span::plain(", "));
                to_spans_impl(out, arg, Precedence::Equals);
            }
            out.push(Span::plain(")"))
        }
        Expr::Of {
            ref property,
            ref expr,
        } => {
            if prec < Precedence::Add {
                out.push(Span::plain("("));
            }
            out.push(Span::prop_name(property));
            out.push(Span::plain(" "));
            out.push(Span::keyword("of"));
            out.push(Span::plain(" "));
            to_spans_impl(out, expr, Precedence::Div);
            if prec < Precedence::Add {
                out.push(Span::plain(")"));
            }
        }
        Expr::Error { ref message } => {
            out.push(Span::error("<error: "));
            out.push(Span::error(message));
            out.push(Span::error(">"));
        }
    }
}

impl<'a> TokenFmt<'a> for Expr {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut res = vec![];
        to_spans_impl(&mut res, self, Precedence::Equals);
        res
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.spans_to_string())
    }
}

impl From<i64> for Expr {
    fn from(x: i64) -> Self {
        Expr::new_const(x.into())
    }
}
