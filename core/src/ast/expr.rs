use super::*;

#[derive(Debug, Clone, Serialize)]
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

    pub fn new_mul(exprs: Vec<Expr>) -> Expr {
        Expr::Mul { exprs }
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
            BinOpType::Equals => Precedence::Equals,
        }
    }

    pub fn next(binop_type: BinOpType) -> Precedence {
        match binop_type {
            BinOpType::Add => Precedence::Div,
            BinOpType::Sub => Precedence::Div,
            BinOpType::Pow => Precedence::Term,
            BinOpType::Frac => Precedence::Mul,
            BinOpType::Equals => Precedence::Add,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn recurse(expr: &Expr, fmt: &mut fmt::Formatter<'_>, prec: Precedence) -> fmt::Result {
            match *expr {
                Expr::Unit { ref name } => write!(fmt, "{}", name),
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
