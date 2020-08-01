use super::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
pub enum Expr {
    Unit(String),
    Quote(String),
    #[serde(skip_deserializing)]
    Const(Numeric),
    Date(Vec<DateToken>),
    BinOp(BinOpExpr),
    Mul(Vec<Expr>),
    Neg(Box<Expr>),
    Plus(Box<Expr>),
    Suffix {
        suffix: Degree,
        expr: Box<Expr>,
    },
    Of {
        property: String,
        expr: Box<Expr>,
    },
    Call {
        func: Function,
        args: Vec<Expr>,
    },
    Error(String),
}

impl Expr {
    pub fn new_call(func: Function, args: Vec<Expr>) -> Expr {
        Expr::Call { func, args }
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

    pub fn new_suffix(suffix: Degree, expr: Expr) -> Expr {
        let expr = Box::new(expr);
        Expr::Suffix { suffix, expr }
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
                Expr::Unit(ref name) => write!(fmt, "{}", name),
                Expr::Quote(ref name) => write!(fmt, "'{}'", name),
                Expr::Const(ref num) => {
                    let (_exact, val) = crate::number::to_string(num, 10, Digits::Default);
                    write!(fmt, "{}", val)
                }
                Expr::Date(ref _date) => write!(fmt, "NYI: date expr Display"),
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
                Expr::Mul(ref exprs) => {
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
                Expr::Plus(ref expr) => {
                    write!(fmt, "+")?;
                    recurse(expr, fmt, Precedence::Plus)
                }
                Expr::Neg(ref expr) => {
                    write!(fmt, "-")?;
                    recurse(expr, fmt, Precedence::Plus)
                }
                Expr::Suffix {
                    ref suffix,
                    ref expr,
                } => {
                    if prec < Precedence::Mul {
                        write!(fmt, "(")?;
                    }
                    recurse(expr, fmt, Precedence::Mul)?;
                    write!(fmt, " {}", suffix)?;
                    if prec < Precedence::Mul {
                        write!(fmt, ")")?;
                    }
                    Ok(())
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
                Expr::Error(ref err) => write!(fmt, "<error: {}>", err),
            }
        }

        recurse(self, fmt, Precedence::Equals)
    }
}
