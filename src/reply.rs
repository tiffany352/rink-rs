use number::NumberParts;
use std::convert::From;
use std::rc::Rc;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::fmt::Result as FmtResult;
use chrono::{DateTime, TimeZone};
use std::iter::once;
use ast::{Expr, Digits};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub enum ExprParts {
    Literal(String),
    Unit(String),
    Property(String, Vec<ExprParts>),
    Error(String),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct ExprReply {
    exprs: Vec<ExprParts>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct DefReply {
    pub canon_name: String,
    pub def: Option<String>,
    pub def_expr: Option<ExprReply>,
    pub value: Option<NumberParts>,
    pub doc: Option<String>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct ConversionReply {
    pub value: NumberParts,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct FactorizeReply {
    pub factorizations: Vec<BTreeMap<Rc<String>, usize>>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct UnitsInCategory {
    pub category: Option<String>,
    pub units: Vec<String>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct UnitsForReply {
    pub units: Vec<UnitsInCategory>,
    /// Dimensions and quantity are set.
    pub of: NumberParts,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct UnitListReply {
    pub rest: NumberParts,
    pub list: Vec<NumberParts>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct DurationReply {
    pub raw: NumberParts,
    pub years: NumberParts,
    pub months: NumberParts,
    pub weeks: NumberParts,
    pub days: NumberParts,
    pub hours: NumberParts,
    pub minutes: NumberParts,
    pub seconds: NumberParts,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct SearchReply {
    pub results: Vec<NumberParts>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct PropertyReply {
    pub name: String,
    pub value: NumberParts,
    pub doc: Option<String>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct SubstanceReply {
    pub name: String,
    pub doc: Option<String>,
    pub amount: NumberParts,
    pub properties: Vec<PropertyReply>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct DateReply {
    pub year: i32,
    pub month: i32,
    pub day: i32,
    pub hour: i32,
    pub minute: i32,
    pub second: i32,
    pub nanosecond: i32,
    /// chrono-humanize output, if enabled.
    pub human: Option<String>,
    pub string: String,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub enum QueryReply {
    Number(NumberParts),
    Date(DateReply),
    Substance(SubstanceReply),
    Duration(DurationReply),
    Def(DefReply),
    Conversion(ConversionReply),
    Factorize(FactorizeReply),
    UnitsFor(UnitsForReply),
    UnitList(UnitListReply),
    Search(SearchReply),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct ConformanceError {
    pub left: NumberParts,
    pub right: NumberParts,
    pub suggestions: Vec<String>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub struct NotFoundError {
    pub got: String,
    pub suggestion: Option<String>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "nightly", derive(Serialize, Deserialize))]
pub enum QueryError {
    Conformance(ConformanceError),
    NotFound(NotFoundError),
    Generic(String),
}

impl ExprReply {
    pub fn from(expr: &Expr) -> ExprReply {
        let mut parts = vec![];

        #[derive(PartialOrd, Ord, PartialEq, Eq)]
        enum Prec {
            Term, Plus, Pow, Mul, Div, Add, Equals
        }

        fn recurse(expr: &Expr, parts: &mut Vec<ExprParts>, prec: Prec) {
            macro_rules! literal {
                ($e:expr) => {{
                    parts.push(ExprParts::Literal($e.to_owned()))
                }}
            }
            macro_rules! binop {
                ($left:expr, $right:expr, $prec:expr, $succ:expr, $sym:expr) => {{
                    if prec < $prec {
                        literal!("(");
                    }
                    recurse($left, parts, $succ);
                    literal!($sym);
                    recurse($right, parts, $prec);
                    if prec < $prec {
                        literal!(")");
                    }
                }}
            }
            match *expr {
                Expr::Unit(ref name) => parts.push(ExprParts::Unit(name.clone())),
                Expr::Quote(ref name) => literal!(format!("'{}'", name)),
                Expr::Const(ref num) => {
                    let (_exact, val) = ::number::to_string(num, 10, Digits::Default);
                    literal!(val.to_string())
                },
                Expr::Date(ref _date) => literal!("NYI: date expr to expr parts"),
                Expr::Mul(ref exprs) => {
                    if prec < Prec::Mul {
                        literal!("(");
                    }
                    for expr in exprs.iter() {
                        recurse(expr, parts, Prec::Pow);
                    }
                    if prec < Prec::Mul {
                        literal!(")");
                    }
                },
                Expr::Call(ref func, ref args) => {
                    literal!(format!("{}(", func.name()));
                    if let Some(first) = args.first() {
                        recurse(first, parts, Prec::Equals);
                    }
                    for arg in args.iter().skip(1) {
                        literal!(",");
                        recurse(arg, parts, Prec::Equals);
                    }
                    literal!(")")
                },
                Expr::Pow(ref left, ref right) => binop!(left, right, Prec::Pow, Prec::Term, "^"),
                Expr::Frac(ref left, ref right) => binop!(left, right, Prec::Div, Prec::Mul, " / "),
                Expr::Add(ref left, ref right) => binop!(left, right, Prec::Add, Prec::Div, " + "),
                Expr::Sub(ref left, ref right) => binop!(left, right, Prec::Add, Prec::Div, " - "),
                Expr::Plus(ref expr) => {
                    literal!("+");
                    recurse(expr, parts, Prec::Plus)
                },
                Expr::Neg(ref expr) => {
                    literal!("-");
                    recurse(expr, parts, Prec::Plus)
                },
                Expr::Equals(ref left, ref right) => binop!(left, right, Prec::Equals, Prec::Add, " = "),
                Expr::Suffix(ref op, ref expr) => {
                    if prec < Prec::Mul {
                        literal!("(");
                    }
                    recurse(expr, parts, Prec::Mul);
                    literal!(op.to_string());
                    if prec < Prec::Mul {
                        literal!(")");
                    }
                },
                Expr::Of(ref field, ref expr) => {
                    if prec < Prec::Add {
                        literal!("(");
                    }
                    let mut sub = vec![];
                    recurse(expr, &mut sub, Prec::Div);
                    parts.push(ExprParts::Property(
                        field.to_owned(),
                        sub
                    ));
                    if prec < Prec::Add {
                        literal!(")");
                    }
                },
                Expr::Error(ref err) => parts.push(ExprParts::Error(err.to_owned()))
            }
        }

        recurse(expr, &mut parts, Prec::Equals);

        ExprReply {
            exprs: parts
        }
    }
}

impl From<NotFoundError> for QueryError {
    fn from(v: NotFoundError) -> Self {
        QueryError::NotFound(v)
    }
}

impl From<String> for QueryError {
    fn from(s: String) -> Self {
        QueryError::Generic(s)
    }
}

impl Display for QueryReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            QueryReply::Number(ref v) => write!(fmt, "{}", v),
            QueryReply::Date(ref v) => write!(fmt, "{}", v),
            QueryReply::Substance(ref v) => write!(fmt, "{}", v),
            QueryReply::Duration(ref v) => write!(fmt, "{}", v),
            QueryReply::Def(ref v) => write!(fmt, "{}", v),
            QueryReply::Conversion(ref v) => write!(fmt, "{}", v),
            QueryReply::Factorize(ref v) => write!(fmt, "{}", v),
            QueryReply::UnitsFor(ref v) => write!(fmt, "{}", v),
            QueryReply::UnitList(ref v) => write!(fmt, "{}", v),
            QueryReply::Search(ref v) => write!(fmt, "{}", v),
        }
    }
}

impl Display for QueryError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            QueryError::Generic(ref v) => write!(fmt, "{}", v),
            QueryError::Conformance(ref v) => write!(fmt, "{}", v),
            QueryError::NotFound(ref v) => write!(fmt, "{}", v),
        }
    }
}

impl Display for NotFoundError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self.suggestion.as_ref() {
            Some(ref s) => write!(
                fmt, "No such unit {}, did you mean {}?", self.got, s),
            None => write!(
                fmt, "No such unit {}", self.got)
        }
    }
}

impl Display for ConformanceError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        try!(writeln!(fmt, "Conformance error: {} != {}", self.left, self.right));
        write!(fmt, "Suggestions: {}", self.suggestions.join(", "))
    }
}

impl DateReply {
    pub fn new<Tz>(ctx: &::context::Context, date: DateTime<Tz>) -> DateReply
    where Tz: TimeZone, Tz::Offset: Display {
        use chrono::{Datelike, Timelike};
        DateReply {
            string: date.to_string(),
            year: date.year(),
            month: date.month() as i32,
            day: date.day() as i32,
            hour: date.hour() as i32,
            minute: date.minute() as i32,
            second: date.second() as i32,
            nanosecond: date.nanosecond() as i32,
            human: ctx.humanize(date),
        }
    }
}

impl Display for DateReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        try!(write!(fmt, "{}", self.string));
        if let Some(ref human) = self.human {
            try!(write!(fmt, " ({})", human));
        }
        Ok(())
    }
}

impl Display for SubstanceReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(
            fmt, "{}: {}{}",
            self.name,
            self.doc.as_ref().map(|x| format!("{} ", x)).unwrap_or_default(),
            self.properties.iter().map(|prop| format!(
                "{} = {}{}",
                prop.name,
                prop.value.format("n u"),
                prop.doc.as_ref()
                    .map(|x| format!(" ({})", x))
                    .unwrap_or_else(|| "".to_owned())
            )).collect::<Vec<_>>().join("; ")
        )
    }
}

impl Display for DefReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        try!(write!(fmt, "Definition: {}", self.canon_name));
        if let Some(ref def) = self.def {
            try!(write!(fmt, " = {}", def));
        }
        if let Some(ref value) = self.value {
            try!(write!(fmt, " = {}", value.format("n u p")));
        }
        if let Some(ref doc) = self.doc {
            try!(write!(fmt, ". {}", doc));
        }
        Ok(())
    }
}

impl Display for ConversionReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}", self.value)
    }
}

impl Display for FactorizeReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "Factorizations: {}", self.factorizations.iter().map(|x| {
            x.iter().map(|(u, p)| {
                if *p == 1 { u.to_string() }
                else { format!("{}^{}", u, p) }
            }).collect::<Vec<_>>().join(" ")
        }).collect::<Vec<_>>().join(";  "))
    }
}

impl Display for UnitsForReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "Units for {}: {}", self.of.format("D w"), self.units.iter().map(|cat| {
            if let Some(ref category) = cat.category {
                format!("{}: {}", category, cat.units.join(", "))
            } else {
                cat.units.join(", ")
            }
        }).collect::<Vec<_>>().join("; "))
    }
}

impl Display for DurationReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let res = [&self.years, &self.months, &self.weeks, &self.days,
                   &self.hours, &self.minutes]
            .iter()
            .filter(|x| x.exact_value.as_ref().map(|x| &**x) != Some("0"))
            .chain(once(&&self.seconds))
            .map(|x| {
                 x.to_string()
            })
            .collect::<Vec<_>>()
            .join(", ");
        try!(write!(fmt, "{}", res));
        if let Some(q) = self.raw.quantity.as_ref() {
            write!(fmt, " ({})", q)
        } else {
            Ok(())
        }
    }
}

impl Display for UnitListReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        try!(write!(fmt, "{}",
                    self.list.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")));
        if let Some(q) = self.rest.quantity.as_ref() {
            write!(fmt, " ({})", q)
        } else {
            Ok(())
        }
    }
}

impl Display for SearchReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(
            fmt, "Search results: {}",
            self.results.iter()
                .map(|x| x.format("u p"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
