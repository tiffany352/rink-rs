use crate::ast::{Expr, Precedence, UnaryOpType};
use crate::number::NumberParts;
use crate::numeric::Digits;
use chrono::{DateTime, TimeZone};
use std::fmt::Result as FmtResult;
use std::fmt::{Display, Formatter};
use std::iter::once;
use std::rc::Rc;
use std::{borrow::Cow, collections::BTreeMap};
use std::{convert::From, iter::Peekable};

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "lowercase")]
pub enum ExprParts {
    Literal {
        text: String,
    },
    Unit {
        name: String,
    },
    Property {
        property: String,
        subject: Vec<ExprParts>,
    },
    Error {
        message: String,
    },
}

#[derive(Debug, Clone, Serialize)]
pub struct ExprReply {
    exprs: Vec<ExprParts>,
    ast: Expr,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DefReply {
    pub canon_name: String,
    pub def: Option<String>,
    pub def_expr: Option<ExprReply>,
    pub value: Option<NumberParts>,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConversionReply {
    pub value: NumberParts,
}

#[derive(Debug, Clone, Serialize)]
pub struct FactorizeReply {
    pub factorizations: Vec<Factorization>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(transparent)]
pub struct Factorization {
    pub units: BTreeMap<Rc<String>, usize>,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnitsInCategory {
    pub category: Option<String>,
    pub units: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnitsForReply {
    pub units: Vec<UnitsInCategory>,
    /// Dimensions and quantity are set.
    pub of: NumberParts,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnitListReply {
    pub rest: NumberParts,
    pub list: Vec<NumberParts>,
}

#[derive(Debug, Clone, Serialize)]
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

#[derive(Debug, Clone, Serialize)]
pub struct SearchReply {
    pub results: Vec<NumberParts>,
}

#[derive(Debug, Clone, Serialize)]
pub struct PropertyReply {
    pub name: String,
    pub value: NumberParts,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct SubstanceReply {
    pub name: String,
    pub doc: Option<String>,
    pub amount: NumberParts,
    pub properties: Vec<PropertyReply>,
}

#[derive(Debug, Clone, Serialize)]
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
    pub rfc3339: String,
}

#[derive(Debug, Clone, Serialize)]
#[allow(clippy::large_enum_variant)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
pub enum QueryReply {
    Number(NumberParts),
    Date(DateReply),
    Substance(SubstanceReply),
    Duration(Box<DurationReply>),
    Def(Box<DefReply>),
    Conversion(Box<ConversionReply>),
    Factorize(FactorizeReply),
    UnitsFor(UnitsForReply),
    UnitList(UnitListReply),
    Search(SearchReply),
}

#[derive(Debug, Clone, Serialize)]
pub struct ConformanceError {
    pub left: NumberParts,
    pub right: NumberParts,
    pub suggestions: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct NotFoundError {
    pub got: String,
    pub suggestion: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "camelCase")]
pub enum QueryError {
    Conformance(Box<ConformanceError>),
    NotFound(NotFoundError),
    Generic { message: String },
}

impl QueryError {
    pub fn generic(message: String) -> QueryError {
        QueryError::Generic { message }
    }
}

impl ExprReply {
    pub fn from(expr: &Expr) -> ExprReply {
        let mut parts = vec![];

        fn recurse(expr: &Expr, parts: &mut Vec<ExprParts>, prec: Precedence) {
            macro_rules! literal {
                ($e:expr) => {{
                    parts.push(ExprParts::Literal {
                        text: $e.to_owned(),
                    })
                }};
            }
            match *expr {
                Expr::Unit { ref name } => parts.push(ExprParts::Unit { name: name.clone() }),
                Expr::Quote { ref string } => literal!(format!("'{}'", string)),
                Expr::Const { ref value } => {
                    let (_exact, val) = value.to_string(10, Digits::Default);
                    literal!(val)
                }
                Expr::Date { .. } => literal!("NYI: date expr to expr parts"),
                Expr::Mul { ref exprs } => {
                    if prec < Precedence::Mul {
                        literal!("(");
                    }
                    for expr in exprs.iter() {
                        recurse(expr, parts, Precedence::Pow);
                    }
                    if prec < Precedence::Mul {
                        literal!(")");
                    }
                }
                Expr::Call { ref func, ref args } => {
                    literal!(format!("{}(", func.name()));
                    if let Some(first) = args.first() {
                        recurse(first, parts, Precedence::Equals);
                    }
                    for arg in args.iter().skip(1) {
                        literal!(",");
                        recurse(arg, parts, Precedence::Equals);
                    }
                    literal!(")")
                }
                Expr::BinOp(ref binop) => {
                    let op_prec = Precedence::from(binop.op);
                    let succ = Precedence::next(binop.op);
                    if prec < op_prec {
                        literal!("(");
                    }
                    recurse(&binop.left, parts, succ);
                    literal!(binop.op.symbol());
                    recurse(&binop.right, parts, op_prec);
                    if prec < op_prec {
                        literal!(")");
                    }
                }
                Expr::UnaryOp(ref unaryop) => match unaryop.op {
                    UnaryOpType::Positive => {
                        literal!("+");
                        recurse(expr, parts, Precedence::Plus)
                    }
                    UnaryOpType::Negative => {
                        literal!("-");
                        recurse(expr, parts, Precedence::Plus)
                    }
                    UnaryOpType::Degree(ref suffix) => {
                        if prec < Precedence::Mul {
                            literal!("(");
                        }
                        recurse(&unaryop.expr, parts, Precedence::Mul);
                        literal!(suffix.to_string());
                        if prec < Precedence::Mul {
                            literal!(")");
                        }
                    }
                },
                Expr::Of {
                    ref property,
                    ref expr,
                } => {
                    if prec < Precedence::Add {
                        literal!("(");
                    }
                    let mut sub = vec![];
                    recurse(expr, &mut sub, Precedence::Div);
                    parts.push(ExprParts::Property {
                        property: property.to_owned(),
                        subject: sub,
                    });
                    if prec < Precedence::Add {
                        literal!(")");
                    }
                }
                Expr::Error { ref message } => parts.push(ExprParts::Error {
                    message: message.to_owned(),
                }),
            }
        }

        recurse(expr, &mut parts, Precedence::Equals);

        ExprReply {
            exprs: parts,
            ast: expr.clone(),
        }
    }
}

impl From<NotFoundError> for QueryError {
    fn from(v: NotFoundError) -> Self {
        QueryError::NotFound(v)
    }
}

impl From<String> for QueryError {
    fn from(message: String) -> Self {
        QueryError::Generic { message }
    }
}

impl Display for QueryReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
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
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        match *self {
            QueryError::Generic { ref message } => write!(fmt, "{}", message),
            QueryError::Conformance(ref v) => write!(fmt, "{}", v),
            QueryError::NotFound(ref v) => write!(fmt, "{}", v),
        }
    }
}

impl Display for NotFoundError {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        match self.suggestion.as_ref() {
            Some(ref s) => write!(fmt, "No such unit {}, did you mean {}?", self.got, s),
            None => write!(fmt, "No such unit {}", self.got),
        }
    }
}

impl Display for ConformanceError {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        writeln!(fmt, "Conformance error: {} != {}", self.left, self.right)?;
        write!(fmt, "Suggestions: {}", self.suggestions.join(", "))
    }
}

impl DateReply {
    pub fn new<Tz>(ctx: &crate::context::Context, date: DateTime<Tz>) -> DateReply
    where
        Tz: TimeZone,
        Tz::Offset: Display,
    {
        use chrono::{Datelike, Timelike};
        DateReply {
            string: date.to_string(),
            rfc3339: date.to_rfc3339(),
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
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(fmt, "{}", self.string)?;
        if let Some(ref human) = self.human {
            write!(fmt, " ({})", human)?;
        }
        Ok(())
    }
}

impl Display for SubstanceReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(
            fmt,
            "{}: {}{}",
            self.name,
            self.doc
                .as_ref()
                .map(|x| format!("{} ", x))
                .unwrap_or_default(),
            self.properties
                .iter()
                .map(|prop| format!(
                    "{} = {}{}",
                    prop.name,
                    prop.value.format("n u"),
                    prop.doc
                        .as_ref()
                        .map(|x| format!(" ({})", x))
                        .unwrap_or_else(|| "".to_owned())
                ))
                .collect::<Vec<_>>()
                .join("; ")
        )
    }
}

impl Display for DefReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(fmt, "Definition: {}", self.canon_name)?;
        if let Some(ref def) = self.def {
            write!(fmt, " = {}", def)?;
        }
        if let Some(ref value) = self.value {
            write!(fmt, " = {}", value.format("n u p"))?;
        }
        if let Some(ref doc) = self.doc {
            write!(fmt, ". {}", doc)?;
        }
        Ok(())
    }
}

impl Display for ConversionReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(fmt, "{}", self.value)
    }
}

impl Display for FactorizeReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(
            fmt,
            "Factorizations: {}",
            self.factorizations
                .iter()
                .map(|x| {
                    x.units
                        .iter()
                        .map(|(u, p)| {
                            if *p == 1 {
                                u.to_string()
                            } else {
                                format!("{}^{}", u, p)
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                })
                .collect::<Vec<_>>()
                .join(";  ")
        )
    }
}

impl Display for UnitsForReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(
            fmt,
            "Units for {}: {}",
            self.of.format("D w"),
            self.units
                .iter()
                .map(|cat| {
                    if let Some(ref category) = cat.category {
                        format!("{}: {}", category, cat.units.join(", "))
                    } else {
                        cat.units.join(", ")
                    }
                })
                .collect::<Vec<_>>()
                .join("; ")
        )
    }
}

impl Display for DurationReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        let res = [
            &self.years,
            &self.months,
            &self.weeks,
            &self.days,
            &self.hours,
            &self.minutes,
        ]
        .iter()
        .filter(|x| x.exact_value.as_ref().map(|x| &**x) != Some("0"))
        .chain(once(&&self.seconds))
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join(", ");
        write!(fmt, "{}", res)?;
        if let Some(q) = self.raw.quantity.as_ref() {
            write!(fmt, " ({})", q)
        } else {
            Ok(())
        }
    }
}

impl Display for UnitListReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(
            fmt,
            "{}",
            self.list
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        if let Some(q) = self.rest.quantity.as_ref() {
            write!(fmt, " ({})", q)
        } else {
            Ok(())
        }
    }
}

impl Display for SearchReply {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> FmtResult {
        write!(
            fmt,
            "Search results: {}",
            self.results
                .iter()
                .map(|x| x.format("u p"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone)]
pub enum Span<'a> {
    Content { text: Cow<'a, str>, token: FmtToken },
    Child(&'a dyn TokenFmt<'a>),
}

impl<'a> Span<'a> {
    pub fn new(text: impl Into<Cow<'a, str>>, token: FmtToken) -> Span<'a> {
        Span::Content {
            text: text.into(),
            token,
        }
    }

    pub fn plain(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Plain)
    }

    pub fn unit(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Unit)
    }

    pub fn quantity(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Quantity)
    }

    pub fn prop_name(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::PropName)
    }

    pub fn user_input(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::UserInput)
    }

    pub fn list_begin(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::ListBegin)
    }

    pub fn list_sep(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::ListSep)
    }

    pub fn doc_string(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::DocString)
    }

    pub fn pow(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Pow)
    }

    pub fn child(obj: &'a dyn TokenFmt<'a>) -> Span<'a> {
        Span::Child(obj)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum FmtToken {
    /// Indicator text that isn't based on user input. Generally displayed without any formatting.
    Plain,
    /// The name of a unit, like `kilogram`.
    Unit,
    /// A quantity like length or time.
    Quantity,
    /// A number in any context.
    Number,
    /// A string that's derived in some way from user input, but doesn't
    /// have a more specific usage.
    UserInput,
    /// Text indicating the start of a list, usually a string followed
    /// by a colon.
    ListBegin,
    /// A separator between items in a list, usually a comma or
    /// semicolon. When a lot of vertical space is available, these can
    /// be turned into a bulleted list.
    ListSep,
    /// A documentation string.
    DocString,
    /// A number raised to a power, could be substituted with
    /// superscript.
    Pow,
    /// The name of a property in a substance.
    PropName,
}

pub trait TokenFmt<'a> {
    fn to_spans(&'a self) -> Vec<Span<'a>>;
}

struct JoinIter<'a, I>
where
    I: Iterator,
{
    iter: Peekable<I>,
    sep: Span<'a>,
    last_was_sep: bool,
}

fn join<'a, I>(iter: I, sep: Span<'a>) -> impl Iterator<Item = Span<'a>>
where
    I: Iterator<Item = Span<'a>>,
{
    JoinIter {
        iter: iter.peekable(),
        sep,
        last_was_sep: true,
    }
}

impl<'a, I> Iterator for JoinIter<'a, I>
where
    I: Iterator<Item = Span<'a>>,
{
    type Item = Span<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.peek().is_some() {
            if self.last_was_sep {
                self.last_was_sep = false;
                self.iter.next()
            } else {
                self.last_was_sep = true;
                Some(self.sep.clone())
            }
        } else {
            None
        }
    }
}

struct FlatJoinIter<'a, I, I2>
where
    I: Iterator<Item = I2>,
    I2: IntoIterator<Item = Span<'a>>,
{
    iter: Peekable<I>,
    sep: Span<'a>,
    last_was_sep: bool,
    current: Option<I2::IntoIter>,
}

fn flat_join<'a, I, I2>(iter: I, sep: Span<'a>) -> impl Iterator<Item = Span<'a>>
where
    I: Iterator<Item = I2>,
    I2: IntoIterator<Item = Span<'a>>,
{
    FlatJoinIter {
        iter: iter.peekable(),
        sep,
        last_was_sep: true,
        current: None,
    }
}

impl<'a, I, I2> Iterator for FlatJoinIter<'a, I, I2>
where
    I: Iterator<Item = I2>,
    I2: IntoIterator<Item = Span<'a>>,
{
    type Item = Span<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ref mut current) = self.current {
            if let Some(next) = current.next() {
                return Some(next);
            }
        }
        if self.iter.peek().is_some() {
            if self.last_was_sep {
                self.last_was_sep = false;
                let mut new_current = self.iter.next().unwrap().into_iter();
                let next = new_current.next();
                self.current = Some(new_current);
                next
            } else {
                self.last_was_sep = true;
                Some(self.sep.clone())
            }
        } else {
            None
        }
    }
}

impl<'a> TokenFmt<'a> for QueryReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        match self {
            QueryReply::Number(reply) => reply.to_spans(),
            QueryReply::Date(reply) => reply.to_spans(),
            QueryReply::Substance(reply) => reply.to_spans(),
            QueryReply::Duration(reply) => reply.to_spans(),
            QueryReply::Def(reply) => reply.to_spans(),
            QueryReply::Conversion(reply) => reply.to_spans(),
            QueryReply::Factorize(reply) => reply.to_spans(),
            QueryReply::UnitsFor(reply) => reply.to_spans(),
            QueryReply::UnitList(reply) => reply.to_spans(),
            QueryReply::Search(reply) => reply.to_spans(),
        }
    }
}

impl<'a> TokenFmt<'a> for NumberParts {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        vec![Span::plain(self.format("n u w"))]
    }
}

impl<'a> TokenFmt<'a> for DateReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        if let Some(ref human) = self.human {
            vec![
                Span::user_input(&self.string),
                Span::plain(" ("),
                Span::user_input(human),
                Span::plain(")"),
            ]
        } else {
            vec![Span::user_input(&self.string)]
        }
    }
}

impl<'a> TokenFmt<'a> for SubstanceReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens = vec![Span::unit(&self.name), Span::plain(": ")];
        if let Some(ref doc) = self.doc {
            tokens.push(Span::doc_string(doc));
            tokens.push(Span::plain(" "));
        }
        tokens.push(Span::list_begin(""));
        tokens.extend(join(
            self.properties.iter().map(|prop| Span::child(prop)),
            Span::list_sep("; "),
        ));
        tokens
    }
}

impl<'a> TokenFmt<'a> for PropertyReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens = vec![
            Span::prop_name(&self.name),
            Span::plain(" = "),
            Span::plain(self.value.format("n u")),
        ];
        if let Some(ref doc) = self.doc {
            tokens.push(Span::plain(" ("));
            tokens.push(Span::doc_string(doc));
            tokens.push(Span::plain(")"));
        }
        tokens
    }
}

impl<'a> TokenFmt<'a> for DurationReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let parts = [
            &self.years,
            &self.months,
            &self.weeks,
            &self.days,
            &self.hours,
            &self.minutes,
        ];

        let res = join(
            parts
                .iter()
                .map(|x| *x)
                .filter(|x| x.exact_value.as_ref().map(|x| &**x) != Some("0"))
                .chain(once(&self.seconds))
                .map(|x| Span::plain(x.to_string())),
            Span::list_sep(", "),
        );

        if let Some(ref q) = self.raw.quantity {
            res.chain(once(Span::plain(" (")))
                .chain(once(Span::quantity(q)))
                .chain(once(Span::plain(")")))
                .collect()
        } else {
            res.collect()
        }
    }
}

impl<'a> TokenFmt<'a> for DefReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens = vec![Span::plain("Definition: "), Span::unit(&self.canon_name)];
        if let Some(ref def) = self.def {
            tokens.push(Span::plain(" = "));
            tokens.push(Span::plain(def));
        }
        if let Some(ref value) = self.value {
            tokens.push(Span::plain(" = "));
            tokens.push(Span::plain(value.format("n u p")));
        }
        if let Some(ref doc) = self.doc {
            tokens.push(Span::plain(". "));
            tokens.push(Span::doc_string(doc));
        }
        tokens
    }
}

impl<'a> TokenFmt<'a> for ConversionReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        self.value.to_spans()
    }
}

impl<'a> TokenFmt<'a> for FactorizeReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        once(Span::list_begin("Factorizations: "))
            .chain(join(
                self.factorizations.iter().map(|fac| Span::child(fac)),
                Span::list_sep("; "),
            ))
            .collect()
    }
}

impl<'a> TokenFmt<'a> for Factorization {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        flat_join(
            self.units.iter().map(|(unit, pow)| {
                if *pow == 1 {
                    vec![Span::unit(&**unit)]
                } else {
                    vec![Span::unit(&**unit), Span::pow(format!("^{}", pow))]
                }
            }),
            Span::plain(" "),
        )
        .collect()
    }
}

impl<'a> TokenFmt<'a> for UnitsForReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens = vec![
            Span::plain("Units for "),
            Span::plain(self.of.format("D w")),
            Span::list_begin(": "),
        ];
        tokens.extend(join(
            self.units.iter().map(|cat| Span::child(cat)),
            Span::list_sep("; "),
        ));

        tokens
    }
}

impl<'a> TokenFmt<'a> for UnitsInCategory {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens = vec![];
        if let Some(ref category) = self.category {
            tokens.push(Span::plain(category));
            tokens.push(Span::list_begin(": "));
        }
        tokens.extend(join(
            self.units.iter().map(Span::unit),
            Span::list_sep(", "),
        ));
        tokens
    }
}

impl<'a> TokenFmt<'a> for UnitListReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens: Vec<Span<'a>> = join(
            self.list.iter().map(|x| Span::plain(x.to_string())),
            Span::list_sep(", "),
        )
        .collect();
        if let Some(ref quantity) = self.rest.quantity {
            tokens.push(Span::plain(" ("));
            tokens.push(Span::quantity(quantity));
            tokens.push(Span::plain(")"));
        }
        tokens
    }
}

impl<'a> TokenFmt<'a> for SearchReply {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        once(Span::list_begin("Search results: "))
            .chain(join(
                self.results
                    .iter()
                    .map(|x| x.format("u p"))
                    .map(Span::plain),
                Span::list_sep(", "),
            ))
            .collect()
    }
}

impl<'a> TokenFmt<'a> for QueryError {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        match self {
            QueryError::Conformance(err) => err.to_spans(),
            QueryError::NotFound(err) => err.to_spans(),
            QueryError::Generic { message } => vec![Span::plain(message)],
        }
    }
}

impl<'a> TokenFmt<'a> for ConformanceError {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens = vec![
            Span::plain("Conformance error: "),
            Span::plain(self.left.to_string()),
            Span::plain(" != "),
            Span::plain(self.right.to_string()),
            Span::list_begin("\nSuggestions: "),
        ];
        tokens.extend(join(
            self.suggestions.iter().map(Span::plain),
            Span::list_sep(", "),
        ));
        tokens
    }
}

impl<'a> TokenFmt<'a> for NotFoundError {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut tokens = vec![Span::plain("No such unit "), Span::user_input(&self.got)];
        if let Some(ref suggestion) = self.suggestion {
            tokens.push(Span::plain(", did you mean "));
            tokens.push(Span::unit(suggestion));
            tokens.push(Span::plain("?"));
        }
        tokens
    }
}
