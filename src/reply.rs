use number::NumberParts;
use std::convert::From;
use std::rc::Rc;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::fmt::Result as FmtResult;
use chrono::{DateTime, FixedOffset};

#[derive(Debug, Clone)]
pub struct DefReply {
    pub canon_name: String,
    pub def: String,
    pub value: NumberParts,
    pub doc: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ConversionReply {
    pub value: NumberParts,
}

#[derive(Debug, Clone)]
pub struct FactorizeReply {
    pub factorizations: Vec<BTreeMap<Rc<String>, usize>>,
}

#[derive(Debug, Clone)]
pub struct UnitsForReply {
    pub units: Vec<String>,
    /// Dimensions and quantity are set.
    pub of: NumberParts,
}

#[derive(Debug, Clone)]
pub struct ConformanceError {
    pub left: NumberParts,
    pub right: NumberParts,
    pub suggestions: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct UnitListReply {
    pub rest: NumberParts,
    pub list: Vec<NumberParts>,
}

#[derive(Debug, Clone)]
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
pub enum QueryReply {
    Number(NumberParts),
    Date(DateTime<FixedOffset>),
    Duration(DurationReply),
    Def(DefReply),
    Conversion(ConversionReply),
    Factorize(FactorizeReply),
    UnitsFor(UnitsForReply),
    UnitList(UnitListReply),
}

#[derive(Debug, Clone)]
pub enum QueryError {
    Conformance(ConformanceError),
    Generic(String),
}

impl From<String> for QueryError {
    fn from(s: String) -> Self {
        QueryError::Generic(s)
    }
}

impl Display for QueryReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        use chrono_humanize::HumanTime;
        match *self {
            QueryReply::Number(ref v) => write!(fmt, "{}", v),
            QueryReply::Date(ref v) => write!(fmt, "{} ({})", v, HumanTime::from(*v)),
            QueryReply::Duration(ref v) => write!(fmt, "{}", v),
            QueryReply::Def(ref v) => write!(fmt, "{}", v),
            QueryReply::Conversion(ref v) => write!(fmt, "{}", v),
            QueryReply::Factorize(ref v) => write!(fmt, "{}", v),
            QueryReply::UnitsFor(ref v) => write!(fmt, "{}", v),
            QueryReply::UnitList(ref v) => write!(fmt, "{}", v),
        }
    }
}

impl Display for QueryError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            QueryError::Generic(ref v) => write!(fmt, "{}", v),
            QueryError::Conformance(ref v) => write!(fmt, "{}", v),
        }
    }
}

impl Display for ConformanceError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        try!(writeln!(fmt, "Conformance error: {} != {}", self.left, self.right));
        write!(fmt, "Suggestions: {}", self.suggestions.join(", "))
    }
}

impl Display for DefReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        try!(write!(fmt, "Definition: {} = {} = {}",
                    self.canon_name, self.def, self.value.format("n u p")));
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
                if *p == 1 { format!("{}", u) }
                else { format!("{}^{}", u, p) }
            }).collect::<Vec<_>>().join(" ")
        }).collect::<Vec<_>>().join(";  "))
    }
}

impl Display for UnitsForReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "Units for {}: {}", self.of.format("D w"), self.units.join(", "))
    }
}

impl Display for DurationReply {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let res = [&self.years, &self.months, &self.weeks, &self.days, &self.hours, &self.minutes, &self.seconds]
            .iter()
            .filter_map(|x| {
                if x.exact_value.as_ref().map(|x| &**x) == Some("0") {
                    None
                } else {
                    Some(x)
                }
            })
            .map(|x| {
                 format!("{}", x)
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
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(", ")));
        if let Some(q) = self.rest.quantity.as_ref() {
            write!(fmt, " ({})", q)
        } else {
            Ok(())
        }
    }
}
