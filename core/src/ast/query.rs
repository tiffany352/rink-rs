use super::*;

#[derive(Debug, Clone, Serialize)]
pub enum Conversion {
    None,
    Expr(Expr),
    Degree(Degree),
    List(Vec<String>),
    Offset(i64),
    #[serde(skip)]
    Timezone(Tz),
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub enum Query {
    Expr(Expr),
    Convert(Expr, Conversion, Option<u8>, Digits),
    Factorize(Expr),
    UnitsFor(Expr),
    Search(String),
    Error(String),
}

impl fmt::Display for Conversion {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Conversion::None => write!(fmt, "nothing"),
            Conversion::Expr(ref expr) => write!(fmt, "{}", expr),
            Conversion::Degree(ref deg) => write!(fmt, "{}", deg),
            Conversion::List(ref list) => {
                let list = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(fmt, "{}", list)
            }
            Conversion::Offset(off) => write!(fmt, "{:02}:{:02}", off / 3600, (off / 60) % 60),
            Conversion::Timezone(ref tz) => write!(fmt, "{:?}", tz),
        }
    }
}
