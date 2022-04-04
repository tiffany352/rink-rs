use crate::types::Numeric;
use serde_derive::Serialize;

/// Used when converting to string representation to choose desired
/// output mode.
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum Digits {
    Default,
    FullInt,
    Digits(u64),
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NumericParts {
    numer: String,
    denom: String,
    exact_value: Option<String>,
    approx_value: Option<String>,
}

impl From<Numeric> for NumericParts {
    fn from(value: Numeric) -> NumericParts {
        let (exact, approx) = value.string_repr(10, Digits::Default);
        let (num, den) = value.to_rational();
        NumericParts {
            numer: num.to_string(),
            denom: den.to_string(),
            exact_value: exact,
            approx_value: approx,
        }
    }
}
