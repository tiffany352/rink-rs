use num::BigInt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Approx {
    pub value: BigInt,
    pub precision: u64,
    pub exact: bool,
}

impl Approx {
    pub fn new(value: BigInt, precision: u64, exact: bool) -> Approx {
        Approx {
            value,
            precision,
            exact,
        }
    }
}
