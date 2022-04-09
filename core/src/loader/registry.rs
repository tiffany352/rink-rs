use std::collections::{BTreeMap, BTreeSet};

use crate::{
    ast::{DatePattern, Expr},
    runtime::Substance,
    types::{BaseUnit, Dimensionality, Number, Numeric},
};

#[derive(Default, Debug)]
pub struct Registry {
    pub dimensions: BTreeSet<BaseUnit>,
    pub canonicalizations: BTreeMap<String, String>,
    pub units: BTreeMap<String, Number>,
    pub quantities: BTreeMap<Dimensionality, String>,
    pub reverse: BTreeMap<Dimensionality, String>,
    pub prefixes: Vec<(String, Numeric)>,
    pub definitions: BTreeMap<String, Expr>,
    pub docs: BTreeMap<String, String>,
    pub categories: BTreeMap<String, String>,
    pub category_names: BTreeMap<String, String>,
    pub datepatterns: Vec<Vec<DatePattern>>,
    pub substances: BTreeMap<String, Substance>,
    pub substance_symbols: BTreeMap<String, String>,
}
