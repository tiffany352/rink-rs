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

impl Registry {
    fn lookup_exact(&self, name: &str) -> Option<Number> {
        if let Some(k) = self.dimensions.get(name) {
            return Some(Number::one_unit(k.to_owned()));
        }
        if let Some(v) = self.units.get(name).cloned() {
            return Some(v);
        }
        for (unit, quantity) in &self.quantities {
            if name == quantity {
                return Some(Number {
                    value: Numeric::one(),
                    unit: unit.clone(),
                });
            }
        }
        None
    }

    fn lookup_with_prefix(&self, name: &str) -> Option<Number> {
        if let Some(v) = self.lookup_exact(name) {
            return Some(v);
        }
        for &(ref pre, ref value) in &self.prefixes {
            if name.starts_with(pre) {
                if let Some(v) = self.lookup_exact(&name[pre.len()..]) {
                    return Some((&v * &Number::new(value.clone())).unwrap());
                }
            }
        }
        None
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<Number> {
        let res = self.lookup_with_prefix(name);
        if res.is_some() {
            return res;
        }

        // Check for plurals, but only do this after exhausting every
        // other possibility, so that `ks` is kiloseconds instead of
        // kelvin.
        if let Some(name) = name.strip_suffix('s') {
            self.lookup_with_prefix(name)
        } else {
            None
        }
    }
}
