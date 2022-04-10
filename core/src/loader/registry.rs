use std::collections::{BTreeMap, BTreeSet};

use crate::{
    ast::{DatePattern, Expr},
    runtime::Substance,
    types::{BaseUnit, Dimensionality, Number, Numeric},
};

#[derive(Default, Debug)]
pub struct Registry {
    /// Contains the base units, e.g. `kg`, `bit`.
    pub base_units: BTreeSet<BaseUnit>,
    /// Mappings from short forms of base units to long forms, e.g. `kg` → `kilogram`.
    pub base_unit_long_names: BTreeMap<String, String>,
    /// Contains numerical values of units.
    pub units: BTreeMap<String, Number>,
    /// Maps dimensionality to named physical quantities like `energy`.
    pub quantities: BTreeMap<Dimensionality, String>,
    /// Maps dimensionality to names of SI derived units (newton,
    /// pascal, etc.) for showing simplified forms of units.
    pub decomposition_units: BTreeMap<Dimensionality, String>,
    /// A list of prefixes that can be applied to units, like `kilo`.
    pub prefixes: Vec<(String, Numeric)>,
    /// Contains the original expressions defining a unit.
    pub definitions: BTreeMap<String, Expr>,
    /// Contains documentation strings.
    pub docs: BTreeMap<String, String>,
    /// Maps unit names to category IDs.
    pub categories: BTreeMap<String, String>,
    /// Maps category IDs to display names.
    pub category_names: BTreeMap<String, String>,
    /// Used for matching date formats.
    pub datepatterns: Vec<Vec<DatePattern>>,
    /// Objects or materials that have certain properties.
    pub substances: BTreeMap<String, Substance>,
    /// Maps elemental names (like `He`) to substance names (`helium`),
    /// used for parsing molecular formulas, e.g. `H2O`.
    pub substance_symbols: BTreeMap<String, String>,
}

impl Registry {
    fn lookup_exact(&self, name: &str) -> Option<Number> {
        if let Some(k) = self.base_units.get(name) {
            return Some(Number::one_unit(k.to_owned()));
        }
        if let Some(v) = self.units.get(name).cloned() {
            return Some(v);
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

    fn canonicalize_exact(&self, name: &str) -> Option<String> {
        if let Some(v) = self.base_unit_long_names.get(name) {
            return Some(v.clone());
        }
        if let Some(base_unit) = self.base_units.get(name) {
            return Some(base_unit.to_string());
        }
        if let Some(expr) = self.definitions.get(name) {
            if let Expr::Unit { ref name } = *expr {
                if let Some(canonicalized) = self.canonicalize(&*name) {
                    return Some(canonicalized);
                } else {
                    return Some(name.clone());
                }
            } else {
                // we cannot canonicalize it further
                return Some(name.to_owned());
            }
        }
        None
    }

    fn canonicalize_with_prefix(&self, name: &str) -> Option<String> {
        if let Some(v) = self.canonicalize_exact(name) {
            return Some(v);
        }
        for &(ref prefix, ref value) in &self.prefixes {
            if let Some(name) = name.strip_prefix(prefix) {
                if let Some(canonicalized) = self.canonicalize_exact(name) {
                    let mut prefix = prefix;
                    for &(ref other, ref otherval) in &self.prefixes {
                        if other.len() > prefix.len() && value == otherval {
                            prefix = other;
                        }
                    }
                    return Some(format!("{}{}", prefix, canonicalized));
                }
            }
        }
        None
    }

    /// Given a unit name, try to return a canonical name (expanding aliases and such)
    pub fn canonicalize(&self, name: &str) -> Option<String> {
        let res = self.canonicalize_with_prefix(name);
        if res.is_some() {
            return res;
        }

        if let Some(name) = name.strip_suffix('s') {
            self.canonicalize_with_prefix(name)
        } else {
            None
        }
    }
}
