// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::ast::{DatePattern, Expr};
use crate::number::{Dimension, Number, Quantity};
use crate::numeric::Numeric;
use crate::reply::NotFoundError;
use crate::search;
use crate::substance::Substance;
use chrono::{DateTime, Local, TimeZone};
use std::collections::{BTreeMap, BTreeSet};

/// The evaluation context that contains unit definitions.
#[derive(Debug)]
pub struct Context {
    pub dimensions: BTreeSet<Dimension>,
    pub canonicalizations: BTreeMap<String, String>,
    pub units: BTreeMap<String, Number>,
    pub quantities: BTreeMap<Quantity, String>,
    pub reverse: BTreeMap<Quantity, String>,
    pub prefixes: Vec<(String, Number)>,
    pub definitions: BTreeMap<String, Expr>,
    pub docs: BTreeMap<String, String>,
    pub categories: BTreeMap<String, String>,
    pub category_names: BTreeMap<String, String>,
    pub datepatterns: Vec<Vec<DatePattern>>,
    pub substances: BTreeMap<String, Substance>,
    pub substance_symbols: BTreeMap<String, String>,
    pub temporaries: BTreeMap<String, Number>,
    pub now: DateTime<Local>,
    pub short_output: bool,
    pub use_humanize: bool,
    pub save_previous_result: bool,
    pub previous_result: Option<Number>,
}

impl Default for Context {
    /// Equivalent to Context::new()
    fn default() -> Self {
        Context::new()
    }
}

impl Context {
    /// Creates a new, empty context
    pub fn new() -> Context {
        Context {
            short_output: false,
            use_humanize: true,
            save_previous_result: false,

            now: Local.timestamp(0, 0),

            dimensions: BTreeSet::new(),
            prefixes: vec![],
            datepatterns: vec![],
            canonicalizations: BTreeMap::new(),
            units: BTreeMap::new(),
            quantities: BTreeMap::new(),
            reverse: BTreeMap::new(),
            definitions: BTreeMap::new(),
            docs: BTreeMap::new(),
            categories: BTreeMap::new(),
            category_names: BTreeMap::new(),
            substances: BTreeMap::new(),
            substance_symbols: BTreeMap::new(),
            temporaries: BTreeMap::new(),
            previous_result: None,
        }
    }

    pub fn set_time(&mut self, time: DateTime<Local>) {
        self.now = time;
    }

    pub fn update_time(&mut self) {
        self.now = Local::now();
    }

    pub fn load_dates(&mut self, mut dates: Vec<Vec<DatePattern>>) {
        self.datepatterns.append(&mut dates)
    }

    /// Given a unit name, returns its value if it exists. Supports SI
    /// prefixes, plurals, bare dimensions like length, and quantities.
    pub fn lookup(&self, name: &str) -> Option<Number> {
        fn inner(ctx: &Context, name: &str) -> Option<Number> {
            if name == "ans" || name == "ANS" || name == "_" {
                return ctx.previous_result.clone();
            }
            if let Some(v) = ctx.temporaries.get(name).cloned() {
                return Some(v);
            }
            if let Some(k) = ctx.dimensions.get(name) {
                return Some(Number::one_unit(k.to_owned()));
            }
            if let Some(v) = ctx.units.get(name).cloned() {
                return Some(v);
            }
            for (unit, quantity) in &ctx.quantities {
                if name == quantity {
                    return Some(Number {
                        value: Numeric::one(),
                        unit: unit.clone(),
                    });
                }
            }
            None
        }

        let outer = |name: &str| -> Option<Number> {
            if let Some(v) = inner(self, name) {
                return Some(v);
            }
            for &(ref pre, ref value) in &self.prefixes {
                if name.starts_with(pre) {
                    if let Some(v) = inner(self, &name[pre.len()..]) {
                        return Some((&v * value).unwrap());
                    }
                }
            }
            None
        };

        let res = outer(name);
        if res.is_some() {
            return res;
        }

        // after so that "ks" is kiloseconds
        if name.ends_with('s') {
            let name = &name[0..name.len() - 1];
            outer(name)
        } else {
            None
        }
    }

    /// Given a unit name, try to return a canonical name (expanding aliases and such)
    pub fn canonicalize(&self, name: &str) -> Option<String> {
        fn inner(ctx: &Context, name: &str) -> Option<String> {
            if let Some(v) = ctx.canonicalizations.get(name) {
                return Some(v.clone());
            }
            if let Some(k) = ctx.dimensions.get(name) {
                return Some((*k.id).clone());
            }
            if let Some(v) = ctx.definitions.get(name) {
                if let Expr::Unit { ref name } = *v {
                    if let Some(r) = ctx.canonicalize(&*name) {
                        return Some(r);
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

        let outer = |name: &str| -> Option<String> {
            if let Some(v) = inner(self, name) {
                return Some(v);
            }
            for &(ref pre, ref val) in &self.prefixes {
                if name.starts_with(pre) {
                    if let Some(v) = inner(self, &name[pre.len()..]) {
                        let mut pre = pre;
                        for &(ref other, ref otherval) in &self.prefixes {
                            if other.len() > pre.len() && val == otherval {
                                pre = other;
                            }
                        }
                        return Some(format!("{}{}", pre, v));
                    }
                }
            }
            None
        };

        let res = outer(name);
        if res.is_some() {
            return res;
        }

        if name.ends_with('s') {
            let name = &name[0..name.len() - 1];
            outer(name)
        } else {
            None
        }
    }

    /// Describes a value's unit, gives true if the unit is reciprocal
    /// (e.g. you should prefix "1.0 / " or replace "multiply" with
    /// "divide" when rendering it).
    pub fn describe_unit(&self, value: &Number) -> (bool, String) {
        use std::io::Write;

        let mut buf = vec![];
        let mut recip = false;
        let square = Number {
            value: Numeric::one(),
            unit: value.unit.clone(),
        }
        .root(2)
        .ok();
        let inverse = (&Number::one()
            / &Number {
                value: Numeric::one(),
                unit: value.unit.clone(),
            })
            .unwrap();
        if let Some(name) = self.quantities.get(&value.unit) {
            write!(buf, "{}", name).unwrap();
        } else if let Some(name) = square.and_then(|square| self.quantities.get(&square.unit)) {
            write!(buf, "{}^2", name).unwrap();
        } else if let Some(name) = self.quantities.get(&inverse.unit) {
            recip = true;
            write!(buf, "{}", name).unwrap();
        } else {
            let helper = |dim: &Dimension, pow: i64, buf: &mut Vec<u8>| {
                let mut map = Quantity::new();
                map.insert(dim.clone(), pow);
                if let Some(name) = self.quantities.get(&map) {
                    write!(buf, " {}", name).unwrap();
                } else {
                    let mut map = Quantity::new();
                    map.insert(dim.clone(), 1);
                    if let Some(name) = self.quantities.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        write!(buf, " '{}'", dim).unwrap();
                    }
                    if pow != 1 {
                        write!(buf, "^{}", pow).unwrap();
                    }
                }
            };

            let mut frac = vec![];
            let mut found = false;
            for (dim, &pow) in &value.unit {
                if pow < 0 {
                    frac.push((dim, -pow));
                } else {
                    found = true;
                    helper(dim, pow, &mut buf);
                }
            }
            if !frac.is_empty() {
                if !found {
                    recip = true;
                } else {
                    write!(buf, " /").unwrap();
                }
                for (dim, pow) in frac {
                    let mut map = Quantity::new();
                    map.insert(dim.clone(), pow);
                    if let Some(name) = self.quantities.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        helper(dim, pow, &mut buf);
                    }
                }
            }
            buf.remove(0);
        }

        (recip, String::from_utf8(buf).unwrap())
    }

    pub fn typo_dym<'a>(&'a self, what: &str) -> Option<&'a str> {
        search::search(self, what, 1).into_iter().next()
    }

    pub fn unknown_unit_err(&self, name: &str) -> NotFoundError {
        NotFoundError {
            got: name.to_owned(),
            suggestion: self.typo_dym(name).map(|x| x.to_owned()),
        }
    }
}
