// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::{BTreeMap, BTreeSet};
use number::{Dim, Number, Unit};
use num::Num;
use ast::{Expr, DatePattern};
use search;
use substance::Substance;
use reply::NotFoundError;
use std::cell::RefCell;

#[cfg(feature = "lmdb")]
pub struct Lmdb {
    pub env: ::lmdb::Environment,
    pub substances: ::lmdb::Database,
}

#[cfg(not(feature = "lmdb"))]
pub type Lmdb = ();

/// The evaluation context that contains unit definitions.
pub struct Context {
    pub dimensions: BTreeSet<Dim>,
    pub canonicalizations: BTreeMap<String, String>,
    pub units: BTreeMap<String, Number>,
    pub quantities: BTreeMap<Unit, String>,
    pub reverse: BTreeMap<Unit, String>,
    pub prefixes: Vec<(String, Number)>,
    pub definitions: BTreeMap<String, Expr>,
    pub docs: BTreeMap<String, String>,
    pub datepatterns: Vec<Vec<DatePattern>>,
    pub substances: BTreeMap<String, Substance>,
    pub temporaries: RefCell<BTreeMap<String, Number>>,
    pub short_output: bool,
    pub use_humanize: bool,
    pub lmdb: Option<Lmdb>,
}

impl Context {
    /// Creates a new, empty context
    pub fn new() -> Context {
        Context {
            dimensions: BTreeSet::new(),
            canonicalizations: BTreeMap::new(),
            units: BTreeMap::new(),
            quantities: BTreeMap::new(),
            reverse: BTreeMap::new(),
            prefixes: Vec::new(),
            definitions: BTreeMap::new(),
            docs: BTreeMap::new(),
            datepatterns: Vec::new(),
            substances: BTreeMap::new(),
            temporaries: RefCell::new(BTreeMap::new()),
            short_output: false,
            use_humanize: true,
            lmdb: None,
        }
    }

    pub fn load_dates(&mut self, mut dates: Vec<Vec<DatePattern>>) {
        self.datepatterns.append(&mut dates)
    }

    /// Given a unit name, returns its value if it exists. Supports SI
    /// prefixes, plurals, bare dimensions like length, and quantities.
    pub fn lookup(&self, name: &str) -> Option<Number> {
        fn inner(ctx: &Context, name: &str) -> Option<Number> {
            if let Some(v) = ctx.temporaries.borrow().get(name).cloned() {
                return Some(v)
            }
            if let Some(k) = ctx.dimensions.get(name) {
                return Some(Number::one_unit(k.to_owned()))
            }
            if let Some(v) = ctx.units.get(name).cloned() {
                return Some(v)
            }
            for (unit, quantity) in &ctx.quantities {
                if name == quantity {
                    return Some(Number {
                        value: Num::one(),
                        unit: unit.clone()
                    })
                }
            }
            None
        }
        if let Some(v) = inner(self, name) {
            return Some(v)
        }
        for &(ref pre, ref value) in &self.prefixes {
            if name.starts_with(pre) {
                if let Some(v) = inner(self, &name[pre.len()..]) {
                    return Some((&v * &value).unwrap())
                }
            }
        }
        // after so that "ks" is kiloseconds
        if name.ends_with("s") {
            let name = &name[0..name.len()-1];
            if let Some(v) = inner(self, name) {
                return Some(v)
            }
            for &(ref pre, ref value) in &self.prefixes {
                if name.starts_with(pre) {
                    if let Some(v) = inner(self, &name[pre.len()..]) {
                        return Some((&v * &value).unwrap())
                    }
                }
            }
        }
        None
    }

    /// Given a unit name, try to return a canonical name (expanding aliases and such)
    pub fn canonicalize(&self, name: &str) -> Option<String> {
        fn inner(ctx: &Context, name: &str) -> Option<String> {
            if let Some(v) = ctx.canonicalizations.get(name) {
                return Some(v.clone())
            }
            if let Some(k) = ctx.dimensions.get(name) {
                return Some((*k.0).clone())
            }
            if let Some(v) = ctx.definitions.get(name) {
                if let Expr::Unit(ref name) = *v {
                    if let Some(r) = ctx.canonicalize(&*name) {
                        return Some(r)
                    } else {
                        return Some(name.clone())
                    }
                } else {
                    // we cannot canonicalize it further
                    return Some(name.to_owned())
                }
            }
            None
        }
        if let Some(v) = inner(self, name) {
            return Some(v)
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
                    return Some(format!("{}{}", pre, v))
                }
            }
        }
        if name.ends_with("s") {
            let name = &name[0..name.len()-1];
            if let Some(v) = inner(self, name) {
                return Some(v)
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
                        return Some(format!("{}{}", pre, v))
                    }
                }
            }
        }
        None
    }

    /// Describes a value's unit, gives true if the unit is reciprocal
    /// (e.g. you should prefix "1.0 / " or replace "multiply" with
    /// "divide" when rendering it).
    pub fn describe_unit(&self, value: &Number) -> (bool, String) {
        use std::io::Write;

        let mut buf = vec![];
        let mut recip = false;
        let square = Number {
            value: Num::one(),
            unit: value.unit.clone()
        }.root(2).ok();
        let inverse = (&Number::one() / &Number {
            value: Num::one(),
            unit: value.unit.clone()
        }).unwrap();
        if let Some(name) = self.quantities.get(&value.unit) {
            write!(buf, "{}", name).unwrap();
        } else if let Some(name) = square.and_then(|square| self.quantities.get(&square.unit)) {
            write!(buf, "{}^2", name).unwrap();
        } else if let Some(name) = self.quantities.get(&inverse.unit) {
            recip = true;
            write!(buf, "{}", name).unwrap();
        } else {
            let mut frac = vec![];
            let mut found = false;
            for (dim, &pow) in &value.unit {
                if pow < 0 {
                    frac.push((dim, -pow));
                } else {
                    found = true;
                    let mut map = Unit::new();
                    map.insert(dim.clone(), pow);
                    if let Some(name) = self.quantities.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
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
                }
            }
            if frac.len() > 0 {
                if !found {
                    recip = true;
                } else {
                    write!(buf, " /").unwrap();
                }
                for (dim, pow) in frac {
                    let mut map = Unit::new();
                    map.insert(dim.clone(), pow);
                    if let Some(name) = self.quantities.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
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

    #[cfg(feature = "lmdb")]
    pub fn substance(
        &self, name: &str
    ) -> Result<Option<Substance>, String> {
        use lmdb::Transaction;
        use gnu_units;
        use ast;
        use std::str::from_utf8;

        let lmdb = match self.lmdb {
            Some(ref lmdb) => lmdb,
            None => return Ok(None)
        };
        let defs = {
            let tx = try!(lmdb.env.begin_ro_txn().map_err(|e| format!(
                "Failed to create read transaction: {}", e
            )));
            let res = match tx.get(lmdb.substances, &name) {
                Ok(res) => res,
                Err(_) => return Ok(None)
            };
            let res = try!(from_utf8(res).map_err(|e| format!(
                "Record is invalid utf-8: {}", e
            )));
            let mut iter = gnu_units::TokenIterator::new(res).peekable();
            let defs = gnu_units::parse(&mut iter);
            if defs.defs.len() != 1 {
                return Err(format!(
                    "Record contains more than one substance"
                ))
            }
            defs
        };
        let (name, def, _doc) = defs.defs.into_iter().next().unwrap();
        let props = match *def {
            ast::Def::Substance(ref props) => props,
            _ => return Err(format!(
                "Record is not a substance"
            ))
        };
        let sub = try!(self.eval_substance(props, name));
        Ok(Some(sub))
    }

    #[cfg(not(feature = "lmdb"))]
    pub fn substance(
        &self, _name: &str
    ) -> Result<Option<Substance>, String> {
        Ok(None)
    }
}
