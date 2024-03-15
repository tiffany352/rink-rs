// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::Context;
use crate::ast::{BinOpExpr, BinOpType, Def, DefEntry, Defs, Expr, UnaryOpExpr, UnaryOpType};
use crate::runtime::{Properties, Property, Substance, Value};
use crate::types::{BaseUnit, Dimensionality, Number, Numeric};
use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryInto;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
enum Namespace {
    Unit,
    Prefix,
    Quantity,
    Category,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
struct Id {
    namespace: Namespace,
    name: Rc<String>,
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.namespace {
            Namespace::Unit => write!(f, "unit {}", self.name),
            Namespace::Prefix => write!(f, "prefix {}-", self.name),
            Namespace::Quantity => write!(f, "quantity {}", self.name),
            Namespace::Category => write!(f, "category {}", self.name),
        }
    }
}

struct Resolver {
    interned: BTreeSet<Rc<String>>,
    input: BTreeMap<Id, Rc<Def>>,
    sorted: Vec<Id>,
    unmarked: BTreeSet<Id>,
    temp_marks: BTreeSet<Id>,
    docs: BTreeMap<Id, String>,
    categories: BTreeMap<Id, String>,
    errors: Vec<String>,
}

impl Resolver {
    // Doesn't seem to be a way to get a value in a BTreeSet<String> without having a String, it doesn't like &str.
    #[allow(clippy::ptr_arg)]
    fn intern(&mut self, name: &String) -> Rc<String> {
        if let Some(v) = self.interned.get(name).cloned() {
            v
        } else {
            let v = Rc::new(name.to_owned());
            self.interned.insert(v.clone());
            v
        }
    }

    fn lookup_exact(&mut self, name: &Rc<String>, context: Namespace) -> bool {
        let to_check: &[Namespace] = match context {
            Namespace::Quantity => &[Namespace::Quantity],
            _ => &[Namespace::Unit, Namespace::Prefix, Namespace::Quantity],
        };
        for namespace in to_check.iter().copied() {
            let id = Id {
                namespace,
                name: name.clone(),
            };
            if self.input.contains_key(&id) {
                self.visit(&id);
                return true;
            }
        }
        false
    }

    fn lookup_with_prefix(&mut self, name: &Rc<String>, context: Namespace) -> bool {
        if self.lookup_exact(name, context) {
            return true;
        }
        let mut found = vec![];
        for prefix in self.input.keys() {
            if prefix.namespace == Namespace::Prefix && name.starts_with(&prefix.name[..]) {
                found.push(prefix.clone());
            }
        }
        found.into_iter().any(|pre| {
            self.lookup_exact(&Rc::new(name[pre.name.len()..].to_owned()), context) && {
                self.visit(&pre);
                true
            }
        })
    }

    fn lookup(&mut self, name: &Rc<String>, context: Namespace) -> bool {
        self.lookup_with_prefix(name, context)
            || name.ends_with('s') && {
                let name = &Rc::new(name[0..name.len() - 1].to_owned());
                self.lookup_with_prefix(name, context)
            }
    }

    fn eval(&mut self, expr: &Expr, context: Namespace) {
        match *expr {
            Expr::Unit { ref name } => {
                let name = self.intern(name);
                self.lookup(&name, context);
            }
            Expr::BinOp(BinOpExpr {
                ref left,
                ref right,
                ..
            }) => {
                self.eval(left, context);
                self.eval(right, context);
            }
            Expr::UnaryOp(ref unaryop) => self.eval(&unaryop.expr, context),
            Expr::Of { ref expr, .. } => self.eval(expr, context),

            Expr::Mul { ref exprs }
            | Expr::Call {
                args: ref exprs, ..
            } => {
                for expr in exprs {
                    self.eval(expr, context);
                }
            }
            _ => (),
        }
    }

    fn visit(&mut self, id: &Id) {
        if self.temp_marks.get(id).is_some() {
            self.errors
                .push(format!("Unit {} has a dependency cycle", id));
            return;
        }
        if self.unmarked.get(id).is_some() {
            self.temp_marks.insert(id.clone());
            if let Some(v) = self.input.get(id).cloned() {
                match *v {
                    Def::Prefix { ref expr, .. }
                    | Def::Unit { ref expr }
                    | Def::Quantity { ref expr } => self.eval(expr, id.namespace),
                    Def::Substance { ref properties, .. } => {
                        for prop in properties {
                            self.eval(&prop.input, id.namespace);
                            self.eval(&prop.output, id.namespace);
                        }
                    }
                    _ => (),
                }
            }
            self.unmarked.remove(id);
            self.temp_marks.remove(id);
            self.sorted.push(id.clone());
        }
    }
}

fn eval_prefix(prefixes: &BTreeMap<String, Numeric>, expr: &Expr) -> Result<Numeric, String> {
    match expr {
        Expr::Const { ref value } => Ok(value.clone()),
        Expr::Unit { name: ref other } => {
            if let Some(value) = prefixes.get(other).cloned() {
                Ok(value)
            } else {
                Err(format!("References non-existent prefix {other}"))
            }
        }
        Expr::BinOp(BinOpExpr {
            op: BinOpType::Frac,
            left,
            right,
        }) => {
            let left = eval_prefix(prefixes, &*left)?;
            let right = eval_prefix(prefixes, &*right)?;
            Ok(&left / &right)
        }
        Expr::BinOp(BinOpExpr {
            op: BinOpType::Pow,
            left,
            right,
        }) => {
            let left = eval_prefix(prefixes, &*left)?;
            let right = eval_prefix(prefixes, &*right)?;
            let right: i32 = right
                .to_int()
                .and_then(|value| value.try_into().ok())
                .ok_or_else(|| "Exponent is too big".to_string())?;
            Ok(left.pow(right))
        }
        Expr::UnaryOp(UnaryOpExpr {
            op: UnaryOpType::Negative,
            expr,
        }) => {
            let value = eval_prefix(prefixes, &*expr)?;
            Ok(&value * &Numeric::from(-1))
        }
        ref expr => Err(format!("Not a numeric constant: {expr}")),
    }
}

fn eval_quantity(
    base_units: &BTreeSet<BaseUnit>,
    quantities: &BTreeMap<String, Dimensionality>,
    expr: &Expr,
) -> Result<Dimensionality, String> {
    match *expr {
        Expr::Unit { ref name } => {
            if let Some(base) = base_units.get(&name[..]) {
                Ok(Dimensionality::base_unit(base.clone()))
            } else if let Some(quantity) = quantities.get(&name[..]) {
                Ok(quantity.clone())
            } else {
                Err(format!("No quantity or base unit named {}", name))
            }
        }
        Expr::Const { ref value } if *value == Numeric::one() => Ok(Dimensionality::default()),
        Expr::Mul { ref exprs } => {
            exprs
                .iter()
                .fold(Ok(Dimensionality::default()), |acc, value| {
                    let acc = acc?;
                    let value = eval_quantity(base_units, quantities, value)?;
                    Ok(&acc * &value)
                })
        }
        Expr::BinOp(BinOpExpr {
            op: BinOpType::Frac,
            ref left,
            ref right,
        }) => {
            let left = eval_quantity(base_units, quantities, &*left)?;
            let right = eval_quantity(base_units, quantities, &*right)?;
            Ok(&left / &right)
        }
        Expr::BinOp(BinOpExpr {
            op: BinOpType::Pow,
            ref left,
            ref right,
        }) => {
            let left = eval_quantity(base_units, quantities, &*left)?;
            match **right {
                Expr::Const { ref value } => {
                    let value = value
                        .to_int()
                        .ok_or_else(|| "RHS of `^` is too big".to_string())?;
                    Ok(left.pow(value))
                }
                Expr::UnaryOp(UnaryOpExpr {
                    op: UnaryOpType::Negative,
                    ref expr,
                }) => {
                    if let Expr::Const { ref value } = **expr {
                        let value = -value
                            .to_int()
                            .ok_or_else(|| "RHS of `^` is too big".to_string())?;
                        Ok(left.pow(value))
                    } else {
                        Err(format!("RHS of `^` must be a constant: {expr}"))
                    }
                }
                _ => Err(format!("RHS of `^` must be a constant: {expr}")),
            }
        }
        Expr::UnaryOp(UnaryOpExpr {
            op: UnaryOpType::Negative,
            ref expr,
        }) => Ok(eval_quantity(base_units, quantities, &*expr)?.recip()),
        ref expr => Err(format!("Invalid expression in quantity: {expr}")),
    }
}

pub(crate) fn load_defs(ctx: &mut Context, defs: Defs) -> Vec<String> {
    let mut resolver = Resolver {
        interned: BTreeSet::new(),
        input: BTreeMap::new(),
        sorted: vec![],
        unmarked: BTreeSet::new(),
        temp_marks: BTreeSet::new(),
        docs: BTreeMap::new(),
        categories: BTreeMap::new(),
        errors: Vec::new(),
    };
    for DefEntry {
        name,
        def,
        doc,
        category,
    } in defs.defs.into_iter()
    {
        // Base units can have two different names from one Def, handle that here.
        if let Def::BaseUnit {
            long_name: Some(ref long_name),
        } = *def
        {
            let long_name = resolver.intern(long_name);
            resolver.input.insert(
                Id {
                    namespace: Namespace::Unit,
                    name: long_name,
                },
                def.clone(),
            );
        }

        let name = resolver.intern(&name);
        let id = match *def {
            Def::Prefix { .. } => Id {
                namespace: Namespace::Prefix,
                name,
            },
            Def::Quantity { .. } => Id {
                namespace: Namespace::Quantity,
                name,
            },
            Def::Category { .. } => Id {
                namespace: Namespace::Category,
                name,
            },
            _ => Id {
                namespace: Namespace::Unit,
                name,
            },
        };
        if let Some(doc) = doc {
            resolver.docs.insert(id.clone(), doc);
        }
        if let Some(category) = category {
            // for now, only allow units to have categories.
            if id.namespace == Namespace::Unit {
                resolver.categories.insert(id.clone(), category);
            }
        }
        if resolver.input.insert(id.clone(), def).is_some() {
            let namespace = match id.namespace {
                Namespace::Prefix => "prefixes",
                Namespace::Quantity => "quantities",
                Namespace::Unit => "units",
                Namespace::Category => "categories",
            };
            if id.namespace != Namespace::Category {
                resolver
                    .errors
                    .push(format!("warning: multiple {} named {}", namespace, id.name));
            }
        }
        resolver.unmarked.insert(id);
    }

    while let Some(name) = resolver.unmarked.iter().next().cloned() {
        resolver.visit(&name)
    }
    let sorted = resolver.sorted;
    let mut input = resolver.input;
    let udefs = sorted.into_iter().map(move |name| {
        let res = input.remove(&name).unwrap();
        (name, res)
    });

    let mut decomposition_units = BTreeSet::new();
    decomposition_units.insert("newton");
    decomposition_units.insert("pascal");
    decomposition_units.insert("joule");
    decomposition_units.insert("watt");
    decomposition_units.insert("coulomb");
    decomposition_units.insert("volt");
    decomposition_units.insert("ohm");
    decomposition_units.insert("siemens");
    decomposition_units.insert("farad");
    decomposition_units.insert("weber");
    decomposition_units.insert("henry");
    decomposition_units.insert("tesla");
    decomposition_units.insert("lumen");
    decomposition_units.insert("lux");
    decomposition_units.insert("gray");
    decomposition_units.insert("katal");

    let mut prefix_lookup = BTreeMap::new();
    let mut quantities = BTreeMap::new();

    for (id, def) in udefs {
        let name = id.name.to_string();
        match *def {
            Def::BaseUnit { ref long_name } => {
                let unit = BaseUnit::new(&*name);
                ctx.registry.base_units.insert(unit.clone());
                if let Some(long_name) = long_name {
                    ctx.registry
                        .base_unit_long_names
                        .insert(name.clone(), long_name.clone());
                    ctx.registry
                        .definitions
                        .insert(long_name.clone(), Expr::new_unit(name.clone()));
                    ctx.registry
                        .units
                        .insert(long_name.clone(), Number::one_unit(unit));
                }
            }
            Def::Unit { ref expr } => match ctx.eval(expr) {
                Ok(Value::Number(v)) => {
                    // This is one of the SI derived units, so put it in the map.
                    if v.value == Numeric::one() && decomposition_units.contains(&*name) {
                        ctx.registry
                            .decomposition_units
                            .insert(v.unit.clone(), name.clone());
                    }
                    ctx.registry
                        .definitions
                        .insert(name.clone(), expr.0.clone());
                    ctx.registry.units.insert(name.clone(), v);
                }
                Ok(Value::Substance(sub)) => {
                    let sub = if sub.properties.name.contains('+') {
                        sub.rename(name.clone())
                    } else {
                        sub
                    };
                    if ctx.registry.substances.insert(name.clone(), sub).is_some() {
                        resolver
                            .errors
                            .push(format!("Warning: Conflicting substances for {}", id));
                    }
                }
                Ok(_) => resolver.errors.push(format!("{} is not a number", id)),
                Err(e) => resolver.errors.push(format!("{} is malformed: {}", id, e)),
            },
            Def::Prefix { ref expr, is_long } => match eval_prefix(&prefix_lookup, &expr.0) {
                Ok(value) => {
                    prefix_lookup.insert(name.clone(), value.clone());
                    ctx.registry.prefixes.push((name.clone(), value.clone()));
                    if is_long {
                        ctx.registry
                            .units
                            .insert(name.clone(), Number::new(value.clone()));
                    }
                }
                Err(err) => resolver.errors.push(format!("Prefix {name}: {err}")),
            },
            Def::Quantity { ref expr } => {
                match eval_quantity(&ctx.registry.base_units, &quantities, &expr.0) {
                    Ok(dimensionality) => {
                        quantities.insert(name.clone(), dimensionality.clone());
                        let res = ctx
                            .registry
                            .quantities
                            .insert(dimensionality.clone(), name.clone());
                        if !ctx.registry.definitions.contains_key(&name) {
                            ctx.registry
                                .definitions
                                .insert(name.clone(), expr.0.clone());
                        }
                        if let Some(old) = res {
                            resolver.errors.push(format!(
                                "Warning: Conflicting quantities {} and {}",
                                name, old
                            ));
                        }
                    }
                    Err(err) => resolver.errors.push(format!("Quantity {name}: {err}")),
                }
            }
            Def::Substance {
                ref properties,
                ref symbol,
            } => {
                // Needed because fields of structs can't be individually closed over.
                let errors = &mut resolver.errors;
                let mut prev = BTreeMap::new();
                let res = properties
                    .iter()
                    .map(|prop| {
                        let input = match ctx.eval(&prop.input) {
                            Ok(Value::Number(v)) => v,
                            Ok(x) => {
                                return Err(format!(
                                    "Expected number for input of \
                                         property {}, got {:?}",
                                    name, x
                                ))
                            }
                            Err(e) => {
                                return Err(format!("Malformed property input for {}: {}", name, e))
                            }
                        };
                        let output = match ctx.eval(&prop.output) {
                            Ok(Value::Number(v)) => v,
                            Ok(x) => {
                                return Err(format!(
                                    "Expected number for output of \
                                         property {}, got {:?}",
                                    name, x
                                ))
                            }
                            Err(e) => {
                                return Err(format!(
                                    "Malformed property output for {}: {}",
                                    name, e
                                ))
                            }
                        };
                        let mut unique = BTreeSet::new();
                        unique.insert(&*prop.name);
                        unique.insert(&*prop.input_name);
                        unique.insert(&*prop.output_name);
                        let unit = (&input / &output).expect("Non-zero property").unit;
                        let existing = prev.entry(unit).or_insert_with(BTreeSet::new);
                        for conflict in existing.intersection(&unique) {
                            errors.push(format!(
                                "Warning: conflicting \
                                     properties for {} of {}",
                                conflict, name
                            ));
                        }
                        existing.append(&mut unique);
                        ctx.temporaries.insert(
                            prop.name.clone(),
                            (&input / &output).expect("Non-zero property"),
                        );
                        if output == Number::one() {
                            ctx.temporaries
                                .insert(prop.input_name.clone(), input.clone());
                        }
                        if input == Number::one() {
                            ctx.temporaries
                                .insert(prop.output_name.clone(), output.clone());
                        }
                        Ok((
                            prop.name.clone(),
                            Property {
                                input,
                                input_name: prop.input_name.clone(),
                                output,
                                output_name: prop.output_name.clone(),
                                doc: prop.doc.clone(),
                            },
                        ))
                    })
                    .collect::<Result<BTreeMap<_, _>, _>>();
                ctx.temporaries.clear();
                match res {
                    Ok(res) => {
                        ctx.registry.substances.insert(
                            name.clone(),
                            Substance {
                                amount: Number::one(),
                                properties: Arc::new(Properties {
                                    name: name.clone(),
                                    properties: res,
                                }),
                            },
                        );
                        if let Some(ref symbol) = symbol {
                            ctx.registry
                                .substance_symbols
                                .insert(symbol.clone(), name.clone());
                        }
                    }
                    Err(e) => resolver
                        .errors
                        .push(format!("Substance {} is malformed: {}", name, e)),
                }
            }
            Def::Category { ref display_name } => {
                ctx.registry
                    .category_names
                    .insert(name.clone(), display_name.clone());
            }
            Def::Error { ref message } => {
                resolver.errors.push(format!("Def {}: {}", name, message))
            }
        };
    }

    for (id, val) in resolver.docs {
        let name = id.name.to_string();
        if ctx.registry.docs.insert(name, val).is_some() {
            resolver.errors.push(format!("Doc conflict for {}", id));
        }
    }

    for (id, category_name) in resolver.categories {
        let name = id.name.to_string();
        if let Some(existing_category) = ctx
            .registry
            .categories
            .insert(name.clone(), category_name.clone())
        {
            resolver.errors.push(format!(
                "Category conflict: {id} is in both {category_name} and {existing_category}"
            ));
        }
    }

    resolver.errors
}
