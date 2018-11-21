// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::{BTreeMap, BTreeSet};
use number::{Number, Dim};
use num::Num;
use ast::{Expr, Def, Defs, DefEntry};
use substance::{Substance, Property, Properties};
use std::rc::Rc;
use value::Value;
use Context;

impl Context {
    /// Takes a parsed definitions.units from
    /// `gnu_units::parse()`. Prints if there are errors in the file.
    pub fn load(&mut self, defs: Defs) {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
        enum Name {
            Unit(Rc<String>),
            Prefix(Rc<String>),
            Quantity(Rc<String>),
            Category(Rc<String>),
        }

        struct Resolver {
            interned: BTreeSet<Rc<String>>,
            input: BTreeMap<Name, Rc<Def>>,
            sorted: Vec<Name>,
            unmarked: BTreeSet<Name>,
            temp_marks: BTreeSet<Name>,
            docs: BTreeMap<Name, String>,
            categories: BTreeMap<Name, String>,
        }

        impl Resolver {
            fn intern(&mut self, name: &String) -> Rc<String> {
                if let Some(v) = self.interned.get(name).cloned() {
                    v
                } else {
                    let v = Rc::new(name.to_owned());
                    self.interned.insert(v.clone());
                    v
                }
            }

            fn lookup(&mut self, name: &Rc<String>) -> bool {
                fn inner(ctx: &mut Resolver, name: &Rc<String>) -> bool {
                    [Name::Unit, Name::Prefix, Name::Quantity].iter().any(|f| {
                        let unit = f(name.clone());
                        ctx.input.contains_key(&unit) && {
                            ctx.visit(&unit);
                            true
                        }
                    })
                }

                let mut outer = |name: &Rc<String>| -> bool {
                    if inner(self, name) {
                        return true;
                    }
                    let mut found = vec![];
                    for pre in self.input.keys() {
                        if let Name::Prefix(ref pre) = *pre {
                            if (*name).starts_with(&**pre) {
                                found.push(pre.clone());
                            }
                        }
                    }
                    found.into_iter().any(|pre| {
                        inner(self, &Rc::new(name[pre.len()..].to_owned())) && {
                            let unit = Name::Prefix(pre);
                            self.visit(&unit);
                            true
                        }
                    })
                };

                outer(name) || name.ends_with('s') && {
                    let name = &Rc::new(name[0..name.len()-1].to_owned());
                    outer(name)
                }
            }

            fn eval(&mut self, expr: &Expr) {
                match *expr {
                    Expr::Unit(ref name) => {
                        let name = self.intern(name);
                        self.lookup(&name);
                    },
                    Expr::Frac(ref left, ref right) |
                    Expr::Pow(ref left, ref right) |
                    Expr::Add(ref left, ref right) |
                    Expr::Sub(ref left, ref right) => {
                        self.eval(left);
                        self.eval(right);
                    },
                    Expr::Neg(ref expr) | Expr::Plus(ref expr) |
                    Expr::Suffix(_, ref expr) | Expr::Of(_, ref expr) =>
                        self.eval(expr),
                    Expr::Mul(ref exprs) | Expr::Call(_, ref exprs) => for expr in exprs {
                        self.eval(expr);
                    },
                    _ => ()
                }
            }

            fn visit(&mut self, name: &Name) {
                if self.temp_marks.get(name).is_some() {
                    println!("Unit {:?} has a dependency cycle", name);
                    return;
                }
                if self.unmarked.get(name).is_some() {
                    self.temp_marks.insert(name.clone());
                    if let Some(v) = self.input.get(name).cloned() {
                        match *v {
                            Def::Prefix(ref e) | Def::SPrefix(ref e) | Def::Unit(ref e) |
                            Def::Quantity(ref e) =>
                                self.eval(e),
                            Def::Canonicalization(ref e) => {
                                self.lookup(&Rc::new(e.clone()));
                            },
                            Def::Substance { ref properties, .. } => {
                                for prop in properties {
                                    self.eval(&prop.input);
                                    self.eval(&prop.output);
                                }
                            },
                            _ => (),
                        }
                    }
                    self.unmarked.remove(name);
                    self.temp_marks.remove(name);
                    self.sorted.push(name.clone());
                }
            }
        }

        let mut resolver = Resolver {
            interned: BTreeSet::new(),
            input: BTreeMap::new(),
            sorted: vec![],
            unmarked: BTreeSet::new(),
            temp_marks: BTreeSet::new(),
            docs: BTreeMap::new(),
            categories: BTreeMap::new(),
        };
        for DefEntry { name, def, doc, category } in defs.defs.into_iter() {
            let name = resolver.intern(&name);
            let unit = match *def {
                Def::Prefix(_) | Def::SPrefix(_) => Name::Prefix(name),
                Def::Quantity(_) => Name::Quantity(name),
                Def::Category(_) => Name::Category(name),
                _ => Name::Unit(name)
            };
            if let Some(doc) = doc {
                resolver.docs.insert(unit.clone(), doc);
            }
            if let Some(category) = category {
                resolver.categories.insert(unit.clone(), category);
            }
            if resolver.input.insert(unit.clone(), def).is_some() {
                let (ty, name) = match unit {
                    Name::Prefix(ref name) => ("prefixes", name),
                    Name::Quantity(ref name) => ("quantities", name),
                    Name::Unit(ref name) => ("units", name),
                    Name::Category(ref name) => ("category", name),
                };
                if ty != "category" {
                    println!("warning: multiple {} named {}", ty, name);
                }
            }
            resolver.unmarked.insert(unit);
        }

        while let Some(name) = resolver.unmarked.iter().next().cloned() {
            resolver.visit(&name)
        }
        let sorted = resolver.sorted;
        //println!("{:#?}", sorted);
        let mut input = resolver.input;
        let udefs = sorted.into_iter().map(move |name| {
            let res = input.remove(&name).unwrap();
            (name, res)
        });

        let mut reverse = BTreeSet::new();
        reverse.insert("newton");
        reverse.insert("pascal");
        reverse.insert("joule");
        reverse.insert("watt");
        reverse.insert("coulomb");
        reverse.insert("volt");
        reverse.insert("ohm");
        reverse.insert("siemens");
        reverse.insert("farad");
        reverse.insert("weber");
        reverse.insert("henry");
        reverse.insert("tesla");
        reverse.insert("lumen");
        reverse.insert("lux");
        reverse.insert("gray");
        reverse.insert("katal");

        for (name, def) in udefs {
            let name = match name {
                Name::Unit(name) => (*name).clone(),
                Name::Prefix(name) => (*name).clone(),
                Name::Quantity(name) => (*name).clone(),
                Name::Category(name) => (*name).clone(),
            };
            match *def {
                Def::Dimension => {
                    self.dimensions.insert(Dim::new(&*name));
                },
                Def::Canonicalization(ref of) => {
                    self.canonicalizations.insert(of.clone(), name.clone());
                    match self.lookup(of) {
                        Some(v) => {
                            self.definitions.insert(name.clone(), Expr::Unit(of.clone()));
                            self.units.insert(name.clone(), v);
                        },
                        None => println!("Canonicalization {} is malformed: {} not found", name, of)
                    }
                },
                Def::Unit(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        if v.value == Num::one() && reverse.contains(&*name) {
                            self.reverse.insert(v.unit.clone(), name.clone());
                        }
                        self.definitions.insert(name.clone(), expr.clone());
                        self.units.insert(name.clone(), v);
                    },
                    Ok(Value::Substance(sub)) => {
                        let sub = if sub.properties.name.contains("+") {
                            sub.rename(name.clone())
                        } else {
                            sub
                        };
                        if self.substances.insert(name.clone(), sub).is_some() {
                            println!("Warning: Conflicting substances for {}", name);
                        }
                    },
                    Ok(_) => println!("Unit {} is not a number", name),
                    Err(e) => println!("Unit {} is malformed: {}", name, e)
                },
                Def::Prefix(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        self.prefixes.push((name.clone(), v));
                    },
                    Ok(_) => println!("Prefix {} is not a number", name),
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::SPrefix(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        self.prefixes.push((name.clone(), v.clone()));
                        self.units.insert(name.clone(), v);
                    },
                    Ok(_) => println!("Prefix {} is not a number", name),
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::Quantity(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        let res = self.quantities.insert(v.unit, name.clone());
                        if !self.definitions.contains_key(&name) {
                            self.definitions.insert(name.clone(), expr.clone());
                        }
                        if let Some(old) = res {
                            println!("Warning: Conflicting quantities {} and {}", name, old);
                        }
                    },
                    Ok(_) => println!("Quantity {} is not a number", name),
                    Err(e) => println!("Quantity {} is malformed: {}", name, e)
                },
                Def::Substance { ref properties, ref symbol } => {
                    let mut prev = BTreeMap::new();
                    let res = properties.iter().map(|prop| {
                        let input = match self.eval(&prop.input) {
                            Ok(Value::Number(v)) => v,
                            Ok(x) => return Err(format!(
                                "Expected number for input of \
                                 property {}, got {:?}", name, x)),
                            Err(e) => return Err(format!(
                                "Malformed property input for {}: {}",
                                name, e)),
                        };
                        let output = match self.eval(&prop.output) {
                            Ok(Value::Number(v)) => v,
                            Ok(x) => return Err(format!(
                                "Expected number for output of \
                                 property {}, got {:?}", name, x)),
                            Err(e) => return Err(format!(
                                "Malformed property output for {}: {}",
                                name, e)),
                        };
                        let mut unique = BTreeSet::new();
                        unique.insert(&*prop.name);
                        unique.insert(&*prop.input_name);
                        unique.insert(&*prop.output_name);
                        let unit = (&input / &output)
                            .expect("Non-zero property")
                            .unit;
                        let existing = prev.entry(unit).or_insert(BTreeSet::new());
                        for conflict in existing.intersection(&unique) {
                            println!(
                                "Warning: conflicting \
                                 properties for {} of {}",
                                conflict, name
                            );
                        }
                        existing.append(&mut unique);
                        self.temporaries.insert(
                            prop.name.clone(),
                            (&input / &output)
                                .expect("Non-zero property")
                        );
                        if output == Number::one() {
                            self.temporaries.insert(
                                prop.input_name.clone(),
                                input.clone()
                            );
                        }
                        if input == Number::one() {
                            self.temporaries.insert(
                                prop.output_name.clone(),
                                output.clone()
                            );
                        }
                        Ok((prop.name.clone(), Property {
                            input,
                            input_name: prop.input_name.clone(),
                            output,
                            output_name: prop.output_name.clone(),
                            doc: prop.doc.clone(),
                        }))
                    }).collect::<Result<BTreeMap<_,_>, _>>();
                    self.temporaries.clear();
                    match res {
                        Ok(res) => {
                            self.substances.insert(name.clone(), Substance {
                                amount: Number::one(),
                                properties: Rc::new(Properties {
                                    name: name.clone(),
                                    properties: res,
                                }),
                            });
                            if let &Some(ref symbol) = symbol {
                                self.substance_symbols.insert(symbol.clone(), name.clone());
                            }
                        },
                        Err(e) => println!("Substance {} is malformed: {}", name, e),
                    }
                },
                Def::Category(ref desc) => {
                    self.category_names.insert(name.clone(), desc.clone());
                },
                Def::Error(ref err) => println!("Def {}: {}", name, err),
            };
        }

        for (name, val) in resolver.docs {
            let name = match name {
                Name::Unit(name) => (*name).clone(),
                Name::Prefix(name) => (*name).clone(),
                Name::Quantity(name) => (*name).clone(),
                Name::Category(name) => (*name).clone(),
            };
            if self.docs.insert(name.clone(), val).is_some() {
                println!("Doc conflict for {}", name);
            }
        }

        for (name, val) in resolver.categories {
            let name = match name {
                Name::Unit(name) => (*name).clone(),
                Name::Prefix(name) => (*name).clone(),
                Name::Quantity(name) => (*name).clone(),
                Name::Category(name) => (*name).clone(),
            };
            if self.categories.insert(name.clone(), val).is_some() {
                println!("Category conflict for {}", name);
            }
        }
    }
}
