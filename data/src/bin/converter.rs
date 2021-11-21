use std::{collections::HashMap, fs::File, io::Write, path::PathBuf};

use kdl::{KdlNode, KdlValue};
use rink_core::{
    self,
    ast::{Def, DefEntry, Expr, ExprString},
    gnu_units,
};

#[derive(Ord, PartialOrd, PartialEq, Eq)]
enum DefType {
    Dimension,
    Quantity,
    Prefix,
    SPrefix,
    Unit,
    Substance,
    Category,
    Canonicalization,
    Error,
}

impl DefType {
    fn from(def: &DefEntry) -> DefType {
        match *def.def {
            Def::Dimension => DefType::Dimension,
            Def::Canonicalization { .. } => DefType::Canonicalization,
            Def::Prefix { .. } => DefType::Prefix,
            Def::SPrefix { .. } => DefType::SPrefix,
            Def::Unit { .. } => DefType::Unit,
            Def::Quantity { .. } => DefType::Quantity,
            Def::Substance { .. } => DefType::Substance,
            Def::Category { .. } => DefType::Category,
            Def::Error { .. } => DefType::Error,
        }
    }
}

fn as_alias(def: &DefEntry) -> Option<&str> {
    match *def.def {
        Def::SPrefix {
            expr: ExprString(Expr::Unit { ref name }),
        } => Some(name),
        Def::Unit {
            expr: ExprString(Expr::Unit { ref name }),
        } => Some(name),
        _ => None,
    }
}

fn main() {
    let defs = gnu_units::parse_str(rink_core::DEFAULT_FILE.unwrap());

    let mut canonicalizations = HashMap::new();
    let mut aliases: HashMap<String, Vec<String>> = HashMap::new();
    let mut categories = vec![];
    let mut prefix_aliases: HashMap<String, Vec<String>> = HashMap::new();
    let mut short_prefixes = vec![];

    for def in &defs.defs {
        if let Some(of) = as_alias(def) {
            aliases
                .entry(of.to_owned())
                .or_default()
                .push(def.name.clone());
            continue;
        }
        match *def.def {
            Def::Canonicalization { ref of } => {
                canonicalizations.insert(of.clone(), def.name.clone());
            }
            Def::Prefix {
                expr: ExprString(Expr::Unit { ref name }),
            } => prefix_aliases
                .entry(name.clone())
                .or_default()
                .push(def.name.clone()),
            Def::SPrefix { ref expr } => {
                short_prefixes.push((def, expr));
            }
            Def::Category { ref display_name } => {
                let mut contents = vec![];
                for def2 in &defs.defs {
                    if def2.category.as_ref() == Some(&def.name) && as_alias(def2).is_none() {
                        contents.push(def2.clone());
                    }
                }
                categories.push((def, display_name, contents));
            }
            Def::Error { ref message } => panic!("Definitions file had error: {}", message),
            _ => {}
        }
    }

    let create_children = |def: &DefEntry| -> (Vec<KdlNode>, String) {
        let mut children = vec![];
        let full_name = if let Some(full_name) = canonicalizations.get(&def.name) {
            children.push(KdlNode {
                name: "short".to_owned(),
                values: vec![def.name.to_owned().into()],
                properties: HashMap::new(),
                children: vec![],
            });
            full_name.clone()
        } else {
            def.name.clone()
        };
        if let Some(aliases) = aliases.get(&def.name) {
            for alias in aliases {
                children.push(KdlNode {
                    name: "alias".to_owned(),
                    values: vec![alias.clone().into()],
                    properties: HashMap::new(),
                    children: vec![],
                });
            }
        }
        if let Some(doc) = &def.doc {
            for line in doc.lines() {
                children.push(KdlNode {
                    name: "doc".to_owned(),
                    values: vec![line.to_owned().into()],
                    properties: {
                        let mut properties = HashMap::new();
                        properties.insert("lang".to_owned(), KdlValue::String("en".to_owned()));
                        properties
                    },
                    children: vec![],
                });
            }
        }

        (children, full_name)
    };

    let def_to_kdl = |def: &DefEntry| -> Option<KdlNode> {
        let (mut children, full_name) = create_children(def);
        let mut properties = HashMap::new();
        let (name, values) = match *def.def {
            Def::Dimension => ("base_unit", vec![full_name.into()]),
            Def::Canonicalization { .. } => return None,
            // Prefixes that are simple aliases of another prefix are inlined.
            Def::Prefix {
                expr: ExprString(Expr::Unit { .. }),
            } => return None,
            Def::Prefix { ref expr } => {
                properties.insert("short".to_owned(), full_name.into());
                ("prefix", vec![String::from(expr.clone()).into()])
            }
            Def::SPrefix { ref expr } => {
                if let Some(aliases) = prefix_aliases.get(&def.name) {
                    if aliases.len() > 1 {
                        panic!(
                            "prefix {} has {} short prefixes ({:?}), more than the 1 allowed",
                            def.name,
                            aliases.len(),
                            aliases
                        );
                    }
                    if !aliases.is_empty() {
                        properties.insert("short".to_owned(), aliases[0].clone().into());
                    }
                }
                properties.insert("long".to_owned(), full_name.into());
                ("prefix", vec![String::from(expr.clone()).into()])
            }
            Def::Unit { ref expr } => (
                "unit",
                vec![full_name.into(), String::from(expr.clone()).into()],
            ),
            Def::Quantity { ref expr } => (
                "quantity",
                vec![full_name.into(), String::from(expr.clone()).into()],
            ),
            Def::Substance {
                ref symbol,
                ref properties,
            } => {
                for property in properties {
                    let mut prop_children = vec![];
                    if let Some(ref doc) = property.doc {
                        prop_children.push(KdlNode {
                            name: "doc".to_owned(),
                            values: vec![doc.clone().into()],
                            properties: HashMap::new(),
                            children: vec![],
                        });
                    }
                    children.push(KdlNode {
                        name: "prop".to_owned(),
                        values: vec![
                            property.name.clone().into(),
                            property.input_name.clone().into(),
                            String::from(property.input.clone()).into(),
                            property.output_name.clone().into(),
                            String::from(property.output.clone()).into(),
                        ],
                        properties: HashMap::new(),
                        children: prop_children,
                    })
                }
                let mut values = vec![full_name.into()];
                if let Some(symbol) = symbol {
                    values.push(symbol.clone().into());
                }
                ("substance", values)
            }
            Def::Category { .. } => return None,
            Def::Error { ref message } => panic!("{}", message),
        };
        let name = name.to_owned();
        Some(KdlNode {
            name,
            values,
            children,
            properties,
        })
    };

    let create_category =
        |cat_def: &DefEntry, display_name: &String, mut extra_children: Vec<KdlNode>| -> KdlNode {
            let (mut children, _) = create_children(cat_def);
            children.push(KdlNode {
                name: "title".to_owned(),
                values: vec![display_name.clone().into()],
                properties: {
                    let mut properties = HashMap::new();
                    properties.insert("lang".to_owned(), KdlValue::String("en".to_owned()));
                    properties
                },
                children: vec![],
            });
            children.append(&mut extra_children);
            KdlNode {
                name: "category".to_owned(),
                values: vec![cat_def.name.clone().into()],
                properties: HashMap::new(),
                children,
            }
        };

    std::fs::create_dir_all("categories").unwrap();

    for (cat_def, display_name, mut contents) in categories {
        if contents.is_empty() {
            continue;
        }
        contents.sort_by_key(|def| (DefType::from(def), &def.name));

        let mut path = PathBuf::new();
        path.push("categories");
        path.push(&cat_def.name);
        path.set_extension("kdl");
        let mut file = File::create(&path).unwrap();

        let string = format!(
            "{}",
            create_category(
                cat_def,
                display_name,
                contents.into_iter().filter_map(def_to_kdl).collect()
            )
        )
        // String replace hack required because of a bug in kdl-rs.
        .replace("\\/", "/");
        writeln!(file, "{}", string).unwrap();
    }

    {
        let mut file = File::create("_uncategorized.kdl").unwrap();

        let mut contents = defs
            .defs
            .iter()
            .filter(|def| def.category.is_none() && as_alias(def).is_none())
            .collect::<Vec<_>>();
        contents.sort_by_key(|def| (DefType::from(def), &def.name));

        for def in contents {
            if let Some(node) = def_to_kdl(def) {
                let string = format!("{}", node).replace("\\/", "/");
                writeln!(file, "{}", string).unwrap();
            }
        }
    }
}
