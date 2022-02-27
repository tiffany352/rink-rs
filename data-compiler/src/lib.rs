mod ast;
mod error;
mod matcher;
mod resolver;

use std::{collections::HashMap, fs};

use ast::*;
use matcher::*;
use rink_format::{BaseUnitId, CategoryId, Dimensionality, Documentation, UnitsData};

pub use error::Error;

use crate::resolver::{resolve_dependencies, DependencyNode};

struct DocsBuilder {
    title_by_lang: HashMap<String, String>,
    doc_by_lang: HashMap<String, String>,
    aliases: Vec<String>,
}

impl DocsBuilder {
    fn new() -> DocsBuilder {
        DocsBuilder {
            title_by_lang: HashMap::new(),
            doc_by_lang: HashMap::new(),
            aliases: vec![],
        }
    }

    fn add_title(&mut self, node: Title) -> Result<(), Error> {
        self.title_by_lang.insert(node.lang, node.text);
        Ok(())
    }

    fn add_doc(&mut self, node: Doc) -> Result<(), Error> {
        let doc = self.doc_by_lang.entry(node.lang).or_default();
        if doc.is_empty() {
            *doc = node.text;
        } else {
            doc.push('\n');
            doc.push_str(&node.text[..]);
        }
        Ok(())
    }

    fn add_alias(&mut self, node: Alias) -> Result<(), Error> {
        self.aliases.push(node.0);
        Ok(())
    }

    fn build(self, category: Option<CategoryId>) -> Documentation {
        Documentation {
            title_by_lang: self.title_by_lang,
            doc_by_lang: self.doc_by_lang,
            category,
            aliases: self.aliases,
        }
    }
}

struct Context {
    base_units: Vec<(BaseUnit, Documentation)>,
    quantities: Vec<(Quantity, Documentation)>,
    prefixes: Vec<(Prefix, Documentation)>,
    categories: Vec<(Category, Documentation)>,
    units: Vec<(Unit, Documentation)>,
    substances: Vec<(Substance, Vec<Property>, Documentation)>,
}

fn process_docs(children: Vec<Node>, category: Option<CategoryId>) -> Result<Documentation, Error> {
    let mut builder = DocsBuilder::new();
    for child in children {
        match child.data {
            NodeData::Title(node) => builder.add_title(node)?,
            NodeData::Doc(node) => builder.add_doc(node)?,
            NodeData::Alias(node) => builder.add_alias(node)?,
            node => {
                return Err(Error::UnexpectedNode {
                    parent: Substance::NAME,
                    child: node.name(),
                });
            }
        }
    }
    Ok(builder.build(category))
}

fn visit_substance(
    substance: Substance,
    children: Vec<Node>,
    category: Option<CategoryId>,
) -> Result<(Substance, Vec<Property>, Documentation), Error> {
    let mut docs_builder = DocsBuilder::new();
    let mut properties = vec![];

    for node in children {
        match node.data {
            NodeData::Title(node) => docs_builder.add_title(node)?,
            NodeData::Doc(node) => docs_builder.add_doc(node)?,
            NodeData::Alias(node) => docs_builder.add_alias(node)?,
            NodeData::Property(node) => properties.push(node),
            node => {
                return Err(Error::UnexpectedNode {
                    parent: Substance::NAME,
                    child: node.name(),
                });
            }
        }
    }

    Ok((substance, properties, docs_builder.build(category)))
}

fn visit_def(
    node: Node,
    ctx: &mut Context,
    category: Option<CategoryId>,
    parent: &'static str,
) -> Result<(), Error> {
    match node.data {
        NodeData::Unit(data) => ctx
            .units
            .push((data, process_docs(node.children, category)?)),
        NodeData::BaseUnit(data) => ctx
            .base_units
            .push((data, process_docs(node.children, category)?)),
        NodeData::Prefix(data) => ctx
            .prefixes
            .push((data, process_docs(node.children, category)?)),
        NodeData::Quantity(data) => ctx
            .quantities
            .push((data, process_docs(node.children, category)?)),
        NodeData::Substance(data) => {
            ctx.substances
                .push(visit_substance(data, node.children, category)?)
        }
        node => {
            return Err(Error::UnexpectedNode {
                parent,
                child: node.name(),
            });
        }
    }
    Ok(())
}

fn visit_category(category: Category, children: Vec<Node>, ctx: &mut Context) -> Result<(), Error> {
    let id = CategoryId(ctx.categories.len() as u32);
    let mut docs_builder = DocsBuilder::new();

    for node in children {
        match node.data {
            NodeData::Title(node) => docs_builder.add_title(node)?,
            NodeData::Doc(node) => docs_builder.add_doc(node)?,
            NodeData::Alias(node) => docs_builder.add_alias(node)?,
            _ => visit_def(node, ctx, Some(id), Category::NAME)?,
        }
    }

    ctx.categories.push((category, docs_builder.build(None)));
    Ok(())
}

fn visit_top(nodes: Vec<Node>, ctx: &mut Context) -> Result<(), Error> {
    for node in nodes {
        match node.data {
            NodeData::Category(category) => visit_category(category, node.children, ctx)?,
            _ => visit_def(node, ctx, None, "document")?,
        }
    }

    Ok(())
}

fn process_quantities(
    quantities: Vec<(Quantity, Documentation)>,
    base_units: &[rink_format::BaseUnit],
) -> Result<Vec<rink_format::Quantity>, Error> {
    enum TermType {
        BaseUnit(BaseUnitId),
        Quantity(Vec<(String, i64)>, Documentation),
    }

    impl DependencyNode for (String, TermType) {
        type Id = String;
        type Intermediate = Dimensionality;
        type Output = Option<rink_format::Quantity>;

        fn id(&self) -> Self::Id {
            self.0.clone()
        }

        fn dependencies(&self) -> Vec<Self::Id> {
            match self.1 {
                TermType::BaseUnit(_) => vec![],
                TermType::Quantity(ref deps, _) => {
                    deps.iter().map(|(name, _)| name.clone()).collect()
                }
            }
        }

        fn process(&self, values: Vec<Self::Intermediate>) -> (Self::Intermediate, Self::Output) {
            match self.1 {
                TermType::BaseUnit(id) => (Dimensionality::base_unit(id), None),
                TermType::Quantity(ref terms, ref documentation) => {
                    let dimensionality = terms
                        .iter()
                        .zip(values.into_iter())
                        .map(|((_, pow), value)| value.pow(*pow))
                        .reduce(|acc, value| acc * value)
                        .unwrap_or(Dimensionality::dimensionless());
                    (
                        dimensionality.clone(),
                        Some(rink_format::Quantity {
                            documentation: documentation.clone(),
                            name: self.0.to_owned(),
                            dimensionality,
                        }),
                    )
                }
            }
        }
    }

    let mut terms: Vec<(String, TermType)> = vec![];

    for (index, unit) in base_units.iter().enumerate() {
        terms.push((
            unit.name.clone(),
            TermType::BaseUnit(BaseUnitId(index as u16)),
        ));
        if let Some(ref short) = unit.short {
            terms.push((short.clone(), TermType::BaseUnit(BaseUnitId(index as u16))));
        }
    }

    for (
        Quantity {
            dimensionality,
            name,
        },
        documentation,
    ) in &quantities
    {
        let mut result = vec![];

        let mut parse =
            |input: &str, mul: i64| -> Result<(), Error> {
                let input = input.trim();
                for term in input.split(' ') {
                    let term = term.trim();
                    if term.is_empty() {
                        continue;
                    }
                    if let Some((base_unit, power)) = term.split_once('^') {
                        let power = power.parse::<i64>().map_err(|source| {
                            Error::InvalidDimensionality {
                                source,
                                string: dimensionality.clone(),
                            }
                        })? * mul;
                        result.push((base_unit.to_owned(), power));
                    } else if term != "1" {
                        result.push((term.to_owned(), 1));
                    }
                }
                Ok(())
            };

        if let Some((numerator, denominator)) = dimensionality.split_once('/') {
            parse(numerator, 1)?;
            parse(denominator, -1)?;
        } else {
            parse(&dimensionality, 1)?;
        };
        terms.push((
            name.to_owned(),
            TermType::Quantity(result, documentation.clone()),
        ));
    }

    Ok(resolve_dependencies(terms)?
        .into_iter()
        .filter_map(|x| x)
        .collect())
}

pub fn compile() -> Result<Vec<u8>, Error> {
    let paths = glob::glob("**/*.kdl")?;

    let mut ctx = Context {
        base_units: vec![],
        quantities: vec![],
        prefixes: vec![],
        categories: vec![],
        units: vec![],
        substances: vec![],
    };

    for path in paths {
        let path = path?;
        let content = fs::read_to_string(&path)?;
        let value = kdl::parse_document(content)?;

        let tree = value
            .iter()
            .map(Node::parse)
            .collect::<Result<Vec<_>, Error>>()?;

        visit_top(tree, &mut ctx)?
    }

    let base_units = ctx
        .base_units
        .into_iter()
        .map(
            |(BaseUnit { name, short }, documentation)| rink_format::BaseUnit {
                name,
                short,
                documentation,
            },
        )
        .collect::<Vec<_>>();

    let quantities = process_quantities(ctx.quantities, &base_units)?;

    let prefixes = ctx
        .prefixes
        .into_iter()
        .map(
            |(Prefix { value, short, long }, documentation)| rink_format::Prefix {
                long,
                short,
                value,
                documentation,
            },
        )
        .collect();

    let categories = ctx
        .categories
        .into_iter()
        .map(|(Category { name }, documentation)| rink_format::Category {
            name,
            documentation,
        })
        .collect();

    let units = ctx
        .units
        .into_iter()
        .map(
            |(Unit { name, definition }, documentation)| rink_format::Unit {
                name,
                definition,
                documentation,
            },
        )
        .collect();

    let substances = ctx
        .substances
        .into_iter()
        .map(
            |(Substance { name, symbol }, properties, documentation)| rink_format::Substance {
                name,
                symbol,
                documentation,
                properties: properties
                    .into_iter()
                    .map(
                        |Property {
                             name,
                             input_name,
                             input_value,
                             output_name,
                             output_value,
                         }| rink_format::Property {
                            name,
                            input_name,
                            input_value,
                            output_name,
                            output_value,
                        },
                    )
                    .collect(),
            },
        )
        .collect();

    let data = UnitsData {
        base_units,
        quantities,
        prefixes,
        categories,
        units,
        substances,
    };

    Ok(bincode::serialize(&data)?)
}
