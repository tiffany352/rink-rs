mod ast;
mod error;
mod matcher;

use std::{collections::HashMap, fs};

use ast::*;
use matcher::*;
use rink_format::{CategoryId, Documentation, UnitsData};

pub use error::Error;

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

fn process_substance(
    substance: Substance,
    children: Vec<Node>,
    category: Option<CategoryId>,
) -> Result<rink_format::Substance, Error> {
    let mut docs_builder = DocsBuilder::new();
    let mut properties = vec![];

    for node in children {
        match node.data {
            NodeData::Title(node) => docs_builder.add_title(node)?,
            NodeData::Doc(node) => docs_builder.add_doc(node)?,
            NodeData::Alias(node) => docs_builder.add_alias(node)?,
            NodeData::Property(Property {
                name,
                input_name,
                input_value,
                output_name,
                output_value,
            }) => properties.push(rink_format::Property {
                name,
                input_name,
                input_value,
                output_name,
                output_value,
            }),
            node => {
                return Err(Error::UnexpectedNode {
                    parent: Substance::NAME,
                    child: node.name(),
                });
            }
        }
    }

    Ok(rink_format::Substance {
        name: substance.name,
        symbol: substance.symbol,
        documentation: docs_builder.build(category),
        properties,
    })
}

fn process_def(
    node: Node,
    data: &mut UnitsData,
    category: Option<CategoryId>,
    parent: &'static str,
) -> Result<(), Error> {
    match node.data {
        NodeData::Unit(Unit { name, definition }) => data.units.push(rink_format::Unit {
            name,
            definition,
            documentation: process_docs(node.children, category)?,
        }),
        NodeData::BaseUnit(BaseUnit { name, short }) => {
            data.base_units.push(rink_format::BaseUnit {
                name,
                short,
                documentation: process_docs(node.children, category)?,
            })
        }
        NodeData::Prefix(Prefix { value, short, long }) => {
            data.prefixes.push(rink_format::Prefix {
                long,
                short,
                value,
                documentation: process_docs(node.children, category)?,
            })
        }
        NodeData::Quantity(Quantity {
            name,
            dimensionality: dim_str,
        }) => {
            let dimensionality = vec![];

            let parse = |input: &str, mul: i64| -> Result<(), Error> {
                for term in input.split(' ') {
                    if let Some((name, power)) = term.split_once('^') {
                        let power = power.parse::<i64>().map_err(|source| {
                            Error::InvalidDimensionality {
                                source,
                                string: dim_str.clone(),
                            }
                        })? * mul;
                        let _ = (name, power);
                    }
                }
                Ok(())
            };

            if let Some((numerator, denominator)) = dim_str.split_once('/') {
                parse(numerator, 1)?;
                parse(denominator, -1)?;
            } else {
                parse(&dim_str, 1)?;
            };

            data.quantities.push(rink_format::Quantity {
                name,
                dimensionality,
                documentation: process_docs(node.children, category)?,
            })
        }
        NodeData::Substance(substance) => {
            data.substances
                .push(process_substance(substance, node.children, category)?);
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

fn process_category(
    category: Category,
    children: Vec<Node>,
    data: &mut UnitsData,
) -> Result<(), Error> {
    let id = CategoryId(data.categories.len() as u32);
    let mut docs_builder = DocsBuilder::new();

    for node in children {
        match node.data {
            NodeData::Title(node) => docs_builder.add_title(node)?,
            NodeData::Doc(node) => docs_builder.add_doc(node)?,
            NodeData::Alias(node) => docs_builder.add_alias(node)?,
            _ => process_def(node, data, Some(id), Category::NAME)?,
        }
    }

    data.categories.push(rink_format::Category {
        name: category.name,
        documentation: docs_builder.build(None),
    });
    Ok(())
}

fn process_top(nodes: Vec<Node>, data: &mut UnitsData) -> Result<(), Error> {
    for node in nodes {
        match node.data {
            NodeData::Category(category) => process_category(category, node.children, data)?,
            _ => process_def(node, data, None, "document")?,
        }
    }
    Ok(())
}

pub fn compile() -> Result<Vec<u8>, Error> {
    let paths = glob::glob("**/*.kdl")?;

    let mut data = UnitsData {
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

        process_top(tree, &mut data)?
    }

    Ok(bincode::serialize(&data)?)
}
