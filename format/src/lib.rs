// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Definitions for the data format used to store units data.

mod dims;

use serde_derive::{Deserialize, Serialize};
use std::collections::HashMap;

pub use dims::Dimensionality;

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug, Clone, Copy, PartialOrd, Ord)]
pub struct BaseUnitId(pub u16);

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct CategoryId(pub u32);

impl CategoryId {
    pub fn null() -> CategoryId {
        CategoryId(u32::MAX)
    }

    pub fn is_null(self) -> bool {
        self == CategoryId::null()
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Documentation {
    pub title_by_lang: HashMap<String, String>,
    pub doc_by_lang: HashMap<String, String>,
    pub category: Option<CategoryId>,
    pub aliases: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct BaseUnit {
    pub name: String,
    pub short: Option<String>,
    pub documentation: Documentation,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Quantity {
    pub name: String,
    pub dimensionality: Dimensionality,
    pub documentation: Documentation,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Prefix {
    pub long: Option<String>,
    pub short: Option<String>,
    pub value: String,
    pub documentation: Documentation,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Unit {
    pub name: String,
    pub definition: String,
    pub documentation: Documentation,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Category {
    pub name: String,
    pub documentation: Documentation,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Substance {
    pub name: String,
    pub symbol: Option<String>,
    pub documentation: Documentation,
    pub properties: Vec<Property>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Property {
    pub name: String,
    pub input_name: String,
    pub input_value: String,
    pub output_name: String,
    pub output_value: String,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct UnitsData {
    pub base_units: Vec<BaseUnit>,
    pub quantities: Vec<Quantity>,
    pub prefixes: Vec<Prefix>,
    pub categories: Vec<Category>,
    pub units: Vec<Unit>,
    pub substances: Vec<Substance>,
}
