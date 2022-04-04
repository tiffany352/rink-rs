use super::BaseUnit;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::{
        btree_map::{IntoIter, Iter},
        BTreeMap,
    },
    iter::FromIterator,
    ops,
};

type Map = BTreeMap<BaseUnit, i64>;
type Dimension = (BaseUnit, i64);

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[serde(transparent)]
pub struct Dimensionality {
    dims: Map,
}

impl Default for Dimensionality {
    fn default() -> Self {
        Dimensionality::new()
    }
}

impl Dimensionality {
    pub fn new() -> Dimensionality {
        Dimensionality {
            dims: BTreeMap::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, BaseUnit, i64> {
        self.dims.iter()
    }
}

/////////////////////////////////////////
// Compatiblity with BTreeMap interface

impl Dimensionality {
    pub(crate) fn insert(&mut self, unit: BaseUnit, power: i64) {
        self.dims.insert(unit, power);
    }

    pub(crate) fn len(&self) -> usize {
        self.dims.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.dims.is_empty()
    }
}

impl ops::Deref for Dimensionality {
    type Target = Map;

    fn deref(&self) -> &Self::Target {
        &self.dims
    }
}

impl FromIterator<Dimension> for Dimensionality {
    fn from_iter<T: IntoIterator<Item = Dimension>>(iter: T) -> Self {
        let dims = Map::from_iter(iter);
        Dimensionality { dims }
    }
}

impl IntoIterator for Dimensionality {
    type Item = Dimension;
    type IntoIter = IntoIter<BaseUnit, i64>;

    fn into_iter(self) -> Self::IntoIter {
        self.dims.into_iter()
    }
}

impl From<Map> for Dimensionality {
    fn from(dims: Map) -> Self {
        Dimensionality { dims }
    }
}
