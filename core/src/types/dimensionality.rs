use crate::algorithms::btree_merge;

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

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
#[serde(transparent)]
pub struct Dimensionality {
    dims: Map,
}

impl Dimensionality {
    pub fn new() -> Dimensionality {
        Dimensionality::default()
    }

    pub fn new_dim(unit: BaseUnit, power: i64) -> Dimensionality {
        let mut value = Dimensionality::new();
        value.dims.insert(unit, power);
        value
    }

    pub fn base_unit(unit: BaseUnit) -> Dimensionality {
        let mut value = Dimensionality::new();
        value.dims.insert(unit, 1);
        value
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, BaseUnit, i64> {
        self.dims.iter()
    }

    pub fn as_single(&self) -> Option<(&BaseUnit, i64)> {
        if self.dims.len() == 1 {
            let (unit, &power) = self.dims.iter().next().unwrap();
            Some((unit, power))
        } else {
            None
        }
    }

    pub fn is_dimensionless(&self) -> bool {
        self.dims.is_empty()
    }
}

impl<'a> ops::Mul for &'a Dimensionality {
    type Output = Dimensionality;

    fn mul(self, rhs: Self) -> Self::Output {
        let dims = btree_merge(&self.dims, &rhs.dims, |a, b| {
            if a + b != 0 {
                Some(a + b)
            } else {
                None
            }
        });
        Dimensionality { dims }
    }
}

/////////////////////////////////////////
// Compatiblity with BTreeMap interface

impl Dimensionality {
    pub(crate) fn insert(&mut self, unit: BaseUnit, power: i64) {
        self.dims.insert(unit, power);
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
