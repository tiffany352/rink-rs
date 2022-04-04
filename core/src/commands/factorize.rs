// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::types::{Dimensionality, Number, Numeric};
use std::cmp;
use std::collections::{BTreeMap, BinaryHeap};
use std::rc::Rc;

#[derive(PartialEq, Eq, Debug)]
pub struct Factors(pub usize, pub Vec<Rc<String>>);

impl cmp::PartialOrd for Factors {
    fn partial_cmp(&self, other: &Factors) -> Option<cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl cmp::Ord for Factors {
    fn cmp(&self, other: &Factors) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub fn factorize(
    value: &Number,
    quantities: &BTreeMap<Dimensionality, Rc<String>>,
) -> BinaryHeap<Factors> {
    if value.dimless() {
        let mut map = BinaryHeap::new();
        map.push(Factors(0, vec![]));
        return map;
    }
    let mut candidates: BinaryHeap<Factors> = BinaryHeap::new();
    let value_score = value.complexity_score();
    for (unit, name) in quantities.iter().rev() {
        let num = Number {
            value: Numeric::one(),
            unit: unit.clone(),
        };
        let res = (value / &num).unwrap();
        //if res.unit.len() >= value.unit.len() {
        let score = res.complexity_score();
        // we are not making the unit any simpler
        if score >= value_score {
            continue;
        }
        let res = factorize(&res, quantities);
        for Factors(score, mut vec) in res {
            vec.push(name.clone());
            vec.sort();
            candidates.push(Factors(score + 1, vec));
        }
        let mut next = candidates.into_sorted_vec();
        next.dedup();
        candidates = next.into_iter().take(10).collect();
    }
    assert!(candidates.len() <= 10);
    candidates
}
