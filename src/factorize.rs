// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::{BTreeMap, BinaryHeap};
use std::rc::Rc;
use number::{Number, Unit, Dim};
use num::Num;
use std::cmp;

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

pub fn fast_decompose(value: &Number, quantities: &BTreeMap<Unit, String>) -> Unit {
    let mut best = None;
    'outer: for (unit, name) in quantities.iter() {
        // make sure we aren't doing something weird like introducing new base units
        for (dim, pow) in unit {
            let vpow = value.unit.get(dim).cloned().unwrap_or(0);
            let snum = (vpow - pow).signum();
            if snum != 0 && snum != vpow.signum() {
                continue 'outer
            }
        }
        let num = Number {
            value: Num::one(),
            unit: unit.clone(),
        };
        for &i in [-1, 1, 2].iter() {
            let res = (value / &num.powi(i)).unwrap();
            let score = res.complexity_score();
            let better = best.as_ref().map(|&(_, _, _, current)| score < current).unwrap_or(true);
            if better {
                best = Some((name, unit, i, score));
            }
        }
    }
    if let Some((name, unit, pow, score)) = best {
        if score < value.complexity_score() {
            let num = Number {
                value: Num::one(),
                unit: unit.clone(),
            };
            let mut res = (value / &num.powi(pow)).unwrap().unit;
            res.insert(Dim::new(&**name), pow as i64);
            return res
        }
    }
    value.unit.clone()
}

pub fn factorize(value: &Number, quantities: &BTreeMap<Unit, Rc<String>>)
                 -> BinaryHeap<Factors> {
    if value.dimless() {
        let mut map = BinaryHeap::new();
        map.push(Factors(0, vec![]));
        return map;
    }
    let mut candidates: BinaryHeap<Factors> = BinaryHeap::new();
    let value_score = value.complexity_score();
    for (unit, name) in quantities.iter().rev() {
        let num = Number {
            value: Num::one(),
            unit: unit.clone(),
        };
        let res = (value / &num).unwrap();
        //if res.unit.len() >= value.unit.len() {
        let score = res.complexity_score();
        // we are not making the unit any simpler
        if score >= value_score {
            continue
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
