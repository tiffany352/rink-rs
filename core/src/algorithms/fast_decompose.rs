// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;

use crate::types::{Dimension, Number, Numeric, Quantity};

/// Uses a single-pass algorithm for finding a suitable decomposition. A
/// more robust one would require factorization, but that's expensive.
///
/// There are several heuristics that this function uses to produce
/// "human-friendly" outputs. This means never introducing base units
/// that only exist to be cancelled out, and avoiding very long or
/// complex decompositions.
pub(crate) fn fast_decompose(value: &Number, quantities: &BTreeMap<Quantity, String>) -> Quantity {
    let mut best = None;
    'outer: for (unit, name) in quantities.iter() {
        // make sure we aren't doing something weird like introducing new base units
        for (dim, pow) in unit {
            let vpow = value.unit.get(dim).cloned().unwrap_or(0);
            let snum = (vpow - pow).signum();
            if snum != 0 && snum != vpow.signum() {
                continue 'outer;
            }
        }
        let num = Number {
            value: Numeric::one(),
            unit: unit.clone(),
        };
        for &i in [-1, 1, 2].iter() {
            let res = (value / &num.powi(i)).unwrap();
            let score = res.complexity_score();
            let better = best
                .as_ref()
                .map(|&(_, _, _, current)| score < current)
                .unwrap_or(true);
            if better {
                best = Some((name, unit, i, score));
            }
        }
    }
    if let Some((name, unit, pow, score)) = best {
        if score < value.complexity_score() {
            let num = Number {
                value: Numeric::one(),
                unit: unit.clone(),
            };
            let mut res = (value / &num.powi(pow)).unwrap().unit;
            res.insert(Dimension::new(&**name), pow as i64);
            return res;
        }
    }
    value.unit.clone()
}
