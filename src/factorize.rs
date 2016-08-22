use std::collections::{BTreeMap, BinaryHeap, HashMap};
use std::rc::Rc;
use number::{Number, Unit};
use std::cmp;
use gmp::mpq::Mpq;

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

pub fn fast_decompose(value: &Number, aliases: &HashMap<Unit, String>) -> Unit {
    let mut best = None;
    for (unit, name) in aliases.iter() {
        let num = Number(Mpq::one(), unit.clone());
        for &i in [-1, 1, 2].into_iter() {
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
            let mut res = (value / &Number(Mpq::one(), unit.clone()).powi(pow)).unwrap().1;
            res.insert(Rc::new(name.clone()), pow as i64);
            return res
        }
    }
    value.1.clone()
}

pub fn factorize(value: &Number, aliases: &BTreeMap<Unit, Rc<String>>)
                 -> BinaryHeap<Factors> {
    if value.1.len() == 0 {
        let mut map = BinaryHeap::new();
        map.push(Factors(0, vec![]));
        return map;
    }
    let mut candidates: BinaryHeap<Factors> = BinaryHeap::new();
    let value_score = value.complexity_score();
    for (unit, name) in aliases.iter().rev() {
        let res = (value / &Number(Mpq::one(), unit.clone())).unwrap();
        //if res.1.len() >= value.1.len() {
        let score = res.complexity_score();
        // we are not making the unit any simpler
        if score >= value_score {
            continue
        }
        let res = factorize(&res, aliases);
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