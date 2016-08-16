use std::collections::{BTreeMap, BinaryHeap};
use eval::Context;
use std::rc::Rc;
use number::{Number, Unit};
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

pub fn factorize(ctx: &Context, value: &Number, aliases: &BTreeMap<Unit, Rc<String>>)
                 -> BinaryHeap<Factors> {
    if value.1.len() == 0 {
        let mut map = BinaryHeap::new();
        map.push(Factors(0, vec![]));
        return map;
    }
    let mut candidates: BinaryHeap<Factors> = BinaryHeap::new();
    let value_score = value.complexity_score();
    for (unit, name) in aliases.iter().rev() {
        use gmp::mpq::Mpq;

        let res = (value / &Number(Mpq::one(), unit.clone())).unwrap();
        //if res.1.len() >= value.1.len() {
        let score = res.complexity_score();
        // we are not making the unit any simpler
        if score >= value_score {
            continue
        }
        let res = factorize(ctx, &res, aliases);
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
