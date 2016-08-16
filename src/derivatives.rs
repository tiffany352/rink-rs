use std::collections::{BTreeMap, BinaryHeap};
use eval::Context;
use std::rc::Rc;
use number::{Number, Unit};
use std::cmp;

#[derive(PartialEq, Eq, Debug)]
pub struct Deriv(pub usize, pub Vec<Rc<String>>);

impl cmp::PartialOrd for Deriv {
    fn partial_cmp(&self, other: &Deriv) -> Option<cmp::Ordering> {
        Some(self.0.cmp(&other.0).reverse())
    }
}

impl cmp::Ord for Deriv {
    fn cmp(&self, other: &Deriv) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub fn derivatives(ctx: &Context, value: &Number, aliases: &BTreeMap<Unit, Rc<String>>)
                   -> BinaryHeap<Deriv> {
    if value.1.len() == 0 {
        let mut map = BinaryHeap::new();
        map.push(Deriv(0, vec![]));
        return map;
    }
    let mut candidates: BinaryHeap<Deriv> = BinaryHeap::new();
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
        let res = derivatives(ctx, &res, aliases);
        for Deriv(score, mut vec) in res {
            vec.push(name.clone());
            candidates.push(Deriv(score + 1, vec));
        }
        let mut next = BinaryHeap::new();
        for _ in 0..10 {
            if let Some(v) = candidates.pop() {
                next.push(v);
            } else {
                break
            }
        }
        candidates = next;
    }
    assert!(candidates.len() <= 10);
    candidates
}
