use num::{BigInt, BigRational, One};

use super::{Approx, RReal, Term};

#[derive(Debug)]
pub struct Pi;

#[derive(Clone)]
struct Vars {
    i: usize,
    a: RReal,
    b: RReal,
    t: RReal,
}

fn initial() -> Vars {
    Vars {
        i: 1,
        a: RReal::one(),
        b: RReal::one() / RReal::from(2).sqrt(),
        t: RReal::rational(BigRational::new(BigInt::from(1), BigInt::from(4))),
    }
}

fn iterate(Vars { i, a, b, t }: Vars) -> Vars {
    let p = BigInt::one() << i;
    let a1 = (a.clone() + b.clone()) / RReal::from(2);
    let b1 = (a.clone() * b).sqrt();
    let a_delta = a - a1.clone();
    let t1 = t - RReal::from(p) * a_delta.clone() * a_delta;
    Vars {
        i: i + 1,
        a: a1,
        b: b1,
        t: t1,
    }
}

fn approximate(Vars { a, b, t, .. }: Vars) -> RReal {
    let sum = a + b;
    let div = t * RReal::from(4);
    sum.clone() * sum / div
}

impl Term for Pi {
    fn eval(&self, precision: u64) -> Approx {
        let mut vars = initial();
        let extra_eval_prec = (precision as f64).log2().ceil() as u64 + 10;
        for _ in 0..extra_eval_prec {
            vars = iterate(vars);
        }
        let value = approximate(vars);
        value.eval(precision)
    }
}

#[cfg(test)]
mod tests;
