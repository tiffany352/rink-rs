use std::sync::{Arc, Mutex};

use super::{term::Precedence, Term};
use crate::BigFloat;

/// An implementation of pi as a recursive real, using the
/// Gauss–Legendre algorithm.
#[derive(Debug)]
pub struct Pi {
    terms: Arc<Mutex<Vec<BigFloat>>>,
}

impl Pi {
    pub(crate) fn new() -> Pi {
        Pi {
            terms: Arc::new(Mutex::new(vec![])),
        }
    }
}

#[derive(Clone)]
struct Vars {
    i: i64,
    a: BigFloat,
    b: BigFloat,
    t: BigFloat,
}

fn initial(precision: i64) -> Vars {
    Vars {
        i: 0,
        a: BigFloat::one(),
        b: BigFloat::two_pow(1).sqrt(precision),
        t: BigFloat::two_pow(2),
    }
}

fn iterate(Vars { i, a, b, t }: Vars, precision: i64, b_estimate: BigFloat) -> Vars {
    let a1 = (&a + &b) >> 1;
    let b1 = (&a * &b).sqrt_with_estimate(precision, b_estimate);
    let a_delta = &a - &a1;
    let t1 = t - BigFloat::two_pow(-i) * (&a_delta * &a_delta);
    Vars {
        i: i + 1,
        a: a1,
        b: b1,
        t: t1,
    }
}

fn approximate(Vars { a, b, t, .. }: &Vars) -> BigFloat {
    let sum = a + b;
    let num = &sum * &sum;
    let div = t << 2;
    num / div
}

impl Term for Pi {
    fn eval(&self, precision: i64) -> BigFloat {
        let extra_prec = precision * precision + 100;
        let mut vars = initial(extra_prec);
        let approx = approximate(&vars);
        println!(
            "a = {}, b = {}, t = {}, approx = {}",
            vars.a, vars.b, vars.t, approx
        );
        let extra_eval_prec = (precision as f64).log2().ceil() as u64 + 10;
        println!(
            "precision: {}, extra_prec: {}, iterations: {}",
            precision, extra_prec, extra_eval_prec
        );
        let one = BigFloat::one();
        for i in 0..extra_eval_prec {
            let b_estimate = {
                let terms = self.terms.lock().unwrap();
                terms.get(i as usize).unwrap_or(&one).clone()
            };
            vars = iterate(vars, extra_prec, b_estimate);
            {
                let mut terms = self.terms.lock().unwrap();
                if let Some(existing) = terms.get_mut(i as usize) {
                    if vars.b.precision() > existing.precision() {
                        *existing = vars.b.clone();
                    }
                } else {
                    terms.push(vars.b.clone());
                }
            }
            let approx = approximate(&vars);
            println!(
                "a = {}, b = {}, t = {}, approx = {}",
                vars.a, vars.b, vars.t, approx
            );
        }
        let value = approximate(&vars);
        println!("final = {}", value);

        value
    }

    fn describe(&self, writer: &mut String, _prec: Precedence) {
        writer.push_str("pi");
    }

    fn precedence(&self) -> Precedence {
        Precedence::Term
    }
}

#[cfg(test)]
mod tests;
