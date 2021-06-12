use super::{term::Precedence, RReal, Term};
use crate::BigFloat;

#[derive(Debug)]
pub struct Sqrt(pub RReal);

fn search(
    value: &BigFloat,
    eps: &BigFloat,
    min: &BigFloat,
    max: &BigFloat,
    pivot: BigFloat,
) -> BigFloat {
    let square = &pivot * &pivot;
    let delta = square - value.clone();
    let is_negative = delta.is_negative();
    if &delta.abs() < eps {
        pivot
    } else if is_negative {
        // Estimate is too low, need to check higher.
        let new_pivot = (max + &pivot) >> 1;
        search(value, eps, &pivot, max, new_pivot)
    } else {
        // Estimate is too high, need to check lower.
        let new_pivot = (min + &pivot) >> 1;
        search(value, eps, min, &pivot, new_pivot)
    }
}

impl Term for Sqrt {
    fn eval(&self, precision: i64) -> BigFloat {
        let arg_precision = precision * 2 - 1;
        let sample = self.0.eval(arg_precision);

        let one = BigFloat::one();
        let zero = BigFloat::zero();
        let min = if &sample > &one { &one } else { &zero };
        let max = if &sample > &one { &sample } else { &one };

        let value = search(
            &sample,
            &BigFloat::two_pow(precision),
            min,
            max,
            one.clone(),
        );

        value.into_inexact()
    }

    fn describe(&self, writer: &mut String, _prec: Precedence) {
        writer.push_str("sqrt(");
        self.0.describe(writer, Precedence::Add);
        writer.push_str(")");
    }

    fn precedence(&self) -> Precedence {
        Precedence::Term
    }
}
