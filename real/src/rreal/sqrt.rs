use num::{BigInt, BigRational, One, Signed};

use super::{Approx, RReal, Term};

#[derive(Debug)]
pub struct Sqrt(pub RReal);

// x = sqrt(y / 2^p) * 2^p
// x / 2^p = sqrt(y / 2^p)
// (x / 2^p)^2 = y / 2^p
// (x / 2^p)^2 * 2^p = y
// (x / 2^p) * (x / 2^p) * 2^p = y
// x^2 / 2^p / 2^p = y
// x^2 / 2^p = y * 2^p

// value = y * 2^p
fn search(
    value: &BigInt,
    precision: u64,
    eps: &BigInt,
    min: &BigInt,
    max: &BigInt,
    pivot: BigInt,
) -> BigInt {
    let square = &pivot * &pivot >> precision;
    let delta = &square - value;
    if &delta.abs() < eps {
        pivot
    } else if delta.is_negative() {
        // Estimate is too low, need to check higher.
        let new_pivot = (max + &pivot + 1) >> 1;
        search(value, precision, eps, &pivot, max, new_pivot)
    } else {
        // Estimate is too high, need to check lower.
        let new_pivot = (min + &pivot - 1) >> 1;
        search(value, precision, eps, min, &pivot, new_pivot)
    }
}

impl Term for Sqrt {
    fn eval(&self, precision: u64) -> Approx {
        let arg_precision = precision * 2 - 1;
        let sample = self.0.eval(arg_precision);

        let one = BigInt::one() << arg_precision;
        let max = if sample.value > one {
            &sample.value
        } else {
            &one
        };

        let value = search(
            &sample.value,
            arg_precision,
            &(BigInt::one() << (precision - 2)),
            &BigInt::one(),
            max,
            one.clone(),
        ) >> (precision - 1);

        Approx {
            value,
            precision,
            exact: false,
        }
    }
}
