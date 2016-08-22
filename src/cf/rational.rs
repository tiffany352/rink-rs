// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use cf::BigInt;
use cf::Cf;

#[derive(Clone)]
pub struct Rational(BigInt, BigInt);

impl Rational {
    pub fn new(num: BigInt, den: BigInt) -> Rational {
        Rational(num, den)
    }
}

impl Iterator for Rational {
    type Item = BigInt;

    fn next(&mut self) -> Option<BigInt> {
        if self.1.is_zero() {
            None
        } else {
            let den = self.1.clone();
            let p = &self.0 / &den;
            self.1 = &self.0 - &(&p * &den);
            self.0 = den;
            Some(p)
        }
    }
}

impl Cf for Rational {}

#[test]
fn test_rational() {
    let rat = Rational::new(BigInt::from(12), BigInt::from(3));
    let res = rat.collect::<Vec<_>>();
    assert_eq!(&res[..], &[BigInt::from(4)]);
    let rat = Rational::new(BigInt::from(1), BigInt::from(7));
    let res = rat.collect::<Vec<_>>();
    assert_eq!(&res[..], &[BigInt::from(0), BigInt::from(7)]);
}
