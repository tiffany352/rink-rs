// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use cf::BigInt;
use cf::Cf;

#[derive(Clone)]
pub struct E(u64);

impl E {
    pub fn new() -> E {
        E(0)
    }
}

impl Iterator for E {
    type Item = BigInt;

    fn next(&mut self) -> Option<BigInt> {
        let res = match self.0%3 {
            0 | 2 => Some(BigInt::one()),
            1 => Some(BigInt::from(2*(self.0/3))),
            _ => unreachable!()
        };
        self.0 += 1;
        res
    }
}

impl Cf for E {}

#[test]
fn test_e() {
    use gmp::mpq::Mpq;

    let e = E::new();
    let res = e.approximate().nth(9);
    assert_eq!(res, Some(Mpq::ratio(&BigInt::from(193), &BigInt::from(71))));
}
