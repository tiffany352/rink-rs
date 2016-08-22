// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod rational;
pub mod arith;
pub mod e;

pub use cf::rational::Rational;
pub use cf::arith::Arith;
pub use cf::e::E;

use gmp::mpz::Mpz;
use gmp::mpq::Mpq;

pub type BigInt = Mpz;

pub struct Approx {
    h_neg2: BigInt,
    h_neg1: BigInt,
    k_neg2: BigInt,
    k_neg1: BigInt,
}

pub trait Cf : Iterator<Item=BigInt>+Clone {
    fn approximate(self) -> ::std::iter::Scan<Self, Approx, fn(&mut Approx, BigInt) -> Option<Mpq>> {
        // http://www.math.jacobs-university.de/timorin/PM/continued_fractions.pdf
        // h[-2] = 0, h[-1] = 1, h[0] = cf[0], h[1] = cf[0] cf[1] + 1
        // k[-2] = 1, k[-1] = 0, k[0] = 1, k[1] = cf[1]
        let start = Approx {
            h_neg2: BigInt::zero(),
            h_neg1: BigInt::one(),
            k_neg2: BigInt::one(),
            k_neg1: BigInt::zero(),
        };
        fn func(st: &mut Approx, v: BigInt) -> Option<Mpq> {
            // h[n] = cf[n] h[n-1] + h[n-2]
            // k[n] = cf[n] k[n-1] + k[n-2]
            let h = &(&v * &st.h_neg1) + &st.h_neg2;
            let k = &(&v * &st.k_neg1) + &st.k_neg2;
            let res = Mpq::ratio(&h, &k);
            *st = Approx {
                h_neg2: st.h_neg1.clone(),
                h_neg1: h,
                k_neg2: st.k_neg1.clone(),
                k_neg1: k,
            };
            Some(res)
        }
        self.scan(start, func)
    }
}

#[test]
fn test_approx() {
    let rat = Rational::new(BigInt::from(355), BigInt::from(113));
    let mut res = rat.approximate();
    assert_eq!(res.nth(2), Some(Mpq::ratio(&Mpz::from(355), &Mpz::from(113))));
}
