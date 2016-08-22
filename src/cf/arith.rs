// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use cf::BigInt;
use cf::Cf;
use gmp::mpq::Mpq;

/// Represents the equation
///
///       axy + bx + cy + d
/// z  =  ----------------- ,
///       cxy + fx + gy + h
///
/// and the 2x2x2 matrix
///
/// b   d
///  f   h
///
/// a   c
///  e   g .
#[derive(Clone)]
pub struct Arith<X, Y> {
    a: BigInt,
    b: BigInt,
    c: BigInt,
    d: BigInt,
    e: BigInt,
    f: BigInt,
    g: BigInt,
    h: BigInt,
    x: X,
    y: Y,
}

impl<X, Y> Arith<X, Y>
    where X: Cf, Y: Cf {

    pub fn new(
        a: BigInt, b: BigInt, c: BigInt, d: BigInt,
        e: BigInt, f: BigInt, g: BigInt, h: BigInt,
        x: X, y: Y
    ) -> Arith<X, Y> {
        Arith {
            a: a, b: b, c: c, d: d,
            e: e, f: f, g: g, h: h,
            x: x, y: y
        }
    }

    pub fn new_ints(
        a: i64, b: i64, c: i64, d: i64,
        e: i64, f: i64, g: i64, h: i64,
        x: X, y: Y
    ) -> Arith<X, Y> {
        Arith {
            a: BigInt::from(a), b: BigInt::from(b), c: BigInt::from(c), d: BigInt::from(d),
            e: BigInt::from(e), f: BigInt::from(f), g: BigInt::from(g), h: BigInt::from(h),
            x: x, y: y
        }
    }

    pub fn adder(x: X, y: Y) -> Arith<X, Y> {
        Arith::new_ints(
            0, 1, 1, 0,
            0, 0, 0, 1,
            x, y)
    }

    pub fn subtractor(x: X, y: Y) -> Arith<X, Y> {
        Arith::new_ints(
            0, 1, -1, 0,
            0, 0, 0, 1,
            x, y)
    }

    pub fn multiplier(x: X, y: Y) -> Arith<X, Y> {
        Arith::new_ints(
            1, 0, 0, 0,
            0, 0, 0, 1,
            x, y)
    }

    pub fn divider(x: X, y: Y) -> Arith<X, Y> {
        Arith::new_ints(
            0, 1, 0, 0,
            0, 0, 1, 0,
            x, y)
    }

    /// perform a step of euclid and output a number
    fn egest(&mut self, q: &BigInt) {
        // b   d
        //  f   h
        //
        // a   c
        //  e   g

        // to

        // f      h
        //  b-fq   d-hq
        //
        // e      g
        //  a-eq   c-gq

        let a = self.a.clone();
        let b = self.b.clone();
        let c = self.c.clone();
        let d = self.d.clone();
        let e = self.e.clone();
        let f = self.f.clone();
        let g = self.g.clone();
        let h = self.h.clone();

        self.e = &a - &(&e * q);
        self.f = &b - &(&f * q);
        self.g = &c - &(&g * q);
        self.h = &d - &(&h * q);

        self.a = e;
        self.b = f;
        self.c = g;
        self.d = h;
        println!("egested {}", q);
    }

    /// input a term p from x
    fn ingest_x(&mut self) {
        let p = self.x.next();
        if let Some(p) = p {
            let a = self.a.clone();
            let b = self.b.clone();
            let c = self.c.clone();
            let d = self.d.clone();
            let e = self.e.clone();
            let f = self.f.clone();
            let g = self.g.clone();
            let h = self.h.clone();

            // b   d
            //  f   h
            //
            // a   c
            //  e   g

            // to

            // d+bp  b
            //  h+fp  f
            //
            // c+ap  a
            //  g+ep  e

            self.a = &c + &(&a * &p);
            self.b = &d + &(&b * &p);
            self.e = &g + &(&e * &p);
            self.f = &h + &(&f * &p);

            self.d = b;
            self.h = f;
            self.c = a;
            self.g = e;

            println!("ingested {} from x", p);
        } else {
            self.c = self.a.clone();
            self.d = self.b.clone();
            self.g = self.e.clone();
            self.h = self.f.clone();
            println!("ingested ∞ from x");
        }
    }

    /// input a term q from y
    fn ingest_y(&mut self) {
        let p = self.x.next();
        if let Some(p) = p {
            let a = self.a.clone();
            let b = self.b.clone();
            let c = self.c.clone();
            let d = self.d.clone();
            let e = self.e.clone();
            let f = self.f.clone();
            let g = self.g.clone();
            let h = self.h.clone();

            // b   d
            //  f   h
            //
            // a   c
            //  e   g

            // to

            // a     c
            //  e     g
            //
            // b+ap  d+cp
            //  f+ep  h+gp

            self.a = &b + &(&a * &p);
            self.c = &d + &(&c * &p);
            self.e = &f + &(&e * &p);
            self.g = &h + &(&g * &p);

            self.b = a;
            self.d = c;
            self.f = e;
            self.h = g;

            println!("ingested {} from y", p);
        } else {
            self.b = self.a.clone();
            self.d = self.c.clone();
            self.f = self.e.clone();
            self.h = self.g.clone();
            println!("ingested ∞ from y");
        }
    }
}

fn bound(num: &BigInt, den: &BigInt) -> Option<Mpq> {
    if den.is_zero() {
        None
    } else {
        Some(Mpq::ratio(num, den))
    }
}

fn ibound(num: &BigInt, den: &BigInt) -> Option<BigInt> {
    if den.is_zero() {
        None
    } else {
        Some(num / den)
    }
}

fn diff(p: Option<&Mpq>, q: Option<&Mpq>) -> Option<Mpq> {
    if let (Some(p), Some(q)) = (p, q) {
        Some((p - q).abs())
    } else {
        None
    }
}

fn bgt(a: Option<&Mpq>, b: Option<&Mpq>) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => a > b,
        (None, Some(_)) => true,
        _ => false
    }
}

impl<X, Y> Iterator for Arith<X, Y>
    where X: Cf, Y: Cf {
    type Item = BigInt;

    fn next(&mut self) -> Option<BigInt> {
        for _ in 0..30 {
            println!("{} {} {} {} / {} {} {} {}",
                     self.a, self.b, self.c, self.d,
                     self.e, self.f, self.g, self.h);
            if self.e.is_zero() && self.f.is_zero() && self.g.is_zero() && self.h.is_zero() {
                return None
            }
            let ae = bound(&self.a, &self.e);
            let cg = bound(&self.c, &self.g);
            let bf = bound(&self.b, &self.f);
            //let dh = bound(&self.d, &self.h);
            let i11 = ibound(&self.a, &self.e);
            let i01 = ibound(&self.c, &self.g);
            let i10 = ibound(&self.b, &self.f);
            let i00 = ibound(&self.d, &self.h);

            println!("egest? {:?} == {:?} == {:?} == {:?}", i11, i10, i01, i00);
            if i11 == i10 && i10 == i01 && i01 == i00 {
                self.egest(i11.as_ref().unwrap());
                return Some(i11.unwrap())
            } else {
                // if one of these ratios is zero, then we must ingest from either x or y:

                //  y   xy
                //
                // xy    x

                if bgt(diff(bf.as_ref(), ae.as_ref()).as_ref(), diff(cg.as_ref(), ae.as_ref()).as_ref()) {
                    self.ingest_x();
                } else {
                    self.ingest_y();
                }
            }
        }
        panic!("Arith::next() ran too many times")
    }
}

impl<X, Y> Cf for Arith<X, Y> where X: Cf, Y: Cf {}

#[test]
fn test_add_int() {
    use cf::Rational;

    let a = Rational::new(BigInt::from(3), BigInt::from(1));
    let b = Rational::new(BigInt::from(2), BigInt::from(1));
    let r = Arith::adder(a, b);
    let approx = r.approximate().collect::<Vec<_>>();
    println!("{:?}", approx);
    panic!();
}

#[test]
fn test_add_rat() {
    use cf::Rational;

    let a = Rational::new(BigInt::from(3), BigInt::from(7));
    let b = Rational::new(BigInt::from(2), BigInt::from(7));
    let r = Arith::adder(a, b);
    let approx = r.approximate().collect::<Vec<_>>();
    println!("{:?}", approx);
    panic!();
}
