use cf::BigInt;
use cf::Cf;
use gmp::mpq::Mpq;

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

    fn egest(&mut self, q: &BigInt) {
        use std::mem::swap;

        swap(&mut self.a, &mut self.e);
        swap(&mut self.b, &mut self.f);
        swap(&mut self.c, &mut self.g);
        swap(&mut self.d, &mut self.h);
        self.e = &self.e - &(q * &self.a);
        self.f = &self.f - &(q * &self.b);
        self.g = &self.g - &(q * &self.c);
        self.h = &self.h - &(q * &self.d);
        println!("egested {}", q);
    }

    fn ingest_x(&mut self) {
        let p = self.x.next();
        if let Some(p) = p {
            let a = self.a.clone();
            let b = self.b.clone();
            let e = self.e.clone();
            let f = self.f.clone();

            self.a = &(&a * &p) + &self.c;
            self.b = &(&b * &p) + &self.d;
            self.c = a;
            self.d = b;

            self.e = &(&e * &p) + &self.g;
            self.f = &(&f * &p) + &self.h;
            self.g = e;
            self.h = f;
            //println!("ingested {} from x", p);
        } else {
            self.c = self.a.clone();
            self.d = self.b.clone();
            self.g = self.e.clone();
            self.h = self.f.clone();
            //println!("ingested ∞ from x");
        }
    }

    fn ingest_y(&mut self) {
        let p = self.x.next();
        if let Some(p) = p {
            let a = self.a.clone();
            let c = self.c.clone();
            let e = self.e.clone();
            let g = self.g.clone();

            self.a = &(&a * &p) + &self.b;
            self.b = a;
            self.c = &(&c * &p) + &self.d;
            self.d = c;

            self.e = &(&e * &p) + &self.f;
            self.f = e;
            self.g = &(&g * &p) + &self.h;
            self.h = g;
            //println!("ingested {} from y", p);
        } else {
            self.b = self.a.clone();
            self.d = self.c.clone();
            self.f = self.e.clone();
            self.h = self.g.clone();
            //println!("ingested ∞ from y");
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
            //println!("{} {} {} {} / {} {} {} {}",
            //         self.a, self.b, self.c, self.d,
            //         self.e, self.f, self.g, self.h);
            if self.e.is_zero() && self.f.is_zero() && self.g.is_zero() && self.h.is_zero() {
                return None
            }
            let b11 = bound(&self.a, &self.e);
            let b01 = bound(&self.c, &self.g);
            let b10 = bound(&self.b, &self.f);
            let b00 = bound(&self.d, &self.h);
            let i11 = ibound(&self.a, &self.e);
            let i01 = ibound(&self.c, &self.g);
            let i10 = ibound(&self.b, &self.f);
            let i00 = ibound(&self.d, &self.h);

            //println!("egest? {:?} == {:?} == {:?} == {:?}", i11, i10, i01, i00);
            if i11 == i10 && i10 == i01 && i01 == i00 {
                self.egest(i11.as_ref().unwrap());
                return Some(i11.unwrap())
            } else {
                use std::cmp::max;

                let xd1 = diff(b11.as_ref(), b01.as_ref());
                let xd2 = diff(b10.as_ref(), b00.as_ref());
                let xw = max(&xd1, &xd2);
                let yd1 = diff(b11.as_ref(), b10.as_ref());
                let yd2 = diff(b01.as_ref(), b00.as_ref());
                let yw = max(&yd1, &yd2);

                //println!("bounds: {:?} {:?} {:?} {:?}", b00, b01, b10, b11);
                //println!("x diffs: {:?} {:?}", xd1, xd2);
                //println!("y diffs: {:?} {:?}", yd1, yd2);
                //println!("xw = {:?} yw = {:?}", xw, yw);

                if bgt(xw.as_ref(), yw.as_ref()) {
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
fn test_arith() {
    use cf::Rational;

    let a = Rational::new(BigInt::from(1), BigInt::from(1));
    let b = Rational::new(BigInt::from(1), BigInt::from(1));
    let r = Arith::adder(a, b);
    let approx = r.approximate().collect::<Vec<_>>();
    println!("{:?}", approx);
    panic!();
}
