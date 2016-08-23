use gmp::mpz::Mpz;
use gmp::mpq::Mpq;
use std::iter::{once, Once};

pub type Int = Mpz;
pub type Rational = Mpq;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interval(Rational, Rational);

impl Interval {
    pub fn delta(&self) -> Rational {
        &self.1 - &self.0
    }

    pub fn cut(self) -> Once<Interval> {
        once(self)
    }

    pub fn midpoint(&self) -> Rational {
        &(&self.0 + &self.1) / &Rational::ratio(&Int::from(2), &Int::one())
    }

    pub fn ints(a: i64, b: i64, c: i64, d: i64) -> Interval {
        Interval(Rational::ratio(&Int::from(a), &Int::from(b)),
                 Rational::ratio(&Int::from(c), &Int::from(d)))
    }
}

impl From<Rational> for Interval {
    fn from(r: Rational) -> Interval {
        Interval(r.clone(), r)
    }
}

pub trait Cut : Iterator<Item = Interval> {}

impl Cut for Once<Interval> {}

pub struct E {
    i: u64,
    fact: Rational,
    acc: Rational,
}

impl E {
    pub fn new() -> E {
        E {
            i: 0,
            fact: Rational::one(),
            acc: Rational::one(),
        }
    }
}

impl Iterator for E {
    type Item = Interval;

    fn next(&mut self) -> Option<Interval> {
        let lower = self.acc.clone();
        let upper = &self.acc + &(&Rational::one() / &self.fact);
        self.i += 1;
        self.fact = &self.fact * Rational::ratio(&Int::from(self.i), &Int::one());
        self.acc = &self.acc + &(&Rational::one() / &self.fact);
        if self.i == 1 {
            self.next()
        } else {
            Some(Interval(lower, upper))
        }
    }
}

impl Cut for E {}

#[cfg(test)]
fn assert_bounds<I>(iterations: usize, mut iter: I) -> Interval where I: Iterator<Item=Interval> {
    let first = iter.next().unwrap();
    iter.take(iterations).enumerate().fold(first, |a, (i, n)| {
        assert!(n.0 >= a.0, "lower bound loosened at iteration {}: new = {:?}, old = {:?}",
                i, n.0, a.0);
        assert!(n.1 <= a.1, "upper bound loosened at iteration {}: new = {:?}, old = {:?}",
                i, n.1, a.1);
        assert!(n.0 > a.0 || n.1 < a.1, "failed to make progress in iteration {}: new = old = {:?}",
                i, a);
        n
    })
}

#[test]
fn test_e() {
    let e = E::new();
    assert_eq!(assert_bounds(10, e), Interval::ints(13563139,4989600, 36168371,13305600));
}

pub struct Add<A,B> {
    a: A,
    b: B,
    last_a: Interval,
    last_b: Interval,
    done_a: bool,
    done_b: bool,
}

impl<A,B> Add<A,B> where A:Cut, B:Cut {
    pub fn new(mut a: A, mut b: B) -> Add<A,B> {
        Add {
            last_a: a.next().unwrap(),
            last_b: b.next().unwrap(),
            a: a, b: b,
            done_a: false, done_b: false,
        }
    }
}

impl<A,B> Iterator for Add<A,B> where A:Cut, B:Cut {
    type Item = Interval;

    fn next(&mut self) -> Option<Interval> {
        if (self.last_b.delta() > self.last_a.delta() || self.done_a) && !self.done_b {
            if let Some(b) = self.b.next() {
                self.last_b = b;
            } else {
                self.done_b = true;
            }
        } else if !self.done_a {
            if let Some(a) = self.a.next() {
                self.last_a = a;
            } else {
                self.done_a = true;
            }
        } else {
            return None
        }
        Some(Interval(&self.last_a.0 + &self.last_b.0,
                      &self.last_a.1 + &self.last_b.1))
    }
}

impl<A,B> Cut for Add<A,B> where A:Cut, B:Cut {}

pub struct Neg<A>(A);

impl<A> Neg<A> where A:Cut {
    pub fn new(a: A) -> Neg<A> {
        Neg(a)
    }
}

impl<A> Iterator for Neg<A> where A:Cut {
    type Item = Interval;

    fn next(&mut self) -> Option<Interval> {
        self.0.next().map(|i| Interval(-i.1, -i.0))
    }
}

impl<A> Cut for Neg<A> where A:Cut {}

#[test]
fn test_add() {
    let one_fifth: Interval = Rational::ratio(&Int::from(1), &Int::from(5)).into();
    let three_fifth: Interval = Rational::ratio(&Int::from(3), &Int::from(5)).into();
    let add = Add::new(one_fifth.cut(), three_fifth.cut());
    assert_eq!(assert_bounds(10, add), Rational::ratio(&Int::from(4), &Int::from(5)).into());
}

#[test]
fn test_sub_e() {
    let e1 = E::new();
    let e2 = E::new();
    let add = Add::new(e1, Neg::new(e2));
    assert_eq!(assert_bounds(10, add), Interval::ints(-1,840, 1,2520));
}

pub struct Root<N> {
    last: Interval,
    guess: Interval,
    delta: Rational,
    root: u32,
    n: N
}

impl<N> Root<N> where N:Cut {
    pub fn new(root: u32, mut n: N) -> Root<N> {
        assert!(root > 1);
        let init = n.next().unwrap();
        let lower = if init.0 < Rational::one() {
            init.0.clone()
        } else {
            Rational::one()
        };
        let upper = if init.1 < Rational::one() {
            Rational::one()
        } else {
            init.1.clone()
        };
        Root {
            guess: Interval(lower, upper),
            last: init,
            delta: Rational::zero(),
            root: root,
            n: n
        }
    }
}

fn pow(left: &Rational, exp: u32) -> Rational {
    let num = left.get_num().pow(exp as u32);
    let den = left.get_den().pow(exp as u32);
    Rational::ratio(&num, &den)
}

fn sign(x: &Rational) -> i8 {
    if x > &Rational::zero() {
        1
    } else if x < &Rational::zero() {
        -1
    } else {
        0
    }
}

impl<N> Iterator for Root<N> where N:Cut {
    type Item = Interval;

    fn next(&mut self) -> Option<Interval> {
        //         ___
        // y =  \n/ x
        //
        // f(y) = y^n - x = 0
        // f'(y) = ny^(n-1) = 0
        if let Some(term) = self.n.next() {
            self.delta = &term.delta() - &self.last.delta();
            self.last = term;
        }
        //let r_root = Int::from(self.root);
        //let r_root = Rational::ratio(&r_root, &Int::one());
        //               f( x_0 )
        // x_1 = x_0 - -----------
        //              f'( x_0 )
        /*let lower = &self.guess.0 -
            &(&(&pow(&self.guess.0, self.root) - &self.last.0)
              / &(&r_root * &pow(&self.guess.0, self.root - 1)));
        let upper = &self.guess.1 -
            &(&(&pow(&self.guess.1, self.root) - &self.last.1)
        / &(&r_root * &pow(&self.guess.1, self.root - 1)));
        assert!(lower >= self.guess.0, "lower bound loosened: new = {:?}, old = {:?}", lower, self.guess.0);
        assert!(upper <= self.guess.1, "upper bound loosened: new = {:?}, old = {:?}", upper, self.guess.1);
        self.guess = Interval(lower, upper);*/

        // Bisection
        // c <- midpoint(a, b)
        // a <- c if sign(f(c)) = sign(f(a))
        // b <- c if sign(f(c)) != sign(f(a))
        let c = self.guess.midpoint();
        let mut a = self.guess.0.clone();
        let mut b = self.guess.1.clone();
        let ref x = self.last;
        let fa = &pow(&a, self.root) - &x.0;
        let fc = &pow(&c, self.root) - &x.0;
        if sign(&fc) == sign(&fa) {
            a = c;
        } else {
            b = c;
        }
        assert!(a >= self.guess.0, "lower bound loosened: new = {:?}, old = {:?}", a, self.guess.0);
        assert!(b <= self.guess.1, "upper bound loosened: new = {:?}, old = {:?}", b, self.guess.1);
        self.guess = Interval(a, b);
        Some(self.guess.clone())
    }
}

#[test]
fn sqrt_4() {
    let four: Interval = Rational::ratio(&Int::from(4), &Int::one()).into();
    let sqrt = Root::new(2, four.cut());
    assert_eq!(assert_bounds(10, sqrt), Interval::ints(2047,1024, 4097,2048));
}

#[test]
fn sqrt_2() {
    let two: Interval = Rational::ratio(&Int::from(2), &Int::one()).into();
    let sqrt = Root::new(2, two.cut());
    assert_eq!(assert_bounds(10, sqrt), Interval::ints(181,128, 2897,2048));
}

#[test]
fn sqrt_e() {
    let root = Root::new(2, E::new());
    assert_eq!(assert_bounds(10, root), Interval::ints(211,128, 1689,1024));
}
