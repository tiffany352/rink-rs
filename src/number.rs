// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use gmp::mpq::Mpq;
use gmp::mpz::Mpz;
use std::collections::BTreeMap;
use eval::Show;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::rc::Rc;
use std::fmt;
use std::borrow::Borrow;

/// Number type
pub type Num = Mpq;
/// Alias for the primary representation of dimensionality.
pub type Unit = BTreeMap<Dim, i64>;

/// A newtype for a string dimension ID, so that we can implement traits for it.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Dim(pub Rc<String>);

/// The basic representation of a number with a unit.
#[derive(Clone, PartialEq, Eq)]
pub struct Number(pub Num, pub Unit);

impl Borrow<str> for Dim {
    fn borrow(&self) -> &str {
        &**self.0
    }
}

impl fmt::Display for Dim {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(fmt)
    }
}

impl Dim {
    pub fn new(dim: &str) -> Dim {
        Dim(Rc::new(dim.to_owned()))
    }
}

fn one() -> Mpq {
    Mpq::one()
}

fn zero() -> Mpq {
    Mpq::zero()
}

fn pow(left: &Mpq, exp: i32) -> Mpq {
    if exp < 0 {
        one() / pow(left, -exp)
    } else {
        let num = left.get_num().pow(exp as u32);
        let den = left.get_den().pow(exp as u32);
        Mpq::ratio(&num, &den)
    }
}

fn root(left: &Mpq, n: i32) -> Mpq {
    if n < 0 {
        one() / root(left, -n)
    } else {
        let num = left.get_num().root(n as u32);
        let den = left.get_den().root(n as u32);
        Mpq::ratio(&num, &den)
    }
}

pub fn to_string(rational: &Mpq) -> (bool, String) {
    use std::char::from_digit;

    let sign = *rational < Mpq::zero();
    let rational = rational.abs();
    let num = rational.get_num();
    let den = rational.get_den();
    let intdigits = (&num / &den).size_in_base(10) as u32;

    let mut buf = String::new();
    if sign {
        buf.push('-');
    }
    let zero = Mpq::zero();
    let one = Mpz::one();
    let ten = Mpz::from(10);
    let ten_mpq = Mpq::ratio(&ten, &one);
    let mut cursor = rational / Mpq::ratio(&ten.pow(intdigits), &one);
    let mut n = 0;
    let mut only_zeros = true;
    let mut zeros = 0;
    let mut placed_decimal = false;
    loop {
        let exact = cursor == zero;
        let use_sci = intdigits+zeros > 9;
        let placed_ints = n >= intdigits;
        let bail =
            (exact && (placed_ints || use_sci)) ||
            (n as i32 - zeros as i32 > 6 && use_sci) ||
            n as i32 - zeros as i32 > ::std::cmp::max(intdigits as i32, 6);
        if bail && use_sci {
            // scientific notation
            buf = buf[zeros as usize + placed_decimal as usize + sign as usize..].to_owned();
            buf.insert(1, '.');
            if buf.len() == 2 {
                buf.insert(2, '0');
            }
            if sign {
                buf.insert(0, '-');
            }
            buf.push_str(&*format!("e{}", intdigits as i32 - zeros as i32 - 1));
            return (exact, buf)
        }
        if bail {
            return (exact, buf)
        }
        if n == intdigits {
            buf.push('.');
            placed_decimal = true;
        }
        let digit = &(&(&cursor.get_num() * &ten) / &cursor.get_den()) % &ten;
        let v: Option<i64> = (&digit).into();
        let v = v.unwrap();
        if v != 0 {
            only_zeros = false
        } else if only_zeros {
            zeros += 1;
        }
        if !(v == 0 && only_zeros && n < intdigits-1) {
            buf.push(from_digit(v as u32, 10).unwrap());
        }
        cursor = &cursor * &ten_mpq;
        cursor = &cursor - &Mpq::ratio(&digit, &one);
        n += 1;
    }
}

impl Number {
    pub fn one() -> Number {
        Number(one(), Unit::new())
    }

    pub fn one_unit(unit: Dim) -> Number {
        Number::new_unit(one(), unit)
    }

    pub fn zero() -> Number {
        Number(zero(), Unit::new())
    }

    /// Creates a dimensionless value.
    pub fn new(num: Num) -> Number {
        Number(num, Unit::new())
    }

    /// Creates a value with a single dimension.
    pub fn new_unit(num: Num, unit: Dim) -> Number {
        let mut map = Unit::new();
        map.insert(unit, 1);
        Number(num, map)
    }

    pub fn from_parts(integer: &str, frac: Option<&str>, exp: Option<&str>) -> Result<Number, String> {
        use std::str::FromStr;

        let num = Mpz::from_str_radix(integer, 10).unwrap();
        let frac = if let Some(ref frac) = frac {
            let frac_digits = frac.len();
            let frac = Mpz::from_str_radix(&*frac, 10).unwrap();
            Mpq::ratio(&frac, &Mpz::from(10).pow(frac_digits as u32))
        } else {
            Mpq::zero()
        };
        let exp = if let Some(ref exp) = exp {
            let exp: i32 = match FromStr::from_str(&*exp) {
                Ok(exp) => exp,
                // presumably because it is too large
                Err(e) => return Err(format!("Failed to parse exponent: {}", e))
            };
            let res = Mpz::from(10).pow(exp.abs() as u32);
            if exp < 0 {
                Mpq::ratio(&Mpz::one(), &res)
            } else {
                Mpq::ratio(&res, &Mpz::one())
            }
        } else {
            Mpq::one()
        };
        let num = &Mpq::ratio(&num, &Mpz::one()) + &frac;
        let num = &num * &exp;
        Ok(Number::new(num))
    }

    /// Computes the reciprocal (1/x) of the value.
    pub fn invert(&self) -> Number {
        Number(&one() / &self.0,
               self.1.iter()
               .map(|(k, &power)| (k.clone(), -power))
               .collect::<Unit>())
    }

    /// Raises a value to a dimensionless integer power.
    pub fn powi(&self, exp: i32) -> Number {
        let unit = self.1.iter()
            .map(|(k, &power)| (k.clone(), power * exp as i64))
            .collect::<Unit>();
        Number(pow(&self.0, exp), unit)
    }

    /// Computes the nth root of a value iff all of its units have
    /// powers divisible by n.
    pub fn root(&self, exp: i32) -> Result<Number, String> {
        if self.0 < Mpq::zero() {
            return Err(format!("Complex numbers are not implemented"))
        }
        let mut res = Unit::new();
        for (dim, &power) in &self.1 {
            if power % exp as i64 != 0 {
                return Err(format!(
                    "Result must have integer dimensions"))
            } else {
                res.insert(dim.clone(), power / exp as i64);
            }
        }
        Ok(Number(root(&self.0, exp), res))
    }

    pub fn pow(&self, exp: &Number) -> Result<Number, String> {
        use std::convert::Into;

        if exp.1.len() != 0 {
            return Err(format!("Exponent must be dimensionless"))
        }
        let mut exp = exp.0.clone();
        exp.canonicalize();
        let num = exp.get_num();
        let den = exp.get_den();
        let one = Mpz::one();
        if den == one {
            let exp: Option<i64> = (&num).into();
            Ok(self.powi(exp.unwrap() as i32))
        } else if num == one {
            let exp: Option<i64> = (&den).into();
            self.root(exp.unwrap() as i32)
        } else {
            Err(format!("Exponent must be either an integer or the reciprocal of an integer"))
        }
    }

    pub fn show_number_part(&self) -> String {
        use std::io::Write;

        let mut out = vec![];
        let mut value = self.clone();
        value.0.canonicalize();

        let (exact, approx) = match to_string(&value.0) {
            (true, v) => (v, None),
            (false, v) => if value.0.get_den() > Mpz::from(1_000_000) || value.0.get_num() > Mpz::from(1_000_000_000u64) {
                (format!("approx. {}", v), None)
            } else {
                (format!("{:?}", value.0), Some(v))
            }
        };

        write!(out, "{}", exact).unwrap();
        if let Some(approx) = approx {
            write!(out, ", approx. {}", approx).unwrap();
        }

        String::from_utf8(out).unwrap()
    }

    pub fn unit_to_string(unit: &Unit) -> String {
        use std::io::Write;

        let mut out = vec![];
        let mut frac = vec![];

        for (dim, &exp) in unit {
            if exp < 0 {
                frac.push((dim, exp));
            } else {
                write!(out, " {}", dim).unwrap();
                if exp != 1 {
                    write!(out, "^{}", exp).unwrap();
                }
            }
        }
        if frac.len() > 0 {
            write!(out, " /").unwrap();
            for (dim, exp) in frac {
                let exp = -exp;
                write!(out, " {}", dim).unwrap();
                if exp != 1 {
                    write!(out, "^{}", exp).unwrap();
                }
            }
        }

        if out.len() > 0 {
            out.remove(0);
        }
        String::from_utf8(out).unwrap()
    }

    pub fn unit_name(&self, context: &::eval::Context) -> String {
        let pretty = ::factorize::fast_decompose(self, &context.reverse);
        let pretty = Number::unit_to_string(&pretty);
        pretty
    }

    pub fn complexity_score(&self) -> i64 {
        self.1.iter().map(|(_, p)| 1 + p.abs()).fold(0, |a,x| a+x)
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.show_number_part())
    }
}

impl Show for Number {
    fn show(&self, context: &::eval::Context) -> String {
        use std::io::Write;

        let mut out = vec![];
        let mut value = self.clone();
        value.0.canonicalize();

        write!(out, "{}", self.show_number_part()).unwrap();
        let unit = self.unit_name(context);
        if unit.len() > 0 {
            write!(out, " {}", unit).unwrap();
        }

        let alias = context.aliases.get(&value.1).cloned().or_else(|| {
            if value.1.len() == 1 {
                let e = value.1.iter().next().unwrap();
                let ref n = *e.0;
                if *e.1 == 1 {
                    Some((&*n.0).clone())
                } else {
                    Some(format!("{}^{}", n, e.1))
                }
            } else {
                None
            }
        });
        if let Some(alias) = alias {
            write!(out, " ({})", alias).unwrap();
        }
        String::from_utf8(out).unwrap()
    }
}

impl<'a, 'b> Add<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn add(self, other: &Number) -> Self::Output {
        if self.1 != other.1 {
            return None
        }
        Some(Number(&self.0 + &other.0, self.1.clone()))
    }
}

impl<'a, 'b> Sub<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn sub(self, other: &Number) -> Self::Output {
        if self.1 != other.1 {
            return None
        }
        Some(Number(&self.0 - &other.0, self.1.clone()))
    }
}

impl<'a> Neg for &'a Number {
    type Output = Option<Number>;

    fn neg(self) -> Self::Output {
        Some(Number(-&self.0, self.1.clone()))
    }
}

impl<'a, 'b> Mul<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn mul(self, other: &Number) -> Self::Output {
        let val = ::btree_merge(&self.1, &other.1, |a, b| if a+b != 0 { Some(a + b) } else { None });
        Some(Number(&self.0 * &other.0, val))
    }
}

impl<'a, 'b> Div<&'b Number> for &'a Number {
    type Output = Option<Number>;

    fn div(self, other: &Number) -> Self::Output {
        if other.0 == zero() {
            None
        } else {
            self * &other.invert()
        }
    }
}
