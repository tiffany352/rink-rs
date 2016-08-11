use gmp::mpq::Mpq;
use gmp::mpz::Mpz;
use std::collections::BTreeMap;

/// Number type
pub type Num = Mpq;
/// A simple alias to add semantic meaning for when we pass around dimension IDs.
pub type Dim = usize;
/// Alias for the primary representation of dimensionality.
pub type Unit = BTreeMap<Dim, i64>;

/// The basic representation of a number with a unit.
#[derive(Clone)]
pub struct Number(pub Num, pub Unit);

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

    /// Computes the reciprocal (1/x) of the value.
    pub fn invert(&self) -> Number {
        Number(&one() / &self.0,
              self.1.iter()
              .map(|(&k, &power)| (k, -power))
              .collect::<Unit>())
    }

    /// Adds two values. They must have matching units.
    pub fn add(&self, other: &Number) -> Option<Number> {
        if self.1 != other.1 {
            return None
        }
        Some(Number(&self.0 + &other.0, self.1.clone()))
    }

    /// Multiplies two values, also multiplying their units.
    pub fn mul(&self, other: &Number) -> Number {
        let val = ::btree_merge(&self.1, &other.1, |a, b| if a+b != 0 { Some(a + b) } else { None });
        Number(&self.0 * &other.0, val)
    }

    /// Raises a value to a dimensionless integer power.
    pub fn pow(&self, exp: i32) -> Number {
        let unit = self.1.iter()
            .map(|(&k, &power)| (k, power * exp as i64))
            .collect::<Unit>();
        Number(pow(&self.0, exp), unit)
    }

    /// Computes the nth root of a value iff all of its units have
    /// powers divisible by n.
    pub fn root(&self, exp: i32) -> Option<Number> {
        let mut res = Unit::new();
        for (&dim, &power) in &self.1 {
            if power % exp as i64 != 0 {
                return None
            } else {
                res.insert(dim, power / exp as i64);
            }
        }
        Some(Number(root(&self.0, exp), res))
    }
}
