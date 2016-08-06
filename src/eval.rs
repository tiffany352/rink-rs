use std::collections::HashMap;
use std::collections::BTreeMap;
use gmp::mpz::Mpz;
use gmp::mpq::Mpq;

/// Number type
pub type Num = Mpq;
/// A simple alias to add semantic meaning for when we pass around dimension IDs.
pub type Dim = usize;
/// Alias for the primary representation of dimensionality.
pub type Unit = BTreeMap<Dim, i64>;

/// The basic representation of a number with a unit.
#[derive(Clone)]
pub struct Value(Num, Unit);

/// The evaluation context that contains unit definitions.
pub struct Context {
    dimensions: Vec<String>,
    units: HashMap<String, Value>,
    aliases: HashMap<Unit, String>,
    prefixes: Vec<(String, Value)>,
    pub short_output: bool,
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
        Mpq::new(&num, &den)
    }
}

fn root(left: &Mpq, n: i32) -> Mpq {
    if n < 0 {
        one() / root(left, -n)
    } else {
        let num = left.get_num().root(n as u32);
        let den = left.get_den().root(n as u32);
        Mpq::new(&num, &den)
    }
}

fn to_decimal(rational: &Mpq, fracdigits: u32) -> String {
    use std::char::from_digit;

    let num = rational.get_num();
    let den = rational.get_den();
    let intdigits = (&num / &den).size_in_base(10) as u32;
    let integer = &num / &den;

    let mut buf = String::new();
    let ten = Mpz::from(10);
    let mut cursor = ten.pow(intdigits);
    for _ in 0..intdigits {
        cursor = &cursor / &ten;
        let v: Option<i64> = (&(&(&integer / &cursor) % &ten)).into();
        let v = v.unwrap();
        buf.push(from_digit(v as u32, 10).unwrap());
    }
    if fracdigits > 0 {
        buf.push('.');
        let frac = &(&num * &ten.pow(fracdigits)) / &den;
        let mut cursor = ten.pow(fracdigits);
        for _ in 0..fracdigits {
            cursor = &cursor / &ten;
            let v: Option<i64> = (&(&(&frac / &cursor) % &ten)).into();
            let v = v.unwrap();
            buf.push(from_digit(v as u32, 10).unwrap());
        }
    }
    buf
}

fn to_scientific(rational: &Mpq, prefixdigits: u32) -> String {
    use std::char::from_digit;

    let num = rational.get_num();
    let den = rational.get_den();
    let ten = Mpz::from(10);
    let n = ten.pow(prefixdigits);
    let exp = if &num > &den {
        (&num / &den).size_in_base(10) as i32
    } else {
        2 - ((&den / &num).size_in_base(10) as i32)
    };
    let teneexp = Mpz::from(10).pow(exp.abs() as u32);
    let value = if exp >= 0 {
        &(&(&num * &n) / &den) / &teneexp
    } else {
        &(&(&num * &teneexp) * &n) / &den
    };
    let mut buf = String::new();
    let mut cursor = n.clone();
    for i in 0..prefixdigits {
        if i == 1 {
            buf.push('.');
        }
        cursor = &cursor / &ten;
        let v: Option<i64> = (&(&(&value / &cursor) % &ten)).into();
        let v = v.unwrap();
        buf.push(from_digit(v as u32, 10).unwrap());
    }
    format!("{}e{}", buf, exp - 1)
}

impl Value {
    /// Creates a dimensionless value.
    pub fn new(num: Num) -> Value {
        Value(num, Unit::new())
    }

    /// Creates a value with a single dimension.
    pub fn new_unit(num: Num, unit: Dim) -> Value {
        let mut map = Unit::new();
        map.insert(unit, 1);
        Value(num, map)
    }

    /// Computes the reciprocal (1/x) of the value.
    pub fn invert(&self) -> Value {
        Value(&one() / &self.0,
              self.1.iter()
              .map(|(&k, &power)| (k, -power))
              .collect::<Unit>())
    }

    /// Adds two values. They must have matching units.
    pub fn add(&self, other: &Value) -> Option<Value> {
        if self.1 != other.1 {
            return None
        }
        Some(Value(&self.0 + &other.0, self.1.clone()))
    }

    /// Multiplies two values, also multiplying their units.
    pub fn mul(&self, other: &Value) -> Value {
        let mut val = Unit::new();
        let mut a = self.1.iter().peekable();
        let mut b = other.1.iter().peekable();
        loop {
            match (a.peek().cloned(), b.peek().cloned()) {
                (Some((ka, pa)), Some((kb, pb))) if ka == kb => {
                    // merge
                    let power = pa+pb;
                    if power != 0 {
                        val.insert(ka.clone(), power);
                    }
                    a.next();
                    b.next();
                },
                (Some((ka, _)), Some((kb, vb))) if ka > kb => {
                    // push vb, advance
                    val.insert(kb.clone(), vb.clone());
                    b.next();
                },
                (Some((ka, va)), Some((kb, _))) if ka < kb => {
                    // push va, advance
                    val.insert(ka.clone(), va.clone());
                    a.next();
                },
                (Some(_), Some(_)) => panic!(),
                (None, Some((kb, vb))) => {
                    // push vb, advance
                    val.insert(kb.clone(), vb.clone());
                    b.next();
                },
                (Some((ka, va)), None) => {
                    // push va, advance
                    val.insert(ka.clone(), va.clone());
                    a.next();
                },
                (None, None) => break
            }
        }
        Value(&self.0 * &other.0, val)
    }

    /// Raises a value to a dimensionless integer power.
    pub fn pow(&self, exp: i32) -> Value {
        let unit = self.1.iter()
            .map(|(&k, &power)| (k, power * exp as i64))
            .collect::<Unit>();
        Value(pow(&self.0, exp), unit)
    }

    /// Computes the nth root of a value iff all of its units have
    /// powers divisible by n.
    pub fn root(&self, exp: i32) -> Option<Value> {
        let mut res = Unit::new();
        for (&dim, &power) in &self.1 {
            if power % exp as i64 != 0 {
                return None
            } else {
                res.insert(dim, power / exp as i64);
            }
        }
        Some(Value(root(&self.0, exp), res))
    }
}

impl Context {
    /// Provides a string representation of a Value. We can't impl
    /// Display because it requires access to the Context for the unit
    /// names.
    pub fn show(&self, value: &Value) -> String {
        use std::io::Write;

        let mut out = vec![];
        let mut frac = vec![];
        let mut value = value.clone();
        value.0.canonicalize();
        let num = value.0.get_num();
        let den = value.0.get_den();
        let ten = Mpz::from(10);

        let use_sci_when = 12;
        let sci_digits = 12;
        let approx_prec = 15;
        //let max_exact = 40;

        let start = num.size_in_base(10) as i32 - den.size_in_base(10) as i32;
        let end = 1-(den.size_in_base(10) as i32);

        let end = {
            let n = &(&num * &ten.pow((-end) as u32)) / &den;
            let mut cursor = Mpz::from(1);
            let mut end = end;
            for i in end..start+1 {
                cursor = &cursor * &ten;
                end = i;
                if !n.modulus(&cursor).is_zero() {
                    break;
                }
            }
            end
        };

        let exactdigits = {
            let mut res = None;
            let o = (if end > 0 { 0 } else { -end }) as u32;
            for i in 0..approx_prec {
                if ten.pow(i+o).modulus(&den).is_zero() {
                    res = Some(i);
                    break;
                }
            }
            res
        };

        let spandigits = (start - end) as u32 + 1;

        /*let mut fracdigits = None;
        for i in 0..approx_prec {
            if ten.pow(i).modulus(&den).is_zero() {
                fracdigits = Some(i);
                break;
            }
        }
        let prefixdigits = match fracdigits {
            Some(0) => {
                let mut zerodigits = 0;
                let mut cursor = Mpz::from(1);
                for i in 0..intdigits {
                    if integer.modulus(&cursor).is_zero() {
                        zerodigits = i;
                    } else {
                        break;
                    }
                    cursor = &cursor * &ten;
                }
                intdigits - zerodigits
            },
            Some(fracdigits) => intdigits + fracdigits,
            None => intdigits
        };

        println!("intdigits={} fracdigits={:?} prefixdigits={}", intdigits, fracdigits, prefixdigits);*/

        println!("start={} end={} spandigits={} exactdigits={:?}", start, end, spandigits, exactdigits);

        /*let (exact, approx) =
            if prefixdigits <= sci_digits && fracdigits.map(|f| intdigits+f > use_sci_when).unwrap_or(true) {
                (to_scientific(&value.0, prefixdigits), None)
            } else if let Some(fracdigits) = fracdigits {
                (to_decimal(&value.0, fracdigits), None)
            } else {
                (format!("{:?}", value.0), Some(to_decimal(&value.0, approx_prec)))
            };*/

        let (exact, approx): (String, Option<String>) =
            if exactdigits.map(|e| e+spandigits <= sci_digits).unwrap_or(false) && (end > use_sci_when || start < -use_sci_when) {
                (to_scientific(&value.0, spandigits+exactdigits.unwrap_or(0)), None)
            } else if let Some(exactdigits) = exactdigits {
                (to_decimal(&value.0, spandigits+exactdigits), None)
            } else {
                if end > use_sci_when || start < -use_sci_when {
                    (format!("{:?}", value.0), Some(to_scientific(&value.0, approx_prec)))
                } else {
                    (format!("{:?}", value.0), Some(to_decimal(&value.0, approx_prec)))
                }
            };

        write!(out, "{}", exact).unwrap();
        for (&dim, &exp) in &value.1 {
            if exp < 0 {
                frac.push((dim, exp));
            } else {
                write!(out, " {}", self.dimensions[dim]).unwrap();
                if exp != 1 {
                    write!(out, "^{}", exp).unwrap();
                }
            }
        }
        if frac.len() > 0 {
            write!(out, " /").unwrap();
            for (dim, exp) in frac {
                let exp = -exp;
                write!(out, " {}", self.dimensions[dim]).unwrap();
                if exp != 1 {
                    write!(out, "^{}", exp).unwrap();
                }
            }
        }
        let alias = self.aliases.get(&value.1).cloned().or_else(|| {
            if value.1.len() == 1 {
                let e = value.1.iter().next().unwrap();
                let ref n = self.dimensions[*e.0];
                if *e.1 == 1 {
                    Some(n.clone())
                } else {
                    Some(format!("{}^{}", n, e.1))
                }
            } else {
                None
            }
        });
        match (alias, approx) {
            (Some(alias), Some(approx)) => write!(out, " ({}, approx. {})", alias, approx),
            (Some(alias), None) => write!(out, " ({})", alias),
            (None, Some(approx)) => write!(out, " (approx. {})", approx),
            (None, None) => write!(out, "")
        }.unwrap();
        String::from_utf8(out).unwrap()
    }

    /// Wrapper around show that calls `println!`.
    pub fn print(&self, value: &Value) {
        println!("{}", self.show(value));
    }

    /// Given a unit name, returns its value if it exists. Supports SI
    /// prefixes, plurals, bare dimensions like length, and aliases.
    pub fn lookup(&self, name: &str) -> Option<Value> {
        for (i, ref k) in self.dimensions.iter().enumerate() {
            if name == *k {
                return Some(Value::new_unit(one(), i))
            }
        }
        self.units.get(name).cloned().or_else(|| {
            if name.ends_with("s") {
                if let Some(v) = self.lookup(&name[0..name.len()-1]) {
                    return Some(v)
                }
            }
            for &(ref pre, ref value) in &self.prefixes {
                if name.starts_with(pre) {
                    if let Some(v) = self.lookup(&name[pre.len()..]) {
                        return Some(v.mul(&value))
                    }
                }
            }
            for (unit, alias) in &self.aliases {
                if name == alias {
                    return Some(Value(one(), unit.clone()))
                }
            }
            None
        })
    }

    /// Describes a value's unit, gives true if the unit is reciprocal
    /// (e.g. you should prefix "1.0 / " or replace "multiply" with
    /// "divide" when rendering it).
    pub fn describe_unit(&self, value: &Value) -> (bool, String) {
        use std::io::Write;

        let mut buf = vec![];
        let mut recip = false;
        if let Some(name) = self.aliases.get(&value.1) {
            write!(buf, "{}", name).unwrap();
        } else {
            let mut frac = vec![];
            let mut found = false;
            for (&dim, &pow) in &value.1 {
                if pow < 0 {
                    frac.push((dim, -pow));
                } else {
                    found = true;
                    let mut map = Unit::new();
                    map.insert(dim, pow);
                    if let Some(name) = self.aliases.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
                        map.insert(dim, 1);
                        write!(buf, " {}", self.aliases[&map]).unwrap();
                        if pow != 1 {
                            write!(buf, "^{}", pow).unwrap();
                        }
                    }
                }
            }
            if frac.len() > 0 {
                if !found {
                    recip = true;
                } else {
                    write!(buf, " /").unwrap();
                }
                for (dim, pow) in frac {
                    let mut map = Unit::new();
                    map.insert(dim, pow);
                    if let Some(name) = self.aliases.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
                        map.insert(dim, 1);
                        write!(buf, " {}", self.aliases[&map]).unwrap();
                        if pow != 1 {
                            write!(buf, "^{}", pow).unwrap();
                        }
                    }
                }
            }
        }

        (recip, String::from_utf8(buf).unwrap())
    }

    /// Evaluates an expression to compute its value, including `->`
    /// conversions.
    pub fn eval(&self, expr: &::unit_defs::Expr) -> Result<Value, String> {
        use unit_defs::Expr;

        match *expr {
            Expr::Unit(ref name) => self.lookup(name).ok_or(format!("Unknown unit {}", name)),
            Expr::Const(ref num, ref frac, ref exp) => {
                use std::str::FromStr;

                let num = Mpz::from_str_radix(&*num, 10).unwrap();
                let frac = if let &Some(ref frac) = frac {
                    let frac_digits = frac.len();
                    let frac = Mpz::from_str_radix(&*frac, 10).unwrap();
                    Mpq::new(&frac, &Mpz::from(10).pow(frac_digits as u32))
                } else {
                    zero()
                };
                let exp = if let &Some(ref exp) = exp {
                    let exp: i32 = match FromStr::from_str(&*exp) {
                        Ok(exp) => exp,
                        // presumably because it is too large
                        Err(e) => return Err(format!("Failed to parse exponent: {}", e))
                    };
                    let res = Mpz::from(10).pow(exp.abs() as u32);
                    if exp < 0 {
                        Mpq::new(&Mpz::one(), &res)
                    } else {
                        Mpq::new(&res, &Mpz::one())
                    }
                } else {
                    Mpq::one()
                };
                let num = &Mpq::new(&num, &Mpz::one()) + &frac;
                let num = &num * &exp;
                Ok(Value::new(num))
            },
            Expr::Neg(ref expr) => self.eval(&**expr).and_then(|mut v| {
                v.0 = -v.0;
                Ok(v)
            }),
            Expr::Plus(ref expr) => self.eval(&**expr),
            Expr::Frac(ref top, ref bottom) => match (self.eval(&**top), self.eval(&**bottom)) {
                (Ok(top), Ok(bottom)) => Ok(top.mul(&bottom.invert())),
                (Err(e), _) => Err(e),
                (_, Err(e)) => Err(e),
            },
            Expr::Add(ref top, ref bottom) => match (self.eval(&**top), self.eval(&**bottom)) {
                (Ok(top), Ok(bottom)) => {
                    top.add(&bottom).ok_or_else(|| {
                        format!("Add of values with differing units is not meaningful: {} + {}",
                                    self.show(&top), self.show(&bottom))
                    })
                },
                (Err(e), _) => Err(e),
                (_, Err(e)) => Err(e),
            },
            Expr::Convert(ref top, ref bottom) => match (self.eval(&**top), self.eval(&**bottom)) {
                (Ok(top), Ok(bottom)) => {
                    if top.1 != bottom.1 {
                        use std::io::Write;

                        let mut buf = vec![];
                        let width = 12;
                        let mut topu = top.clone();
                        topu.0 = one();
                        let mut bottomu = bottom.clone();
                        bottomu.0 = one();
                        let left = self.show(&topu);
                        let right = self.show(&bottomu);
                        if self.short_output {
                            writeln!(buf, "Conformance error [ {left} || {right} ]",
                                     left=left, right=right).unwrap();
                        } else {
                            writeln!(buf, concat!("Conformance error\n",
                                                  "{:>width$}: {left}\n",
                                                  "{:>width$}: {right}"),
                                     "Left side", "Right side", left=left, right=right, width=width).unwrap();
                        }
                        let diff = topu.mul(&bottomu.invert());
                        let (recip, desc) = self.describe_unit(&diff.invert());
                        let word = match recip {
                            false => "multiply",
                            true => "divide"
                        };
                        writeln!(buf, "{:>width$}: {word} left side by {}", "Suggestion",
                                 desc.trim(), width=width, word=word).unwrap();
                        let (recip, desc) = self.describe_unit(&diff);
                        let word = match recip {
                            false => "multiply",
                            true => "divide"
                        };
                        writeln!(buf, "{:>width$}  {word} right side by {}", "",
                                 desc.trim(), width=width, word=word).unwrap();

                        Err(String::from_utf8(buf).unwrap())
                    } else {
                        Ok(top.mul(&bottom.invert()))
                    }
                },
                (Err(e), _) => Err(e),
                (_, Err(e)) => Err(e),
            },
            Expr::Mul(ref args) => args.iter().fold(Ok(Value::new(one())), |a, b| {
                a.and_then(|a| {
                    let b = try!(self.eval(b));
                    Ok(a.mul(&b))
                })
            }),
            Expr::Pow(ref base, ref exp) => {
                let base = try!(self.eval(&**base));
                let exp = try!(self.eval(&**exp));
                let fexp: f64 = exp.0.into();
                if exp.1.len() != 0 {
                    Err(format!("Exponent not dimensionless"))
                } else if fexp.trunc() == fexp {
                    Ok(base.pow(fexp as i32))
                } else if (1.0 / fexp).trunc() == 1.0 / fexp {
                    base.root((1.0 / fexp) as i32).ok_or(format!("Unit roots must result in integer dimensions"))
                } else {
                    Err(format!("Exponent not integer"))
                }
            },
            Expr::Error(ref e) => Err(e.clone()),
        }
    }

    /// Takes a parsed units.txt from `unit_defs::parse()`. Prints if
    /// there are errors in the file.
    pub fn new(defs: ::unit_defs::Defs) -> Context {
        use unit_defs::Def;

        let mut ctx = Context {
            dimensions: Vec::new(),
            units: HashMap::new(),
            aliases: HashMap::new(),
            prefixes: Vec::new(),
            short_output: false,
        };

        ctx.prefixes.sort_by(|a, b| a.0.cmp(&b.0));

        for (name, def) in defs.defs {
            match *def {
                Def::Dimension(ref dname) => {
                    let i = ctx.dimensions.len();
                    ctx.dimensions.push(dname.clone());
                    let mut map = Unit::new();
                    map.insert(i, 1);
                    ctx.aliases.insert(map, name.clone());
                },
                Def::Unit(ref expr) => match ctx.eval(expr) {
                    Ok(v) => {
                        ctx.units.insert(name.clone(), v);
                    },
                    Err(e) => println!("Unit {} is malformed: {}", name, e)
                },
                Def::Prefix(ref expr) => match ctx.eval(expr) {
                    Ok(v) => {
                        ctx.prefixes.push((name.clone(), v));
                    },
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::SPrefix(ref expr) => match ctx.eval(expr) {
                    Ok(v) => {
                        ctx.prefixes.push((name.clone(), v.clone()));
                        ctx.units.insert(name.clone(), v);
                    },
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::Error(ref err) => println!("Def {}: {}", name, err),
            };
        }

        for (expr, name) in defs.aliases {
            match ctx.eval(&expr) {
                Ok(v) => {
                    ctx.aliases.insert(v.1, name);
                },
                Err(e) => println!("Alias {}: {}", name, e)
            }
        }

        ctx
    }
}
