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

fn to_string(rational: &Mpq) -> (bool, String) {
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

fn btree_merge<K: ::std::cmp::Ord+Clone, V:Clone, F:Fn(&V, &V) -> Option<V>>(
    left: &BTreeMap<K, V>, right: &BTreeMap<K, V>, merge_func: F
) -> BTreeMap<K, V> {
    let mut res = BTreeMap::new();
    let mut a = left.iter().peekable();
    let mut b = right.iter().peekable();
    loop {
        match (a.peek().cloned(), b.peek().cloned()) {
            (Some((akey, aval)), Some((bkey, bval))) if akey == bkey => {
                if let Some(v) = merge_func(aval, bval) {
                    res.insert(akey.clone(), v);
                }
                a.next();
                b.next();
            },
            (Some((akey, _)), Some((bkey, bval))) if akey > bkey => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            },
            (Some((akey, aval)), Some((bkey, _))) if akey < bkey => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            },
            (Some(_), Some(_)) => panic!(),
            (None, Some((bkey, bval))) => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            },
            (Some((akey, aval)), None) => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            },
            (None, None) => break,
        }
    }
    res
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
        let val = btree_merge(&self.1, &other.1, |a, b| if a+b != 0 { Some(a + b) } else { None });
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
        if let Some(alias) = alias {
            write!(out, " ({})", alias).unwrap();
        }
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

    /// Evaluates an expression to compute its value, *excluding* `->`
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
                    Mpq::ratio(&frac, &Mpz::from(10).pow(frac_digits as u32))
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
                        Mpq::ratio(&Mpz::one(), &res)
                    } else {
                        Mpq::ratio(&res, &Mpz::one())
                    }
                } else {
                    Mpq::one()
                };
                let num = &Mpq::ratio(&num, &Mpz::one()) + &frac;
                let num = &num * &exp;
                Ok(Value::new(num))
            },
            Expr::Neg(ref expr) => self.eval(&**expr).and_then(|mut v| {
                v.0 = -v.0;
                Ok(v)
            }),
            Expr::Plus(ref expr) => self.eval(&**expr),
            Expr::Frac(ref top, ref bottom) => match (self.eval(&**top), self.eval(&**bottom)) {
                (Ok(top), Ok(bottom)) => {
                    if bottom.0 == Mpq::zero() {
                        return Err(format!("Division by zero: {} / {}", self.show(&top), self.show(&bottom)))
                    }
                    Ok(top.mul(&bottom.invert()))
                },
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
            Expr::Mul(ref args) => args.iter().fold(Ok(Value::new(one())), |a, b| {
                a.and_then(|a| {
                    let b = try!(self.eval(b));
                    Ok(a.mul(&b))
                })
            }),
            Expr::Pow(ref base, ref exp) => {
                let base = try!(self.eval(&**base));
                let exp = try!(self.eval(&**exp));
                let fexp: f64 = exp.0.to_f64();
                if exp.1.len() != 0 {
                    Err(format!("Exponent not dimensionless"))
                } else if fexp.trunc() == fexp {
                    let iexp = fexp as i32;
                    if iexp < 0 && base.0 == Mpq::zero() {
                        return Err(format!("Disivion by zero: {}^{}", self.show(&base), iexp))
                    }
                    Ok(base.pow(fexp as i32))
                } else if (1.0 / fexp).trunc() == 1.0 / fexp {
                    if base.0 < Mpq::zero() {
                        return Err(format!("Root of a negative number is imaginary, which is not yet implemented: {}^{}", self.show(&base), fexp))
                    }
                    base.root((1.0 / fexp) as i32).ok_or(format!("Unit roots must result in integer dimensions"))
                } else {
                    Err(format!("Exponent not integer"))
                }
            },
            Expr::Convert(_, _) => Err(format!("Conversions (->) must be top-level expressions")),
            Expr::Error(ref e) => Err(e.clone()),
        }
    }

    pub fn eval_unit_name(&self, expr: &::unit_defs::Expr) -> Result<BTreeMap<String, isize>, String> {
        use unit_defs::Expr;

        match *expr {
            Expr::Unit(ref name) => {
                let mut map = BTreeMap::new();
                map.insert(name.clone(), 1);
                Ok(map)
            },
            Expr::Const(ref x, None, None) if x == "1" || x == "-1" => Ok(BTreeMap::new()),
            Expr::Const(_, _, _) => Err(format!("Constants are not allowed in the right hand side of conversions")),
            Expr::Frac(ref left, ref right) => {
                let left = try!(self.eval_unit_name(left));
                let right = try!(self.eval_unit_name(right)).into_iter()
                    .map(|(k,v)| (k, -v)).collect::<BTreeMap<_, _>>();
                Ok(btree_merge(&left, &right, |a,b| if a+b != 0 { Some(a + b) } else { None }))
            },
            Expr::Mul(ref args) => {
                args[1..].iter().fold(self.eval_unit_name(&args[0]), |acc, b| {
                    let acc = try!(acc);
                    let b = try!(self.eval_unit_name(b));
                    Ok(btree_merge(&acc, &b, |a,b| if a+b != 0 { Some(a+b) } else { None }))
                })
            },
            Expr::Pow(ref left, ref exp) => {
                let res = try!(self.eval(exp));
                if res.1.len() > 0 {
                    return Err(format!("Exponents must be dimensionless"))
                }
                Ok(try!(self.eval_unit_name(left)).into_iter()
                   .filter_map(|(k, v)| {
                       let v = v + res.0.to_f64() as isize;
                       if v != 0 {
                           Some((k, v))
                       } else {
                           None
                       }
                   })
                   .collect::<BTreeMap<_, _>>())
            },
            Expr::Add(ref left, ref right) => {
                let left = try!(self.eval_unit_name(left));
                let right = try!(self.eval_unit_name(right));
                if left != right {
                    return Err(format!("Add of values with differing dimensions is not meaningful"))
                }
                Ok(left)
            },
            Expr::Neg(ref v) => self.eval_unit_name(v),
            Expr::Plus(ref v) => self.eval_unit_name(v),
            Expr::Convert(_, _) => Err(format!("Conversions are not allowed in the right hand of conversions")),
            Expr::Error(ref e) => Err(e.clone()),
        }
    }

    /// Evaluates an expression, include `->` conversions.
    pub fn eval_outer(&self, expr: &::unit_defs::Expr) -> Result<String, String> {
        use unit_defs::Expr;

        match *expr {
            Expr::Convert(ref top, ref bottom) => match (self.eval(&**top), self.eval(&**bottom), self.eval_unit_name(&**bottom)) {
                (Ok(top), Ok(bottom), Ok(bottom_name)) => {
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
                        let raw = &top.0 / &bottom.0;
                        let (raw_exact, raw) = to_string(&raw);
                        let approx = if raw_exact {
                            format!("")
                        } else {
                            format!("approx. ")
                        };
                        let mut unit_top = vec![];
                        let mut unit_frac = vec![];
                        for (name, exp) in bottom_name.into_iter() {
                            if exp < 0 {
                                unit_frac.push((name, -exp));
                            } else {
                                unit_top.push((name, exp));
                            }
                        }
                        let unit_top = unit_top.into_iter().fold(String::new(), |mut acc, (name, exp)| {
                            acc.push(' ');
                            acc.push_str(&*name);
                            if exp != 1 {
                                acc.push_str(&*format!("^{}", exp));
                            }
                            acc
                        });
                        let unit_frac = unit_frac.into_iter().fold(String::new(), |mut acc, (name, exp)| {
                            if acc.len() > 0 {
                                acc.push(' ');
                            }
                            acc.push_str(&*name);
                            if exp != 1 {
                                acc.push_str(&*format!("^{}", exp));
                            }
                            acc
                        });
                        let unit_frac = if unit_frac.len() > 0 {
                            format!(" / {}", unit_frac)
                        } else {
                            unit_frac
                        };
                        let reduced = match self.describe_unit(&bottom) {
                            (false, v) => v,
                            (true, v) => format!("1 / {}", v)
                        };
                        Ok(format!("{approx}{raw}{unit_top}{unit_frac} ({reduced})",
                                   approx=approx, raw=raw, unit_top=unit_top,
                                   unit_frac=unit_frac, reduced=reduced))
                    }
                },
                (Err(e), _, _) => Err(e),
                (_, Err(e), _) => Err(e),
                (_, _, Err(e)) => Err(e),
            },
            _ => {
                let val = try!(self.eval(expr));
                Ok(self.show(&val))
            },
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
