use std::collections::HashMap;
use std::collections::BTreeMap;

pub type Dim = usize;
pub type Unit = BTreeMap<Dim, i64>;

#[derive(Clone)]
pub struct Value(f64, Unit);

pub struct Context {
    dimensions: Vec<String>,
    units: HashMap<String, Value>,
    aliases: HashMap<Unit, String>,
    prefixes: Vec<(String, Value)>,
}

impl Value {
    pub fn new(num: f64) -> Value {
        Value(num, Unit::new())
    }

    pub fn new_unit(num: f64, unit: Dim) -> Value {
        let mut map = Unit::new();
        map.insert(unit, 1);
        Value(num, map)
    }

    pub fn invert(&self) -> Value {
        Value(1.0 / self.0,
              self.1.iter()
              .map(|(&k, &power)| (k, -power))
              .collect::<Unit>())
    }

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
        Value(self.0 * other.0, val)
    }

    pub fn pow(&self, exp: i32) -> Value {
        Value(self.0.powi(exp),
              self.1.iter()
              .map(|(&k, &power)| (k, power * exp as i64))
              .collect::<Unit>())
    }

    pub fn root(&self, exp: i32) -> Option<Value> {
        let mut res = Unit::new();
        for (&dim, &power) in &self.1 {
            if power % exp as i64 != 0 {
                return None
            } else {
                res.insert(dim, power / exp as i64);
            }
        }
        Some(Value(self.0.powf(1.0 / exp as f64), res))
    }
}

impl Context {
    pub fn show(&self, value: &Value) -> String {
        use std::io::Write;

        let mut out = vec![];
        let mut frac = vec![];
        write!(out, "{}", value.0).unwrap();
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
        if let Some(name) = self.aliases.get(&value.1) {
            write!(out, " ({})", name).unwrap();
        } else if value.1.len() == 1 {
            write!(out, " ({})", self.dimensions[*value.1.iter().next().unwrap().0]).unwrap();
        }
        String::from_utf8(out).unwrap()
    }

    pub fn print(&self, value: &Value) {
        println!("{}", self.show(value));
    }

    pub fn lookup(&self, name: &str) -> Option<Value> {
        for (i, ref k) in self.dimensions.iter().enumerate() {
            if name == *k {
                return Some(Value::new_unit(1.0, i))
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
            None
        })
    }

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

    pub fn eval(&self, expr: &::unit_defs::Expr) -> Result<Value, String> {
        use unit_defs::Expr;

        match *expr {
            Expr::Unit(ref name) => self.lookup(name).ok_or(format!("Unknown unit {}", name)),
            Expr::Const(num) => Ok(Value::new(num)),
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
            Expr::Convert(ref top, ref bottom) => match (self.eval(&**top), self.eval(&**bottom)) {
                (Ok(top), Ok(bottom)) => {
                    if top.1 != bottom.1 {
                        use std::io::Write;

                        let mut buf = vec![];
                        let width = 12;
                        writeln!(buf, "Conformance error").unwrap();
                        let mut topu = top.clone();
                        topu.0 = 1.0;
                        let mut bottomu = bottom.clone();
                        bottomu.0 = 1.0;
                        writeln!(buf, "{:>width$}: {}", "Left side", self.show(&topu), width=width).unwrap();
                        writeln!(buf, "{:>width$}: {}", "Right side", self.show(&bottomu), width=width).unwrap();
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
            Expr::Mul(ref args) => args.iter().fold(Ok(Value::new(1.0)), |a, b| {
                a.and_then(|a| {
                    let b = try!(self.eval(b));
                    Ok(a.mul(&b))
                })
            }),
            Expr::Pow(ref base, ref exp) => {
                let base = try!(self.eval(&**base));
                let exp = try!(self.eval(&**exp));
                if exp.1.len() != 0 {
                    Err(format!("Exponent not dimensionless"))
                } else if exp.0.trunc() == exp.0 {
                    Ok(base.pow(exp.0 as i32))
                } else if (1.0 / exp.0).trunc() == 1.0 / exp.0 {
                    base.root((1.0 / exp.0) as i32).ok_or(format!("Unit roots must result in integer dimensions"))
                } else {
                    Err(format!("Exponent not integer"))
                }
            },
            Expr::Error(ref e) => Err(e.clone()),
        }
    }

    pub fn new(defs: ::unit_defs::Defs) -> Context {
        use unit_defs::Def;

        let mut ctx = Context {
            dimensions: Vec::new(),
            units: HashMap::new(),
            aliases: HashMap::new(),
            prefixes: Vec::new(),
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
