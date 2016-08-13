use std::collections::HashMap;
use std::collections::BTreeMap;
use gmp::mpq::Mpq;
use chrono::{DateTime, FixedOffset};
use number::{Number, Unit};
use date;
use unit_defs::DatePattern;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Number(Number),
    DateTime(DateTime<FixedOffset>),
}

/// The evaluation context that contains unit definitions.
pub struct Context {
    pub dimensions: Vec<Rc<String>>,
    pub units: HashMap<String, Number>,
    pub aliases: HashMap<Unit, String>,
    pub prefixes: Vec<(String, Number)>,
    pub datepatterns: Vec<Vec<DatePattern>>,
    pub short_output: bool,
}

pub trait Show {
    /// Provides a string representation of something, using information contained in a Context.
    fn show(&self, context: &Context) -> String;
}

impl Show for Value {
    fn show(&self, context: &Context) -> String {
        match *self {
            Value::Number(ref num) => num.show(context),
            Value::DateTime(ref dt) => format!("{}", dt)
        }
    }
}

impl Value {
    fn pow(&self, exp: &Value) -> Result<Value, String> {
        match (self, exp) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                left.pow(right).map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Add<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn add(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left + right)
                .ok_or(format!("Addition of units with mismatched units is not meaningful"))
                .map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Sub<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn sub(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left - right)
                .ok_or(format!("Subtraction of units with mismatched units is not meaningful"))
                .map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a> Neg for &'a Value {
    type Output = Result<Value, String>;

    fn neg(self) -> Self::Output {
        match *self {
            Value::Number(ref num) =>
                (-num).ok_or(format!("Bug: Negation should not fail")).map(Value::Number),
            _ => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Mul<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn mul(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left * right)
                .ok_or(format!("Bug: Mul should not fail"))
                .map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Div<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn div(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left / right)
                .ok_or(format!("Division by zero"))
                .map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl Context {
    /// Wrapper around show that calls `println!`.
    pub fn print(&self, value: &Number) {
        println!("{}", value.show(self));
    }

    /// Given a unit name, returns its value if it exists. Supports SI
    /// prefixes, plurals, bare dimensions like length, and aliases.
    pub fn lookup(&self, name: &str) -> Option<Number> {
        for k in &self.dimensions {
            if name == &***k {
                return Some(Number::one_unit(k.to_owned()))
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
                        return Some((&v * &value).unwrap())
                    }
                }
            }
            for (unit, alias) in &self.aliases {
                if name == alias {
                    return Some(Number(Number::one().0, unit.clone()))
                }
            }
            None
        })
    }

    /// Describes a value's unit, gives true if the unit is reciprocal
    /// (e.g. you should prefix "1.0 / " or replace "multiply" with
    /// "divide" when rendering it).
    pub fn describe_unit(&self, value: &Number) -> (bool, String) {
        use std::io::Write;

        let mut buf = vec![];
        let mut recip = false;
        if let Some(name) = self.aliases.get(&value.1) {
            write!(buf, "{}", name).unwrap();
        } else {
            let mut frac = vec![];
            let mut found = false;
            for (dim, &pow) in &value.1 {
                if pow < 0 {
                    frac.push((dim, -pow));
                } else {
                    found = true;
                    let mut map = Unit::new();
                    map.insert(dim.clone(), pow);
                    if let Some(name) = self.aliases.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
                        map.insert(dim.clone(), 1);
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
                    map.insert(dim.clone(), pow);
                    if let Some(name) = self.aliases.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
                        map.insert(dim.clone(), 1);
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

        macro_rules! operator {
            ($left:ident $op:ident $opname:tt $right:ident) => {{
                let left = try!(self.eval(&**$left));
                let right = try!(self.eval(&**$right));
                ((&left).$op(&right)).map_err(|e| {
                    format!("{}: <{}> {} <{}>",
                            e, left.show(self), stringify!($opname), right.show(self))
                })
            }}
        }

        match *expr {
            Expr::Unit(ref name) => self.lookup(name).ok_or(format!("Unknown unit {}", name)).map(Value::Number),
            Expr::Const(ref num, ref frac, ref exp) =>
                Number::from_parts(
                    num,
                    frac.as_ref().map(AsRef::as_ref),
                    exp.as_ref().map(AsRef::as_ref))
                .map(Value::Number),
            Expr::Date(ref date) => date::try_decode(date, self).map(Value::DateTime),
            Expr::Neg(ref expr) => self.eval(&**expr).and_then(|v| -&v),
            Expr::Plus(ref expr) => self.eval(&**expr),

            Expr::Frac(ref left, ref right) => operator!(left div / right),
            Expr::Add(ref left, ref right)  => operator!(left add + right),
            Expr::Sub(ref left, ref right)  => operator!(left sub - right),
            Expr::Pow(ref left, ref right)  => operator!(left pow ^ right),

            // TODO: A type might not implement * on Number, and this would fail
            Expr::Mul(ref args) => args.iter().fold(Ok(Value::Number(Number::one())), |a, b| {
                a.and_then(|a| {
                    let b = try!(self.eval(b));
                    Ok((&a * &b).unwrap())
                })
            }),
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
                Ok(::btree_merge(&left, &right, |a,b| if a+b != 0 { Some(a + b) } else { None }))
            },
            Expr::Mul(ref args) => {
                args[1..].iter().fold(self.eval_unit_name(&args[0]), |acc, b| {
                    let acc = try!(acc);
                    let b = try!(self.eval_unit_name(b));
                    Ok(::btree_merge(&acc, &b, |a,b| if a+b != 0 { Some(a+b) } else { None }))
                })
            },
            Expr::Pow(ref left, ref exp) => {
                let res = try!(self.eval(exp));
                let res = match res {
                    Value::Number(num) => num,
                    _ => return Err(format!("Exponents must be numbers"))
                };
                if res.1.len() > 0 {
                    return Err(format!("Exponents must be dimensionless"))
                }
                let res: f64 = res.0.into();
                Ok(try!(self.eval_unit_name(left)).into_iter()
                   .filter_map(|(k, v)| {
                       let v = v + res as isize;
                       if v != 0 {
                           Some((k, v))
                       } else {
                           None
                       }
                   })
                   .collect::<BTreeMap<_, _>>())
            },
            Expr::Add(ref left, ref right) | Expr::Sub(ref left, ref right) => {
                let left = try!(self.eval_unit_name(left));
                let right = try!(self.eval_unit_name(right));
                if left != right {
                    return Err(format!("Add of values with differing dimensions is not meaningful"))
                }
                Ok(left)
            },
            Expr::Neg(ref v) => self.eval_unit_name(v),
            Expr::Plus(ref v) => self.eval_unit_name(v),
            Expr::Date(_) => Err(format!("Dates are not allowed in the right hand side of conversions")),
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
                    let (top, bottom) = match (top, bottom) {
                        (Value::Number(top), Value::Number(bottom)) => (top, bottom),
                        _ => return Err(format!("Conversion of non-numbers is not defined"))
                    };
                    if top.1 != bottom.1 {
                        use std::io::Write;

                        let mut buf = vec![];
                        let width = 12;
                        let mut topu = top.clone();
                        topu.0 = Mpq::one();
                        let mut bottomu = bottom.clone();
                        bottomu.0 = Mpq::one();
                        let left = topu.show(self);
                        let right = bottomu.show(self);
                        if self.short_output {
                            writeln!(buf, "Conformance error [ {left} || {right} ]",
                                     left=left, right=right).unwrap();
                        } else {
                            writeln!(buf, concat!("Conformance error\n",
                                                  "{:>width$}: {left}\n",
                                                  "{:>width$}: {right}"),
                                     "Left side", "Right side", left=left, right=right, width=width).unwrap();
                        }
                        let diff = (&topu / &bottomu).unwrap();
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
                        let raw = match &top / &bottom {
                            Some(raw) => raw,
                            None => return Err(format!("Division by zero: {} / {}",
                                                       top.show(self), bottom.show(self)))
                        };
                        let number = raw.show_number_part();
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
                        Ok(format!("{number}{unit_top}{unit_frac} ({reduced})",
                                   number=number, unit_top=unit_top,
                                   unit_frac=unit_frac, reduced=reduced))
                    }
                },
                (Err(e), _, _) => Err(e),
                (_, Err(e), _) => Err(e),
                (_, _, Err(e)) => Err(e),
            },
            _ => {
                let val = try!(self.eval(expr));
                Ok(val.show(self))
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
            datepatterns: Vec::new(),
            short_output: false,
        };

        ctx.prefixes.sort_by(|a, b| a.0.cmp(&b.0));

        for (name, def) in defs.defs {
            match *def {
                Def::Dimension(ref dname) => {
                    let dname = Rc::new(dname.clone());
                    ctx.dimensions.push(dname.clone());
                    let mut map = Unit::new();
                    map.insert(dname, 1);
                    ctx.aliases.insert(map, name.clone());
                },
                Def::Unit(ref expr) => match ctx.eval(expr) {
                    Ok(Value::Number(v)) => {
                        ctx.units.insert(name.clone(), v);
                    },
                    Ok(_) => println!("Unit {} is not a number", name),
                    Err(e) => println!("Unit {} is malformed: {}", name, e)
                },
                Def::Prefix(ref expr) => match ctx.eval(expr) {
                    Ok(Value::Number(v)) => {
                        ctx.prefixes.push((name.clone(), v));
                    },
                    Ok(_) => println!("Prefix {} is not a number", name),
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::SPrefix(ref expr) => match ctx.eval(expr) {
                    Ok(Value::Number(v)) => {
                        ctx.prefixes.push((name.clone(), v.clone()));
                        ctx.units.insert(name.clone(), v);
                    },
                    Ok(_) => println!("Prefix {} is not a number", name),
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::DatePattern(ref pat) => ctx.datepatterns.push(pat.clone()),
                Def::Error(ref err) => println!("Def {}: {}", name, err),
            };
        }

        for (expr, name) in defs.aliases {
            match ctx.eval(&expr) {
                Ok(Value::Number(v)) => {
                    ctx.aliases.insert(v.1, name);
                },
                Ok(_) => println!("Alias {} is not a number", name),
                Err(e) => println!("Alias {}: {}", name, e)
            }
        }

        ctx
    }
}
