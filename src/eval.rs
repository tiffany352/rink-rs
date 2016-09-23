// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::{BTreeMap, BTreeSet};
use gmp::mpq::Mpq;
use number::{Number, Unit, Dim, NumberParts, pow};
use date;
use ast::{DatePattern, Expr, SuffixOp, Def, Defs, Query, Conversion};
use std::rc::Rc;
use factorize::{factorize, Factors};
use value::{Value, Show};
use reply::{DefReply, ConversionReply, FactorizeReply, UnitsForReply,
            QueryReply, ConformanceError, QueryError, UnitListReply,
            DurationReply};

/// The evaluation context that contains unit definitions.
#[derive(Debug)]
pub struct Context {
    pub dimensions: BTreeSet<Dim>,
    pub canonicalizations: BTreeMap<String, String>,
    pub units: BTreeMap<String, Number>,
    pub quantities: BTreeMap<Unit, String>,
    pub reverse: BTreeMap<Unit, String>,
    pub prefixes: Vec<(String, Number)>,
    pub definitions: BTreeMap<String, Expr>,
    pub docs: BTreeMap<String, String>,
    pub datepatterns: Vec<Vec<DatePattern>>,
    pub short_output: bool,
}

impl Context {
    /// Wrapper around show that calls `println!`.
    pub fn print(&self, value: &Number) {
        println!("{}", value.show(self));
    }

    /// Given a unit name, returns its value if it exists. Supports SI
    /// prefixes, plurals, bare dimensions like length, and quantities.
    pub fn lookup(&self, name: &str) -> Option<Number> {
        fn inner(ctx: &Context, name: &str) -> Option<Number> {
            if let Some(k) = ctx.dimensions.get(name) {
                return Some(Number::one_unit(k.to_owned()))
            }
            if let Some(v) = ctx.units.get(name).cloned() {
                return Some(v)
            }
            for (unit, quantity) in &ctx.quantities {
                if name == quantity {
                    return Some(Number(Number::one().0, unit.clone()))
                }
            }
            None
        }
        if let Some(v) = inner(self, name) {
            return Some(v)
        }
        for &(ref pre, ref value) in &self.prefixes {
            if name.starts_with(pre) {
                if let Some(v) = inner(self, &name[pre.len()..]) {
                    return Some((&v * &value).unwrap())
                }
            }
        }
        // after so that "ks" is kiloseconds
        if name.ends_with("s") {
            let name = &name[0..name.len()-1];
            if let Some(v) = inner(self, name) {
                return Some(v)
            }
            for &(ref pre, ref value) in &self.prefixes {
                if name.starts_with(pre) {
                    if let Some(v) = inner(self, &name[pre.len()..]) {
                        return Some((&v * &value).unwrap())
                    }
                }
            }
        }
        None
    }

    /// Given a unit name, try to return a canonical name (expanding aliases and such)
    pub fn canonicalize(&self, name: &str) -> Option<String> {
        fn inner(ctx: &Context, name: &str) -> Option<String> {
            if let Some(v) = ctx.canonicalizations.get(name) {
                return Some(v.clone())
            }
            if let Some(k) = ctx.dimensions.get(name) {
                return Some((*k.0).clone())
            }
            if let Some(v) = ctx.definitions.get(name) {
                if let Expr::Unit(ref name) = *v {
                    if let Some(r) = ctx.canonicalize(&*name) {
                        return Some(r)
                    } else {
                        return Some(name.clone())
                    }
                } else {
                    // we cannot canonicalize it further
                    return Some(name.to_owned())
                }
            }
            None
        }
        if let Some(v) = inner(self, name) {
            return Some(v)
        }
        for &(ref pre, ref val) in &self.prefixes {
            if name.starts_with(pre) {
                if let Some(v) = inner(self, &name[pre.len()..]) {
                    let mut pre = pre;
                    for &(ref other, ref otherval) in &self.prefixes {
                        if other.len() > pre.len() && val == otherval {
                            pre = other;
                        }
                    }
                    return Some(format!("{}{}", pre, v))
                }
            }
        }
        if name.ends_with("s") {
            let name = &name[0..name.len()-1];
            if let Some(v) = inner(self, name) {
                return Some(v)
            }
            for &(ref pre, ref val) in &self.prefixes {
                if name.starts_with(pre) {
                    if let Some(v) = inner(self, &name[pre.len()..]) {
                        let mut pre = pre;
                        for &(ref other, ref otherval) in &self.prefixes {
                            if other.len() > pre.len() && val == otherval {
                                pre = other;
                            }
                        }
                        return Some(format!("{}{}", pre, v))
                    }
                }
            }
        }
        None
    }

    /// Describes a value's unit, gives true if the unit is reciprocal
    /// (e.g. you should prefix "1.0 / " or replace "multiply" with
    /// "divide" when rendering it).
    pub fn describe_unit(&self, value: &Number) -> (bool, String) {
        use std::io::Write;

        let mut buf = vec![];
        let mut recip = false;
        let square = Number(Mpq::one(), value.1.clone()).root(2).ok();
        let inverse = (&Number::one() / &Number(Mpq::one(), value.1.clone())).unwrap();
        if let Some(name) = self.quantities.get(&value.1) {
            write!(buf, "{}", name).unwrap();
        } else if let Some(name) = square.and_then(|square| self.quantities.get(&square.1)) {
            write!(buf, "{}^2", name).unwrap();
        } else if let Some(name) = self.quantities.get(&inverse.1) {
            recip = true;
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
                    if let Some(name) = self.quantities.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
                        map.insert(dim.clone(), 1);
                        if let Some(name) = self.quantities.get(&map) {
                            write!(buf, " {}", name).unwrap();
                        } else {
                            write!(buf, " '{}'", dim).unwrap();
                        }
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
                    if let Some(name) = self.quantities.get(&map) {
                        write!(buf, " {}", name).unwrap();
                    } else {
                        let mut map = Unit::new();
                        map.insert(dim.clone(), 1);
                        if let Some(name) = self.quantities.get(&map) {
                            write!(buf, " {}", name).unwrap();
                        } else {
                            write!(buf, " '{}'", dim).unwrap();
                        }
                        if pow != 1 {
                            write!(buf, "^{}", pow).unwrap();
                        }
                    }
                }
            }
            buf.remove(0);
        }

        (recip, String::from_utf8(buf).unwrap())
    }

    fn typo_dym<'a>(&'a self, what: &str) -> Option<&'a str> {
        use strsim::jaro_winkler;

        let mut best = None;
        {
            let mut try = |x: &'a str| {
                let score = jaro_winkler(
                    &*x.to_lowercase(), &*what.to_lowercase());
                let better = best
                    .as_ref()
                    .map(|&(s, _v)| score > s)
                    .unwrap_or(true);
                if better {
                    best = Some((score, x));
                }
            };

            for k in &self.dimensions {
                try(&**k.0);
            }
            for (k, _v) in &self.units {
                try(&**k);
            }
            for (_u, k) in &self.quantities {
                try(&**k);
            }
        }
        best.and_then(|(s, v)| if s > 0.8 { Some(v) } else { None })
    }

    fn unknown_unit_err(&self, name: &str) -> String {
        match self.typo_dym(name) {
            Some(x) => format!("Unknown unit {}, did you mean {}?", name, x),
            None => format!("Unknown unit {}", name)
        }
    }

    /// Evaluates an expression to compute its value, *excluding* `->`
    /// conversions.
    pub fn eval(&self, expr: &Expr) -> Result<Value, String> {
        use std::ops::*;
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

        macro_rules! temperature {
            ($left:ident, $name:expr, $base:expr, $scale:expr) => {{
                let left = try!(self.eval(&**$left));
                let left = match left {
                    Value::Number(left) => left,
                    _ => return Err(format!("Expected number, got: <{}> °{}",
                                            left.show(self), stringify!($name)))
                };
                if left.1 != BTreeMap::new() {
                    Err(format!("Expected dimensionless, got: <{}>", left.show(self)))
                } else {
                    let left = (&left * &self.lookup($scale).expect(&*format!("Missing {} unit", $scale))).unwrap();
                    Ok(Value::Number((&left + &self.lookup($base)
                                      .expect(&*format!("Missing {} constant", $base))).unwrap()))
                }
            }}
        }

        match *expr {
            Expr::Unit(ref name) if name == "now" => Ok(Value::DateTime(date::now())),
            Expr::Unit(ref name) =>
                self.lookup(name).ok_or_else(|| self.unknown_unit_err(name)).map(Value::Number),
            Expr::Quote(ref name) => Ok(Value::Number(Number::one_unit(Dim::new(&**name)))),
            Expr::Const(ref num) =>
                Ok(Value::Number(Number::new(num.clone()))),
            Expr::Date(ref date) => date::try_decode(date, self).map(Value::DateTime),
            Expr::Neg(ref expr) => self.eval(&**expr).and_then(|v| (-&v).map_err(|e| {
                format!("{}: - <{}>", e, v.show(self))
            })),
            Expr::Plus(ref expr) => self.eval(&**expr),

            Expr::Frac(ref left, ref right) => operator!(left div / right),
            Expr::Add(ref left, ref right)  => operator!(left add + right),
            Expr::Sub(ref left, ref right)  => operator!(left sub - right),
            Expr::Pow(ref left, ref right)  => operator!(left pow ^ right),

            Expr::Suffix(SuffixOp::Celsius, ref left) =>
                temperature!(left, "C", "zerocelsius", "kelvin"),
            Expr::Suffix(SuffixOp::Fahrenheit, ref left) =>
                temperature!(left, "F", "zerofahrenheit", "degrankine"),
            Expr::Suffix(SuffixOp::Reaumur, ref left) =>
                temperature!(left, "Ré", "zerocelsius", "reaumur_absolute"),
            Expr::Suffix(SuffixOp::Romer, ref left) =>
                temperature!(left, "Rø", "zeroromer", "romer_absolute"),
            Expr::Suffix(SuffixOp::Delisle, ref left) =>
                temperature!(left, "De", "zerodelisle", "delisle_absolute"),
            Expr::Suffix(SuffixOp::Newton, ref left) =>
                temperature!(left, "N", "zerocelsius", "newton_absolute"),

            // TODO: A type might not implement * on Number, and this would fail
            Expr::Mul(ref args) => args.iter().fold(Ok(Value::Number(Number::one())), |a, b| {
                a.and_then(|a| {
                    let b = try!(self.eval(b));
                    Ok((&a * &b).unwrap())
                })
            }),
            Expr::Equals(_, ref right) => self.eval(right),
            Expr::Call(ref name, ref args) => {
                let args = try!(args.iter().map(|x| self.eval(x)).collect::<Result<Vec<_>, _>>());
                match &**name {
                    "sqrt" => {
                        if args.len() != 1 {
                            return Err(format!("Argument number mismatch for sqrt: expected 1, got {}", args.len()))
                        }
                        match args[0] {
                            Value::Number(ref num) =>
                                num.root(2).map(Value::Number).map_err(|e| format!(
                                    "{}: sqrt <{}>", e, num.show(self))),
                            ref x => Err(format!("Expected number, got <{}>", x.show(self)))
                        }
                    },
                    _ => Err(format!("Function not found: {}", name))
                }
            },
            Expr::Error(ref e) => Err(e.clone()),
        }
    }

    pub fn eval_unit_name(&self, expr: &Expr) -> Result<(BTreeMap<String, isize>, Mpq), String> {
        match *expr {
            Expr::Equals(ref left, ref _right) => match **left {
                Expr::Unit(ref name) => {
                    let mut map = BTreeMap::new();
                    map.insert(name.clone(), 1);
                    Ok((map, Mpq::one()))
                },
                ref x => Err(format!("Expected identifier, got {:?}", x))
            },
            Expr::Call(_, _) => Err(format!("Calls are not allowed in the right hand side of conversions")),
            Expr::Unit(ref name) | Expr::Quote(ref name) => {
                let mut map = BTreeMap::new();
                map.insert(self.canonicalize(&**name).unwrap_or_else(|| name.clone()), 1);
                Ok((map, Mpq::one()))
            },
            Expr::Const(ref i) =>
                Ok((BTreeMap::new(), i.clone())),
            Expr::Frac(ref left, ref right) => {
                let (left, lv) = try!(self.eval_unit_name(left));
                let (right, rv) = try!(self.eval_unit_name(right));
                let right = right.into_iter()
                    .map(|(k,v)| (k, -v)).collect::<BTreeMap<_, _>>();
                Ok((::btree_merge(&left, &right, |a,b| if a+b != 0 { Some(a + b) } else { None }),
                    &lv / &rv))
            },
            Expr::Mul(ref args) => {
                args[1..].iter().fold(self.eval_unit_name(&args[0]), |acc, b| {
                    let (acc, av) = try!(acc);
                    let (b, bv) = try!(self.eval_unit_name(b));
                    Ok((::btree_merge(&acc, &b, |a,b| if a+b != 0 { Some(a+b) } else { None }),
                        &av * &bv))
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
                let (left, lv) = try!(self.eval_unit_name(left));
                Ok((left.into_iter()
                   .filter_map(|(k, v)| {
                       let v = v * res as isize;
                       if v != 0 {
                           Some((k, v))
                       } else {
                           None
                       }
                   })
                    .collect::<BTreeMap<_, _>>(),
                    pow(&lv, res as i32)))
            },
            Expr::Add(ref left, ref right) | Expr::Sub(ref left, ref right) => {
                let left = try!(self.eval_unit_name(left));
                let right = try!(self.eval_unit_name(right));
                if left != right {
                    return Err(format!("Add of values with differing dimensions is not meaningful"))
                }
                Ok(left)
            },
            Expr::Neg(ref v) => self.eval_unit_name(v).map(|(u, v)| (u, -&v)),
            Expr::Plus(ref v) => self.eval_unit_name(v),
            Expr::Suffix(_, _) =>
                Err(format!("Temperature conversions must not be compound units")),
            Expr::Date(_) => Err(format!("Dates are not allowed in the right hand side of conversions")),
            Expr::Error(ref e) => Err(e.clone()),
        }
    }

    /// Evaluates an expression, include `->` conversions.
    pub fn eval_outer(&self, expr: &Query) -> Result<QueryReply, QueryError> {
        let conformance_err = |top: &Number, bottom: &Number| -> ConformanceError {
            let mut topu = top.clone();
            topu.0 = Mpq::one();
            let mut bottomu = bottom.clone();
            bottomu.0 = Mpq::one();
            let mut suggestions = vec![];
            let diff = (&topu * &bottomu).unwrap();
            if diff.1.len() == 0 {
                suggestions.push(format!("Reciprocal conversion, invert one side"));
            } else {
                let diff = (&topu / &bottomu).unwrap();
                let (recip, desc) = self.describe_unit(&diff.invert());
                let word = match recip {
                    false => "multiply",
                    true => "divide"
                };
                suggestions.push(format!("{word} left side by {}", desc.trim(), word=word));
                let (recip, desc) = self.describe_unit(&diff);
                let word = match recip {
                    false => "multiply",
                    true => "divide"
                };
                suggestions.push(format!("{word} right side by {}", desc.trim(), word=word));
            }

            ConformanceError {
                left: top.to_parts(self),
                right: bottom.to_parts(self),
                suggestions: suggestions,
            }
        };

        let show = |raw: &Number, bottom: &Number, bottom_name: (BTreeMap<String, isize>, Mpq), base: u8| -> ConversionReply {
            use gmp::mpz::Mpz;
            let (bottom_name, bottom_const) = bottom_name;
            let (exact, approx) = raw.numeric_value(base);
            let bottom_name = bottom_name.into_iter().map(
                |(a,b)| (Dim::new(&*a), b as i64)).collect();
            ConversionReply {
                value: NumberParts {
                    exact_value: exact,
                    approx_value: approx,
                    factor: if bottom_const.get_num() != Mpz::one() {
                        Some(format!("{}", bottom_const.get_num()))
                    } else {
                        None
                    },
                    divfactor: if bottom_const.get_den() != Mpz::one() {
                        Some(format!("{}", bottom_const.get_den()))
                    } else {
                        None
                    },
                    unit: Some(Number::unit_to_string(&bottom_name)),
                    raw_unit: Some(bottom_name),
                    ..bottom.to_parts(self)
                },
            }
        };

        let to_list = |top: &Number, list: &[&str]| -> Result<Vec<NumberParts>, QueryError> {
            let units = try!(list.iter().map(|x| {
                self.lookup(x).ok_or_else(|| self.unknown_unit_err(x))
            }).collect::<Result<Vec<Number>, _>>());
            {
                let first = try!(units.first().ok_or(format!("Expected non-empty unit list")));
                try!(units.iter().skip(1).map(|x| {
                    if first.1 != x.1 {
                        Err(format!("Units in unit list must conform: <{}> ; <{}>",
                                    first.show(self), x.show(self)))
                    } else {
                        Ok(())
                    }
                }).collect::<Result<Vec<()>, _>>());
                if top.1 != first.1 {
                    return Err(QueryError::Conformance(conformance_err(&top, &first)))
                }
            }
            let mut value = top.0.clone();
            let mut out = vec![];
            let len = units.len();
            for (i, unit) in units.into_iter().enumerate() {
                // value -= unit * floor(value/unit)
                use gmp::mpz::Mpz;

                let res = &value / &unit.0;
                let div = &res.get_num() / res.get_den();
                let rem = &value - &(&unit.0 * &Mpq::ratio(&div, &Mpz::one()));
                value = rem;
                if i == len-1 {
                    out.push(res);
                } else {
                    out.push(Mpq::ratio(&div, &Mpz::one()));
                }
            }
            Ok(list.into_iter().zip(out.into_iter()).map(|(name, value)| {
                let pretty = Number(value, Number::one_unit(Dim::new(name)).1).to_parts(self);
                NumberParts {
                    unit: Some(pretty.unit.or(pretty.dimensions)
                               .map(|x| self.canonicalize(&*x).unwrap_or(x))
                               .expect("to_parts returned no dimensions")),
                    exact_value: Some(pretty.approx_value.or(pretty.exact_value)
                                      .expect("to_parts returned neither exact nor approx value")),
                    ..Default::default()
                }
                /*NumberParts {
                    unit: Some(self.canonicalize(name).unwrap_or_else(|| (*name).to_owned())),
                    exact_value: Some(::number::to_string(&value).1),
                    ..Default::default()
                }*/
            }).collect())
        };

        match *expr {
            Query::Expr(Expr::Unit(ref name)) if {
                let a = self.definitions.contains_key(name);
                let b = self.canonicalize(name)
                    .map(|x| self.definitions.contains_key(&*x))
                    .unwrap_or(false);
                let c = self.dimensions.contains(&**name);
                let d = self.canonicalize(name)
                    .map(|x| self.dimensions.contains(&*x))
                    .unwrap_or(false);
                a || b || c || d
            } => {
                let mut name = name.clone();
                let mut canon = self.canonicalize(&name).unwrap_or_else(|| name.clone());
                while let Some(&Expr::Unit(ref unit)) = {
                    self.definitions.get(&name).or_else(|| self.definitions.get(&*canon))
                } {
                    let unit_canon = self.canonicalize(unit).unwrap_or_else(|| unit.clone());
                    if self.definitions.get(unit).is_none() {
                        if self.definitions.get(&unit_canon).is_none() {
                            if !self.dimensions.contains(&**unit) {
                                break
                            } else {
                                name = unit.clone();
                                canon = unit_canon;
                                break;
                            }
                        } else {
                            name = unit_canon.clone();
                            canon = unit_canon;
                        }
                    } else {
                        name = unit.clone();
                        canon = unit_canon;
                    }
                }
                let (def, res) = if self.dimensions.contains(&*name) {
                    let parts = self.lookup(&name)
                        .expect("Lookup of base unit failed")
                        .to_parts(self);
                    let def = if let Some(ref q) = parts.quantity {
                        format!("base unit of {}", q)
                    } else {
                        format!("base unit")
                    };
                    (Some(def), None)
                } else {
                    (self.definitions.get(&name).map(|x| format!("{}", x)),
                     self.lookup(&name).map(|x| x.to_parts(self)))
                };
                Ok(QueryReply::Def(DefReply {
                    canon_name: canon,
                    def: def,
                    value: res,
                    doc: self.docs.get(&name).cloned(),
                }))
            },
            Query::Convert(ref top, Conversion::None, Some(base)) => {
                let top = try!(self.eval(top));
                let top = match top {
                    Value::Number(top) => top,
                    _ => return Err(QueryError::Generic(format!(
                        "<{}> in base {} is not defined", top.show(self), base)))
                };
                let (exact, approx) = top.numeric_value(base);
                let parts = NumberParts {
                    exact_value: exact,
                    approx_value: approx,
                    .. top.to_parts(self)
                };
                Ok(QueryReply::Conversion(ConversionReply {
                    value: parts
                }))
            },
            Query::Convert(ref top, Conversion::Expr(ref bottom), base) => match (self.eval(top), self.eval(bottom), self.eval_unit_name(bottom)) {
                (Ok(top), Ok(bottom), Ok(bottom_name)) => {
                    let (top, bottom) = match (top, bottom) {
                        (Value::Number(top), Value::Number(bottom)) =>
                            (top, bottom),
                        _ => return Err(QueryError::Generic(format!(
                            "Conversion of non-numbers is not defined")))
                    };
                    if top.1 == bottom.1 {
                        let raw = match &top / &bottom {
                            Some(raw) => raw,
                            None => return Err(QueryError::Generic(format!(
                                "Division by zero: {} / {}",
                                top.show(self), bottom.show(self))))
                        };
                        Ok(QueryReply::Conversion(show(
                            &raw, &bottom, bottom_name, base.unwrap_or(10))))
                    } else {
                        Err(QueryError::Conformance(conformance_err(
                            &top, &bottom)))
                    }
                },
                (Err(e), _, _) => Err(QueryError::Generic(e)),
                (_, Err(e), _) => Err(QueryError::Generic(e)),
                (_, _, Err(e)) => Err(QueryError::Generic(e)),
            },
            Query::Convert(ref top, Conversion::List(ref list), None) => {
                let top = try!(self.eval(top));
                let top = match top {
                    Value::Number(num) => num,
                    _ => return Err(QueryError::Generic(format!(
                        "Cannot convert <{}> to {:?}", top.show(self), list)))
                };
                to_list(&top, &list.iter().map(|x| &**x).collect::<Vec<_>>()[..]).map(|list| {
                    QueryReply::UnitList(UnitListReply {
                        rest: NumberParts {
                            quantity: self.quantities.get(&top.1).cloned(),
                            ..Default::default()
                        },
                        list: list,
                    })
                })
            },
            Query::Convert(ref top, Conversion::Offset(off), None) => {
                use chrono::FixedOffset;

                let top = try!(self.eval(top));
                let top = match top {
                    Value::DateTime(date) => date,
                    _ => return Err(QueryError::Generic(format!(
                        "Cannot convert <{}> to timezone offset {:+}", top.show(self), off)))
                };
                let top = top.with_timezone(&FixedOffset::east(off as i32));
                Ok(QueryReply::Date(top))
            },
            Query::Convert(ref top, ref which @ Conversion::DegC, None) |
            Query::Convert(ref top, ref which @ Conversion::DegF, None) |
            Query::Convert(ref top, ref which @ Conversion::DegN, None) |
            Query::Convert(ref top, ref which @ Conversion::DegRe, None) |
            Query::Convert(ref top, ref which @ Conversion::DegRo, None) |
            Query::Convert(ref top, ref which @ Conversion::DegDe, None) => {
                let top = try!(self.eval(top));
                macro_rules! temperature {
                    ($name:expr, $base:expr, $scale:expr) => {{
                        let top = match top {
                            Value::Number(ref num) => num,
                            _ => return Err(QueryError::Generic(format!(
                                "Cannot convert <{}> to °{}", top.show(self), $name)))
                        };
                        let bottom = self.lookup($scale)
                            .expect(&*format!("Unit {} missing", $scale));
                        if top.1 != bottom.1 {
                            Err(QueryError::Conformance(conformance_err(&top, &bottom)))
                        } else {
                            let res = (top - &self.lookup($base)
                                       .expect(&*format!("Constant {} missing", $base))).unwrap();
                            let res = (&res / &bottom).unwrap();
                            let mut name = BTreeMap::new();
                            name.insert(format!("°{}", $name), 1);
                            Ok(QueryReply::Conversion(show(&res, &bottom, (name, Mpq::one()), 10)))
                        }
                    }}
                }

                match *which {
                    Conversion::DegC => temperature!("C", "zerocelsius", "kelvin"),
                    Conversion::DegF => temperature!("F", "zerofahrenheit", "degrankine"),
                    Conversion::DegRe => temperature!("Ré", "zerocelsius", "reaumur_absolute"),
                    Conversion::DegRo => temperature!("Rø", "zeroromer", "romer_absolute"),
                    Conversion::DegDe => temperature!("De", "zerodelisle", "delisle_absolute"),
                    Conversion::DegN => temperature!("N", "zerocelsius", "newton_absolute"),
                    _ => panic!()
                }
            },
            Query::Convert(ref _expr, ref which, Some(base)) => {
                Err(QueryError::Generic(format!(
                    "Conversion to {} is not defined in base {}",
                    which, base)))
            },
            Query::Factorize(ref expr) => {
                let val = try!(self.eval(expr));
                let val = match val {
                    Value::Number(val) => val,
                    _ => return Err(QueryError::Generic(format!(
                        "Cannot find derivatives of <{}>", val.show(self))),)
                };
                let quantities = self.quantities.iter()
                    .map(|(a, b)| (a.clone(), Rc::new(b.clone())))
                    .collect::<BTreeMap<_, _>>();
                let results = factorize(&val, &quantities);
                let mut results = results.into_sorted_vec();
                results.dedup();
                let results = results.into_iter().map(|Factors(_score, names)| {
                    let mut next = BTreeMap::<Rc<String>, usize>::new();
                    for name in names.into_iter() {
                        *next.entry(name).or_insert(0) += 1;
                    }
                    next
                }).collect::<Vec<_>>();
                Ok(QueryReply::Factorize(FactorizeReply {
                    factorizations: results
                }))
            },
            Query::UnitsFor(ref expr) => {
                let val = try!(self.eval(expr));
                let val = match val {
                    Value::Number(val) => val,
                    _ => return Err(QueryError::Generic(format!(
                        "Cannot find units for <{}>", val.show(self)))),
                };
                let mut out = vec![];
                for (name, unit) in self.units.iter() {
                    if let Some(&Expr::Unit(_)) = self.definitions.get(name) {
                        continue
                    }
                    if val.1 == unit.1 {
                        out.push(name);
                    }
                }
                out.sort();
                let parts = val.to_parts(self);
                Ok(QueryReply::UnitsFor(UnitsForReply {
                    units: out.into_iter().cloned().collect(),
                    of: NumberParts {
                        dimensions: parts.dimensions,
                        quantity: parts.quantity,
                        ..Default::default()
                    },
                }))
            },
            Query::Expr(ref expr) |
            Query::Convert(ref expr, Conversion::None, None) => {
                let val = try!(self.eval(expr));
                match val {
                    Value::Number(ref n) if n.1 == Number::one_unit(Dim::new("s")).1 => {
                        let units = &["year", "week", "day", "hour", "minute", "second"];
                        let list = try!(to_list(&n, units));
                        let mut list = list.into_iter();
                        Ok(QueryReply::Duration(DurationReply {
                            raw: n.to_parts(self),
                            years: list.next().expect("Unexpected end of iterator"),
                            //months: list.next().expect("Unexpected end of iterator"),
                            months: NumberParts {
                                exact_value: Some("0".to_owned()),
                                unit: Some("month".to_owned()),
                                ..Default::default()
                            },
                            weeks: list.next().expect("Unexpected end of iterator"),
                            days: list.next().expect("Unexpected end of iterator"),
                            hours: list.next().expect("Unexpected end of iterator"),
                            minutes: list.next().expect("Unexpected end of iterator"),
                            seconds: list.next().expect("Unexpected end of iterator"),
                        }))
                    },
                    Value::Number(n) => Ok(QueryReply::Number(n.to_parts(self))),
                    Value::DateTime(d) => Ok(QueryReply::Date(d)),
                }
            },
            Query::Error(ref e) => Err(QueryError::Generic(e.clone())),
        }
    }

    /// Creates a new, empty context
    pub fn new() -> Context {
        Context {
            dimensions: BTreeSet::new(),
            canonicalizations: BTreeMap::new(),
            units: BTreeMap::new(),
            quantities: BTreeMap::new(),
            reverse: BTreeMap::new(),
            prefixes: Vec::new(),
            definitions: BTreeMap::new(),
            docs: BTreeMap::new(),
            datepatterns: Vec::new(),
            short_output: false,
        }
    }

    pub fn load_dates(&mut self, mut dates: Vec<Vec<DatePattern>>) {
        self.datepatterns.append(&mut dates)
    }

    /// Takes a parsed definitions.units from
    /// `gnu_units::parse()`. Prints if there are errors in the file.
    pub fn load(&mut self, defs: Defs) {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone)]
        enum Name {
            Unit(Rc<String>),
            Prefix(Rc<String>),
            Quantity(Rc<String>),
        }

        struct Resolver {
            interned: BTreeSet<Rc<String>>,
            input: BTreeMap<Name, Rc<Def>>,
            sorted: Vec<Name>,
            unmarked: BTreeSet<Name>,
            temp_marks: BTreeSet<Name>,
            docs: BTreeMap<Name, String>,
        }

        impl Resolver {
            fn intern(&mut self, name: &String) -> Rc<String> {
                if let Some(v) = self.interned.get(name).cloned() {
                    v
                } else {
                    let v = Rc::new(name.to_owned());
                    self.interned.insert(v.clone());
                    v
                }
            }

            fn lookup(&mut self, name: &Rc<String>) -> Option<()> {
                fn inner(ctx: &mut Resolver, name: &Rc<String>) -> Option<()> {
                    let unit = Name::Unit(name.clone());
                    if ctx.input.get(&unit).is_some() {
                        ctx.visit(&unit);
                        return Some(())
                    }
                    let unit = Name::Prefix(name.clone());
                    if ctx.input.get(&unit).is_some() {
                        ctx.visit(&unit);
                        return Some(())
                    }
                    let unit = Name::Quantity(name.clone());
                    if ctx.input.get(&unit).is_some() {
                        ctx.visit(&unit);
                        return Some(())
                    }
                    None
                }

                if let Some(()) = inner(self, name) {
                    return Some(())
                }
                let mut found = vec![];
                for (pre, _) in &self.input {
                    if let &Name::Prefix(ref pre) = pre {
                        if (*name).starts_with(&**pre) {
                            found.push(pre.clone());
                        }
                    }
                }
                for pre in found {
                    if let Some(()) = inner(self, &Rc::new(name[pre.len()..].to_owned())) {
                        let unit = Name::Prefix(pre);
                        self.visit(&unit);
                        return Some(())
                    }
                }
                if name.ends_with("s") {
                    let name = &Rc::new(name[0..name.len()-1].to_owned());
                    if let Some(()) = inner(self, name) {
                        return Some(())
                    }
                    let mut found = vec![];
                    for (pre, _) in &self.input {
                        if let &Name::Prefix(ref pre) = pre {
                            if (*name).starts_with(&**pre) {
                                found.push(pre.clone());
                            }
                        }
                    }
                    for pre in found {
                        if let Some(()) = inner(self, &Rc::new(name[pre.len()..].to_owned())) {
                            let unit = Name::Prefix(pre);
                            self.visit(&unit);
                            return Some(())
                        }
                    }
                }
                None
            }

            fn eval(&mut self, expr: &Expr) {
                match *expr {
                    Expr::Unit(ref name) => {
                        let name = self.intern(name);
                        if self.lookup(&name).is_none() {
                            println!("Lookup failed: {}", name);
                        }
                    },
                    Expr::Frac(ref left, ref right) |
                    Expr::Pow(ref left, ref right) |
                    Expr::Add(ref left, ref right) |
                    Expr::Sub(ref left, ref right) => {
                        self.eval(left);
                        self.eval(right);
                    },
                    Expr::Neg(ref expr) | Expr::Plus(ref expr) | Expr::Suffix(_, ref expr) =>
                        self.eval(expr),
                    Expr::Mul(ref exprs) | Expr::Call(_, ref exprs) => for expr in exprs {
                        self.eval(expr);
                    },
                    _ => ()
                }
            }

            fn visit(&mut self, name: &Name) {
                if self.temp_marks.get(name).is_some() {
                    println!("Unit {:?} has a dependency cycle", name);
                    return;
                }
                if self.unmarked.get(name).is_some() {
                    self.temp_marks.insert(name.clone());
                    if let Some(v) = self.input.get(name).cloned() {
                        match *v {
                            Def::Prefix(ref e) | Def::SPrefix(ref e) | Def::Unit(ref e) |
                            Def::Quantity(ref e) =>
                                self.eval(e),
                            Def::Canonicalization(ref e) => {
                                self.lookup(&Rc::new(e.clone()));
                            },
                            _ => (),
                        }
                    }
                    self.unmarked.remove(name);
                    self.temp_marks.remove(name);
                    self.sorted.push(name.clone());
                }
            }
        }

        let mut resolver = Resolver {
            interned: BTreeSet::new(),
            input: BTreeMap::new(),
            sorted: vec![],
            unmarked: BTreeSet::new(),
            temp_marks: BTreeSet::new(),
            docs: BTreeMap::new(),
        };
        for (name, def, doc) in defs.defs.into_iter() {
            let name = resolver.intern(&name);
            let unit = match *def {
                Def::Prefix(_) | Def::SPrefix(_) => Name::Prefix(name),
                Def::Quantity(_) => Name::Quantity(name),
                _ => Name::Unit(name)
            };
            if let Some(doc) = doc {
                resolver.docs.insert(unit.clone(), doc);
            }
            if resolver.input.insert(unit.clone(), def).is_some() {
                let (ty, name) = match unit {
                    Name::Prefix(ref name) => ("prefixes", name),
                    Name::Quantity(ref name) => ("quantities", name),
                    Name::Unit(ref name) => ("units", name),
                };
                println!("warning: multiple {} named {}", ty, name);
            }
            resolver.unmarked.insert(unit);
        }

        while let Some(name) = resolver.unmarked.iter().next().cloned() {
            resolver.visit(&name)
        }
        let sorted = resolver.sorted;
        //println!("{:#?}", sorted);
        let mut input = resolver.input;
        let udefs = sorted.into_iter().map(move |name| {
            let res = input.remove(&name).unwrap();
            (name, res)
        });

        let mut reverse = BTreeSet::new();
        reverse.insert("newton");
        reverse.insert("pascal");
        reverse.insert("joule");
        reverse.insert("watt");
        reverse.insert("coulomb");
        reverse.insert("volt");
        reverse.insert("ohm");
        reverse.insert("siemens");
        reverse.insert("farad");
        reverse.insert("weber");
        reverse.insert("henry");
        reverse.insert("tesla");
        reverse.insert("lumen");
        reverse.insert("lux");
        reverse.insert("gray");
        reverse.insert("katal");

        for (name, def) in udefs {
            let name = match name {
                Name::Unit(name) => (*name).clone(),
                Name::Prefix(name) => (*name).clone(),
                Name::Quantity(name) => (*name).clone(),
            };
            match *def {
                Def::Dimension => {
                    self.dimensions.insert(Dim::new(&*name));
                },
                Def::Canonicalization(ref of) => {
                    self.canonicalizations.insert(of.clone(), name.clone());
                    match self.lookup(of) {
                        Some(v) => {
                            self.units.insert(name.clone(), v);
                        },
                        None => println!("Canonicalization {} is malformed: {} not found", name, of)
                    }
                },
                Def::Unit(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        if v.0 == Mpq::one() && reverse.contains(&*name) {
                            self.reverse.insert(v.1.clone(), name.clone());
                        }
                        self.definitions.insert(name.clone(), expr.clone());
                        self.units.insert(name.clone(), v);
                    },
                    Ok(_) => println!("Unit {} is not a number", name),
                    Err(e) => println!("Unit {} is malformed: {}", name, e)
                },
                Def::Prefix(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        self.prefixes.push((name.clone(), v));
                    },
                    Ok(_) => println!("Prefix {} is not a number", name),
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::SPrefix(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        self.prefixes.push((name.clone(), v.clone()));
                        self.units.insert(name.clone(), v);
                    },
                    Ok(_) => println!("Prefix {} is not a number", name),
                    Err(e) => println!("Prefix {} is malformed: {}", name, e)
                },
                Def::Quantity(ref expr) => match self.eval(expr) {
                    Ok(Value::Number(v)) => {
                        let res = self.quantities.insert(v.1, name.clone());
                        if !self.definitions.contains_key(&name) {
                            self.definitions.insert(name.clone(), expr.clone());
                        }
                        if let Some(old) = res {
                            println!("Warning: Conflicting quantities {} and {}", name, old);
                        }
                    },
                    Ok(_) => println!("Quantity {} is not a number", name),
                    Err(e) => println!("Quantity {} is malformed: {}", name, e)
                },
                Def::Error(ref err) => println!("Def {}: {}", name, err),
            };
        }

        for (name, val) in resolver.docs {
            let name = match name {
                Name::Unit(name) => (*name).clone(),
                Name::Prefix(name) => (*name).clone(),
                Name::Quantity(name) => (*name).clone(),
            };
            if self.docs.insert(name.clone(), val).is_some() {
                println!("Doc conflict for {}", name);
            }
        }
    }
}
