// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;
use number::{Number, Dim, NumberParts, pow};
use num::{Num, Int};
use date;
use ast::{Expr, SuffixOp, Query, Conversion};
use std::rc::Rc;
use factorize::{factorize, Factors};
use value::{Value, Show};
use reply::{
    DefReply, ConversionReply, FactorizeReply, UnitsForReply,
    QueryReply, ConformanceError, QueryError, UnitListReply,
    DurationReply, SearchReply, DateReply, ExprReply
};
use search;
use context::Context;
use substance::SubstanceGetError;

impl Context {
    /// Evaluates an expression to compute its value, *excluding* `->`
    /// conversions.
    pub fn eval(&self, expr: &Expr) -> Result<Value, QueryError> {
        use std::ops::*;
        macro_rules! operator {
            ($left:ident $op:ident $opname:tt $right:ident) => {{
                let left = try!(self.eval(&**$left));
                let right = try!(self.eval(&**$right));
                ((&left).$op(&right)).map_err(|e| {
                    QueryError::Generic(format!(
                        "{}: <{}> {} <{}>",
                        e, left.show(self),
                        stringify!($opname),
                        right.show(self)
                    ))
                })
            }}
        }

        macro_rules! temperature {
            ($left:ident, $name:expr, $base:expr, $scale:expr) => {{
                let left = try!(self.eval(&**$left));
                let left = match left {
                    Value::Number(left) => left,
                    _ => return Err(QueryError::Generic(format!(
                        "Expected number, got: <{}> °{}",
                        left.show(self), stringify!($name)
                    )))
                };
                if left.unit != BTreeMap::new() {
                    Err(QueryError::Generic(format!(
                        "Expected dimensionless, got: <{}>",
                        left.show(self)
                    )))
                } else {
                    let left = (&left * &self.lookup($scale).expect(
                        &*format!("Missing {} unit", $scale))).unwrap();
                    Ok(Value::Number((&left + &self.lookup($base)
                                      .expect(&*format!("Missing {} constant", $base))).unwrap()))
                }
            }}
        }

        match *expr {
            Expr::Unit(ref name) if name == "now" => Ok(Value::DateTime(date::now())),
            Expr::Unit(ref name) =>
                self.lookup(name).map(Value::Number)
                .or_else(|| self.substances.get(name)
                         .cloned().map(Value::Substance)
                )
                .ok_or_else(|| QueryError::NotFound(
                    self.unknown_unit_err(name)
                )),
            Expr::Quote(ref name) => Ok(Value::Number(Number::one_unit(Dim::new(&**name)))),
            Expr::Const(ref num) =>
                Ok(Value::Number(Number::new(num.clone()))),
            Expr::Date(ref date) => date::try_decode(date, self)
                .map(Value::DateTime)
                .map_err(QueryError::Generic),
            Expr::Neg(ref expr) => self.eval(&**expr).and_then(|v| (-&v).map_err(|e| {
                QueryError::Generic(format!("{}: - <{}>", e, v.show(self)))
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

            Expr::Mul(ref args) => args.iter().fold(Ok(Value::Number(Number::one())), |a, b| {
                a.and_then(|a| {
                    let b = try!(self.eval(b));
                    (&a * &b).map_err(|e| QueryError::Generic(format!(
                        "{}: <{}> * <{}>",
                        e, a.show(self), b.show(self)
                    )))
                })
            }),
            Expr::Equals(ref left, ref right) => {
                match **left {
                    Expr::Unit(_) => (),
                    ref x => return Err(QueryError::Generic(format!(
                        "= is currently only used for inline unit definitions: \
                         expected unit, got {}", x
                    )))
                };
                self.eval(right)
            },
            Expr::Of(ref field, ref val) => {
                let val = try!(self.eval(val));
                let val = match val {
                    Value::Substance(sub) => sub,
                    x => return Err(QueryError::Generic(format!(
                        "Not defined: {} of <{}>",
                        field, x.show(self)
                    )))
                };
                val.get(&**field).map(Value::Number).map_err(|e| {
                    match e {
                        SubstanceGetError::Generic(s) =>
                            QueryError::Generic(s),
                        SubstanceGetError::Conformance(l, r) =>
                            QueryError::Conformance(
                                self.conformance_err(&l, &r)
                            ),
                    }
                })
            },
            Expr::Call(ref name, ref args) => {
                let args = try!(
                    args.iter()
                        .map(|x| self.eval(x))
                        .collect::<Result<Vec<_>, _>>());

                macro_rules! func {
                    (fn $fname:ident($($name:ident : $ty:ident),*) $block:block) => {{
                        let mut iter = args.iter();
                        let mut count = 0;
                        $( count += 1; let _ = stringify!($name); )*;
                        $(
                            let $name = match iter.next() {
                                Some(&Value::$ty(ref v)) => v,
                                Some(x) => return Err(QueryError::Generic(
                                    format!(
                                        "Expected {}, got <{}>",
                                        stringify!($ty), x.show(self)
                                    )
                                )),
                                None => return Err(QueryError::Generic(format!(
                                    "Argument number mismatch for {}: \
                                     Expected {}, got {}",
                                    stringify!($fname), count, args.len()
                                )))
                            };
                        )*;
                        if iter.next().is_some() {
                            return Err(QueryError::Generic(format!(
                                "Argument number mismatch for {}: \
                                 Expected {}, got {}",
                                stringify!($fname), count, args.len()
                            )));
                        }
                        let res: Result<Value, String> = {
                            $block
                        };
                        res.map_err(|e| {
                            QueryError::Generic(format!(
                                "{}: {}({})",
                                e, stringify!($fname),
                                args.iter()
                                    .map(|x| x.show(self))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ))
                        })
                    }
                }}

                match &**name {
                    "sqrt" => func!(fn sqrt(num: Number) {
                        num.root(2).map(Value::Number)
                    }),
                    "exp" => func!(fn exp(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().exp()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "ln" => func!(fn ln(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().ln()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "log" => func!(fn log(num: Number, base: Number) {
                        if base.unit.len() > 0 {
                            Err(format!(
                                "Base must be dimensionless"
                            ))
                        } else {
                            Ok(Value::Number(Number {
                                value: Num::Float(num.value.to_f64()
                                                  .log(base.value.to_f64())),
                                unit: num.unit.clone(),
                            }))
                        }
                    }),
                    "log2" => func!(fn log2(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().log2()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "log10" => func!(fn ln(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().log10()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "hypot" => func!(fn hypot(x: Number, y: Number) {
                        if x.unit != y.unit {
                            Err(format!(
                                "Arguments to hypot must have matching \
                                 dimensionality"
                            ))
                        } else {
                            Ok(Value::Number(Number {
                                value: Num::Float(x.value.to_f64().hypot(y.value.to_f64())),
                                unit: x.unit.clone(),
                            }))
                        }
                    }),
                    "sin" => func!(fn sin(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().sin()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "cos" => func!(fn cos(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().cos()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "tan" => func!(fn tan(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().tan()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "asin" => func!(fn asin(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().asin()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "acos" => func!(fn acos(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().acos()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "atan" => func!(fn atan(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().atan()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "atan2" => func!(fn atan2(x: Number, y: Number) {
                        if x.unit != y.unit {
                            Err(format!(
                                "Arguments to atan2 must have matching \
                                 dimensionality"
                            ))
                        } else {
                            Ok(Value::Number(Number {
                                value: Num::Float(x.value.to_f64()
                                                  .atan2(y.value.to_f64())),
                                unit: x.unit.clone(),
                            }))
                        }
                    }),
                    "sinh" => func!(fn sinh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().sinh()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "cosh" => func!(fn cosh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().cosh()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "tanh" => func!(fn tanh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().tanh()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "asinh" => func!(fn asinh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().asinh()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "acosh" => func!(fn acosh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().acosh()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    "atanh" => func!(fn atanh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Num::Float(num.value.to_f64().atanh()),
                            unit: num.unit.clone(),
                        }))
                    }),
                    _ => Err(QueryError::Generic(format!(
                        "Function not found: {}", name
                    )))
                }
            },
            Expr::Error(ref e) => Err(QueryError::Generic(e.clone())),
        }
    }

    pub fn eval_unit_name(&self, expr: &Expr) -> Result<(BTreeMap<String, isize>, Num), QueryError> {
        match *expr {
            Expr::Equals(ref left, ref _right) => match **left {
                Expr::Unit(ref name) => {
                    let mut map = BTreeMap::new();
                    map.insert(name.clone(), 1);
                    Ok((map, Num::one()))
                },
                ref x => Err(QueryError::Generic(format!(
                    "Expected identifier, got {:?}", x
                )))
            },
            Expr::Call(_, _) => Err(QueryError::Generic(format!(
                "Calls are not allowed in the right hand side of conversions"
            ))),
            Expr::Unit(ref name) | Expr::Quote(ref name) => {
                let mut map = BTreeMap::new();
                map.insert(self.canonicalize(&**name)
                           .unwrap_or_else(|| name.clone()), 1);
                Ok((map, Num::one()))
            },
            Expr::Const(ref i) =>
                Ok((BTreeMap::new(), i.clone())),
            Expr::Frac(ref left, ref right) => {
                let (left, lv) = try!(self.eval_unit_name(left));
                let (right, rv) = try!(self.eval_unit_name(right));
                let right = right.into_iter()
                    .map(|(k,v)| (k, -v)).collect::<BTreeMap<_, _>>();
                Ok((::btree_merge(
                    &left, &right,
                    |a,b| if a+b != 0 { Some(a + b) } else { None }),
                    &lv / &rv
                ))
            },
            Expr::Mul(ref args) => {
                args[1..].iter().fold(self.eval_unit_name(&args[0]), |acc, b| {
                    let (acc, av) = try!(acc);
                    let (b, bv) = try!(self.eval_unit_name(b));
                    Ok((::btree_merge(
                        &acc, &b,
                        |a,b| if a+b != 0 { Some(a+b) } else { None }),
                        &av * &bv
                    ))
                })
            },
            Expr::Pow(ref left, ref exp) => {
                let res = try!(self.eval(exp));
                let res = match res {
                    Value::Number(num) => num,
                    _ => return Err(QueryError::Generic(format!(
                        "Exponents must be numbers"
                    )))
                };
                if !res.dimless() {
                    return Err(QueryError::Generic(format!(
                        "Exponents must be dimensionless"
                    )))
                }
                let res = res.value.to_f64();
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
            Expr::Of(ref name, ref expr) => {
                let res = try!(self.eval(expr));
                let res = match res {
                    Value::Substance(sub) => sub,
                    _ => return Err(QueryError::Generic(format!(
                        "Property access on non-substance"
                    )))
                };
                let name = if let Some(prop) = res.properties.properties.get(name) {
                    if prop.input == Number::one() {
                        &prop.input_name
                    } else {
                        name
                    }
                } else {
                    name
                };
                let mut map = BTreeMap::new();
                map.insert(self.canonicalize(&**name)
                           .unwrap_or_else(|| name.clone()), 1);
                Ok((map, Num::one()))
            },
            Expr::Add(ref left, ref right) |
            Expr::Sub(ref left, ref right) => {
                let left = try!(self.eval_unit_name(left));
                let right = try!(self.eval_unit_name(right));
                if left != right {
                    return Err(QueryError::Generic(format!(
                        "Add of values with differing \
                         dimensions is not meaningful"
                    )))
                }
                Ok(left)
            },
            Expr::Neg(ref v) => self.eval_unit_name(v).map(|(u, v)| (u, -&v)),
            Expr::Plus(ref v) => self.eval_unit_name(v),
            Expr::Suffix(_, _) =>
                Err(QueryError::Generic(format!(
                    "Temperature conversions must not be compound units"
                ))),
            Expr::Date(_) => Err(QueryError::Generic(format!(
                "Dates are not allowed in the right hand side of conversions"
            ))),
            Expr::Error(ref e) => Err(QueryError::Generic(e.clone())),
        }
    }

    fn conformance_err(&self, top: &Number, bottom: &Number) -> ConformanceError {
        let mut topu = top.clone();
        topu.value = Num::one();
        let mut bottomu = bottom.clone();
        bottomu.value = Num::one();
        let mut suggestions = vec![];
        let diff = (&topu * &bottomu).unwrap();
        if diff.dimless() {
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
    }

    pub fn show(
        &self,
        raw: &Number,
        bottom: &Number,
        bottom_name: BTreeMap<String, isize>,
        bottom_const: Num,
        base: u8
    ) -> ConversionReply {
        let (exact, approx) = raw.numeric_value(base);
        let bottom_name = bottom_name.into_iter().map(
            |(a,b)| (Dim::new(&*a), b as i64)).collect();
        let (num, den) = bottom_const.to_rational();
        ConversionReply {
            value: NumberParts {
                exact_value: exact,
                approx_value: approx,
                factor: if num != Int::one() {
                    Some(format!("{}", num))
                } else {
                    None
                },
                divfactor: if den != Int::one() {
                    Some(format!("{}", den))
                } else {
                    None
                },
                unit: Some(Number::unit_to_string(&bottom_name)),
                raw_unit: Some(bottom_name),
                ..bottom.to_parts(self)
            },
        }
    }

    fn to_list(
        &self, top: &Number, list: &[&str]
    ) -> Result<Vec<NumberParts>, QueryError> {
        let units = try!(list.iter().map(|x| {
            self.lookup(x).ok_or_else(|| self.unknown_unit_err(x))
        }).collect::<Result<Vec<Number>, _>>());
        {
            let first = try!(units.first().ok_or(
                format!("Expected non-empty unit list")));
            try!(units.iter().skip(1).map(|x| {
                if first.unit != x.unit {
                    Err(format!(
                        "Units in unit list must conform: <{}> ; <{}>",
                        first.show(self), x.show(self)))
                } else {
                    Ok(())
                }
            }).collect::<Result<Vec<()>, _>>());
            if top.unit != first.unit {
                return Err(QueryError::Conformance(
                    self.conformance_err(&top, &first)))
            }
        }
        let mut value = top.value.clone();
        let mut out = vec![];
        let len = units.len();
        for (i, unit) in units.into_iter().enumerate() {
            if i == len-1 {
                out.push(&value / &unit.value);
            } else {
                let (div, rem) = value.div_rem(&unit.value);
                out.push(div);
                value = rem;
            }
        }
        Ok(list.into_iter().zip(out.into_iter()).map(|(name, value)| {
            let pretty = Number {
                value: value,
                unit: Number::one_unit(Dim::new(name)).unit
            }.to_parts(self);
            let unit: String = pretty.unit.or(pretty.dimensions)
                .map(|x| self.canonicalize(&*x).unwrap_or(x))
                .expect("to_parts returned no dimensions");
            let mut raw = BTreeMap::new();
            raw.insert(Dim::new(&unit), 1);
            NumberParts {
                unit: Some(unit),
                raw_unit: Some(raw),
                exact_value: Some(pretty.approx_value.or(pretty.exact_value)
                                  .expect("to_parts returned neither exact nor approx value")),
                ..Default::default()
            }
        }).collect())
    }

    /// Evaluates an expression, include `->` conversions.
    pub fn eval_outer(&self, expr: &Query) -> Result<QueryReply, QueryError> {
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
                    if self.dimensions.contains(&*name) {
                        break;
                    }
                    let unit_canon = self.canonicalize(unit).unwrap_or_else(|| unit.clone());
                    if self.dimensions.contains(&**unit) {
                        name = unit.clone();
                        canon = unit_canon;
                        break;
                    }
                    if self.definitions.get(unit).is_none() {
                        if self.definitions.get(&unit_canon).is_none() {
                            if !self.dimensions.contains(&**unit) {
                                break
                            } else {
                                assert!(name != *unit || canon != unit_canon);
                                name = unit.clone();
                                canon = unit_canon;
                                break;
                            }
                        } else {
                            assert!(name != unit_canon || canon != unit_canon);
                            name = unit_canon.clone();
                            canon = unit_canon;
                        }
                    } else {
                        assert!(name != *unit || canon != unit_canon);
                        name = unit.clone();
                        canon = unit_canon.clone();
                    }
                }
                let (def, def_expr, res) = if self.dimensions.contains(&*name) {
                    let parts = self.lookup(&name)
                        .expect("Lookup of base unit failed")
                        .to_parts(self);
                    let def = if let Some(ref q) = parts.quantity {
                        format!("base unit of {}", q)
                    } else {
                        format!("base unit")
                    };
                    (Some(def), None, None)
                } else {
                    let def = self.definitions.get(&name);
                    (def.as_ref().map(|x| format!("{}", x)),
                     def,
                     self.lookup(&name).map(|x| x.to_parts(self)))
                };
                Ok(QueryReply::Def(DefReply {
                    canon_name: canon,
                    def: def,
                    def_expr: def_expr.as_ref().map(|x| ExprReply::from(*x)),
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
            Query::Convert(ref top, Conversion::Expr(ref bottom), base) => match
                (self.eval(top), self.eval(bottom), self.eval_unit_name(bottom))
            {
                (Ok(Value::Number(top)), Ok(Value::Number(bottom)),
                 Ok((bottom_name, bottom_const))) => {
                    if top.unit == bottom.unit {
                        let raw = match &top / &bottom {
                            Some(raw) => raw,
                            None => return Err(QueryError::Generic(format!(
                                "Division by zero: {} / {}",
                                top.show(self), bottom.show(self))))
                        };
                        Ok(QueryReply::Conversion(self.show(
                            &raw, &bottom,
                            bottom_name, bottom_const,
                            base.unwrap_or(10))))
                    } else {
                        Err(QueryError::Conformance(self.conformance_err(
                            &top, &bottom)))
                    }
                },
                (Ok(Value::Substance(sub)), Ok(Value::Number(bottom)),
                 Ok((bottom_name, bottom_const))) => {
                    sub.get_in_unit(
                        bottom,
                        self,
                        bottom_name,
                        bottom_const,
                        base.unwrap_or(10)
                    ).map_err(
                        QueryError::Generic
                    ).map(
                        QueryReply::Substance
                    )
                },
                (Ok(Value::Number(top)), Ok(Value::Substance(mut sub)), _) => {
                    sub.amount = try!((&top / &sub.amount).ok_or_else(|| format!(
                        "Division by zero: <{}> / <{}>",
                        top.show(self),
                        sub.amount.show(self)
                    )));
                    sub.to_reply(
                        self
                    ).map_err(
                        QueryError::Generic
                    ).map(
                        QueryReply::Substance
                    )
                },
                (Ok(x), Ok(y), Ok(_)) => Err(QueryError::Generic(format!(
                    "Operation is not defined: <{}> -> <{}>",
                    x.show(self),
                    y.show(self)
                ))),
                (Err(e), _, _) => Err(e),
                (_, Err(e), _) => Err(e),
                (_, _, Err(e)) => Err(e),
            },
            Query::Convert(ref top, Conversion::List(ref list), None) => {
                let top = try!(self.eval(top));
                let top = match top {
                    Value::Number(num) => num,
                    _ => return Err(QueryError::Generic(format!(
                        "Cannot convert <{}> to {:?}", top.show(self), list)))
                };
                self.to_list(
                    &top,
                    &list.iter()
                        .map(|x| &**x)
                        .collect::<Vec<_>>()[..]
                ).map(|list| {
                    QueryReply::UnitList(UnitListReply {
                        rest: NumberParts {
                            quantity: self.quantities.get(&top.unit).cloned(),
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
                Ok(QueryReply::Date(DateReply::new(self, top)))
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
                        if top.unit != bottom.unit {
                            Err(QueryError::Conformance(
                                self.conformance_err(&top, &bottom)))
                        } else {
                            let res = (top - &self.lookup($base)
                                       .expect(&*format!("Constant {} missing", $base))).unwrap();
                            let res = (&res / &bottom).unwrap();
                            let mut name = BTreeMap::new();
                            name.insert(format!("°{}", $name), 1);
                            Ok(QueryReply::Conversion(self.show(
                                &res, &bottom,
                                name, Num::one(),
                                10)))
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
                let mut val = None;
                if let Expr::Unit(ref name) = *expr {
                    for (u, k) in &self.quantities {
                        if name == k {
                            val = Some(Number {
                                value: Num::one(),
                                unit: u.clone()
                            });
                            break
                        }
                    }
                }
                let val = match val {
                    None => {
                        let val = try!(self.eval(expr));
                        let val = match val {
                            Value::Number(val) => val,
                            _ => return Err(QueryError::Generic(format!(
                                "Cannot find derivatives of <{}>", val.show(self))),)
                        };
                        val
                    },
                    Some(val) => val
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
                let mut val = None;
                if let Expr::Unit(ref name) = *expr {
                    for (u, k) in &self.quantities {
                        if name == k {
                            val = Some(Number {
                                value: Num::one(),
                                unit: u.clone()
                            });
                            break
                        }
                    }
                }
                let val = match val {
                    None => {
                        let val = try!(self.eval(expr));
                        let val = match val {
                            Value::Number(val) => val,
                            _ => return Err(QueryError::Generic(format!(
                                "Cannot find units for <{}>", val.show(self)))),
                        };
                        val
                    },
                    Some(val) => val
                };
                let mut out = vec![];
                for (name, unit) in self.units.iter() {
                    if let Some(&Expr::Unit(_)) = self.definitions.get(name) {
                        continue
                    }
                    if val.unit == unit.unit {
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
            Query::Search(ref string) => {
                Ok(QueryReply::Search(SearchReply {
                    results: search::search(self, &**string, 5)
                        .into_iter()
                        .map(|x| {
                            let parts = self.lookup(x)
                                .map(|x| x.to_parts(self))
                                .or_else(|| if self.substances.get(x).is_some() {
                                    Some(NumberParts {
                                        quantity: Some("substance".to_owned()),
                                        .. Default::default()
                                    })
                                } else {
                                    None
                                })
                                .expect("Search returned non-existent result");
                            let mut raw = BTreeMap::new();
                            raw.insert(Dim::new(x), 1);
                            NumberParts {
                                unit: Some(x.to_owned()),
                                raw_unit: Some(raw),
                                quantity: parts.quantity,
                                ..Default::default()
                            }
                        })
                        .collect(),
                }))
            },
            Query::Expr(ref expr) |
            Query::Convert(ref expr, Conversion::None, None) => {
                let val = try!(self.eval(expr));
                match val {
                    Value::Number(ref n) if n.unit == Number::one_unit(Dim::new("s")).unit => {
                        let units = &["year", "week", "day", "hour", "minute", "second"];
                        let list = try!(self.to_list(&n, units));
                        let mut list = list.into_iter();
                        Ok(QueryReply::Duration(DurationReply {
                            raw: n.to_parts(self),
                            years: list.next().expect("Unexpected end of iterator"),
                            //months: list.next().expect("Unexpected end of iterator"),
                            months: NumberParts {
                                exact_value: Some("0".to_owned()),
                                unit: Some("month".to_owned()),
                                raw_unit: Some({
                                    let mut raw = BTreeMap::new();
                                    raw.insert(Dim::new("month"), 1);
                                    raw
                                }),
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
                    Value::DateTime(d) => Ok(QueryReply::Date(DateReply::new(self, d))),
                    Value::Substance(s) => Ok(QueryReply::Substance(
                        try!(s.to_reply(self).map_err(QueryError::Generic))
                    )),
                }
            },
            Query::Error(ref e) => Err(QueryError::Generic(e.clone())),
        }
    }
}
