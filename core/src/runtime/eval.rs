// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::{Show, SubstanceGetError, Value};
use crate::ast::{BinOpExpr, BinOpType, Conversion, Expr, Function, Query, UnaryOpType};
use crate::commands;
use crate::loader::Context;
use crate::output::{
    ConformanceError, ConversionReply, DateReply, DefReply, Digits, DurationReply, ExprReply,
    Factorization, FactorizeReply, NumberParts, QueryError, QueryReply, UnitListReply,
    UnitsForReply, UnitsInCategory,
};
use crate::parsing::{datetime, formula};
use crate::types::{BaseUnit, Dimensionality, GenericDateTime, Number, Numeric};
use chrono::FixedOffset;
use std::collections::BTreeMap;
use std::rc::Rc;

/// Evaluates an expression to compute its value, *excluding* `->`
/// conversions.
pub(crate) fn eval_expr(ctx: &Context, expr: &Expr) -> Result<Value, QueryError> {
    use std::ops::*;

    match *expr {
        Expr::Unit { ref name } if name == "now" => Ok(Value::DateTime(GenericDateTime::Fixed(
            ctx.now.fixed_offset(),
        ))),
        Expr::Unit { ref name } => ctx
            .lookup(name)
            .map(Value::Number)
            .or_else(|| {
                ctx.registry
                    .substances
                    .get(name)
                    .cloned()
                    .map(Value::Substance)
            })
            .or_else(|| {
                ctx.registry
                    .substance_symbols
                    .get(name)
                    .and_then(|full_name| {
                        ctx.registry
                            .substances
                            .get(full_name)
                            .cloned()
                            .map(Value::Substance)
                    })
            })
            .or_else(|| {
                formula::substance_from_formula(
                    name,
                    &ctx.registry.substance_symbols,
                    &ctx.registry.substances,
                )
                .map(Value::Substance)
            })
            .ok_or_else(|| QueryError::NotFound(ctx.unknown_unit_err(name))),
        Expr::Quote { ref string } => Ok(Value::Number(Number::one_unit(BaseUnit::new(string)))),
        Expr::Const { ref value } => Ok(Value::Number(Number::new(value.clone()))),
        Expr::Date { ref tokens } => match datetime::try_decode(tokens, ctx) {
            Ok(date) => Ok(Value::DateTime(date)),
            Err(e) => Err(QueryError::generic(e)),
        },

        Expr::BinOp(BinOpExpr {
            op: BinOpType::Equals,
            ref left,
            ref right,
        }) => {
            match **left {
                Expr::Unit { .. } => (),
                ref x => {
                    return Err(QueryError::generic(format!(
                        "= is currently only used for inline unit definitions: \
                             expected unit, got {}",
                        x
                    )))
                }
            };
            eval_expr(ctx, right)
        }

        Expr::BinOp(ref binop) => {
            let left = eval_expr(ctx, &binop.left)?;
            let right = eval_expr(ctx, &binop.right)?;
            let result = match binop.op {
                BinOpType::Add => left.add(&right),
                BinOpType::Sub => left.sub(&right),
                BinOpType::Frac => left.div(&right),
                BinOpType::Pow => left.pow(&right),
                BinOpType::Equals => panic!("Should be unreachable"),
                BinOpType::ShiftL => left.shl(&right),
                BinOpType::ShiftR => left.shr(&right),
                BinOpType::Mod => left.rem(&right),
                BinOpType::And => left.and(&right),
                BinOpType::Or => left.or(&right),
                BinOpType::Xor => left.xor(&right),
            };
            result.map_err(|e| {
                QueryError::generic(format!(
                    "{}: <{}> {} <{}>",
                    e,
                    left.show(ctx),
                    binop.op.symbol().trim(),
                    right.show(ctx)
                ))
            })
        }

        Expr::UnaryOp(ref unaryop) => match unaryop.op {
            UnaryOpType::Positive => eval_expr(ctx, &unaryop.expr),
            UnaryOpType::Negative => eval_expr(ctx, &unaryop.expr).and_then(|v| {
                (-&v).map_err(|e| QueryError::generic(format!("{}: - <{}>", e, v.show(ctx))))
            }),
            UnaryOpType::Degree(ref suffix) => {
                let (name, base, scale) = suffix.name_base_scale();

                let expr = eval_expr(ctx, &unaryop.expr)?;
                let expr = match expr {
                    Value::Number(expr) => expr,
                    _ => {
                        return Err(QueryError::generic(format!(
                            "Expected number, got: <{}> °{}",
                            expr.show(ctx),
                            name
                        )))
                    }
                };
                if expr.unit != Dimensionality::new() {
                    Err(QueryError::generic(format!(
                        "Expected dimensionless, got: <{}>",
                        expr.show(ctx)
                    )))
                } else {
                    let expr = (&expr
                        * &ctx
                            .lookup(scale)
                            .expect(&*format!("Missing {} unit", scale)))
                        .unwrap();
                    Ok(Value::Number(
                        (&expr
                            + &ctx
                                .lookup(base)
                                .expect(&*format!("Missing {} constant", base)))
                            .unwrap(),
                    ))
                }
            }
        },

        Expr::Mul { ref exprs } => exprs.iter().fold(Ok(Value::Number(Number::one())), |a, b| {
            a.and_then(|a| {
                let b = eval_expr(ctx, b)?;
                (&a * &b).map_err(|e| {
                    QueryError::generic(format!("{}: <{}> * <{}>", e, a.show(ctx), b.show(ctx)))
                })
            })
        }),
        Expr::Of {
            ref property,
            ref expr,
        } => {
            let expr = eval_expr(ctx, expr)?;
            let expr = match expr {
                Value::Substance(sub) => sub,
                x => {
                    return Err(QueryError::generic(format!(
                        "Not defined: {} of <{}>",
                        property,
                        x.show(ctx)
                    )))
                }
            };
            expr.get(property).map(Value::Number).map_err(|e| match e {
                SubstanceGetError::Generic(s) => QueryError::generic(s),
                SubstanceGetError::Conformance(l, r) => {
                    QueryError::Conformance(Box::new(conformance_err(ctx, &l, &r)))
                }
            })
        }
        Expr::Call { ref func, ref args } => {
            let args = args
                .iter()
                .map(|x| eval_expr(ctx, x))
                .collect::<Result<Vec<_>, _>>()?;

            macro_rules! func {
                    (fn $fname:ident($($name:ident : $ty:ident),*) $block:block) => {{
                        let mut iter = args.iter();
                        let mut count = 0;
                        $( count += 1; let _ = stringify!($name); )*
                        $(
                            let $name = match iter.next() {
                                Some(&Value::$ty(ref v)) => v,
                                Some(x) => return Err(QueryError::generic(
                                    format!(
                                        "Expected {}, got <{}>",
                                        stringify!($ty), x.show(ctx)
                                    )
                                )),
                                None => return Err(QueryError::generic(format!(
                                    "Argument number mismatch for {}: \
                                     Expected {}, got {}",
                                    stringify!($fname), count, args.len()
                                )))
                            };
                        )*
                        if iter.next().is_some() {
                            return Err(QueryError::generic(format!(
                                "Argument number mismatch for {}: \
                                 Expected {}, got {}",
                                stringify!($fname), count, args.len()
                            )));
                        }
                        let res: Result<Value, String> = {
                            $block
                        };
                        res.map_err(|e| {
                            QueryError::generic(format!(
                                "{}: {}({})",
                                e, stringify!($fname),
                                args.iter()
                                    .map(|x| x.show(ctx))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ))
                        })
                    }
                }}

            match func {
                Function::Sqrt => func!(
                    fn sqrt(num: Number) {
                        num.root(2).map(Value::Number)
                    }
                ),
                Function::Exp => func!(
                    fn exp(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().exp()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Ln => func!(
                    fn ln(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().ln()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Log => func!(
                    fn log(num: Number, base: Number) {
                        if !base.unit.is_dimensionless() {
                            Err("Base must be dimensionless".to_string())
                        } else {
                            Ok(Value::Number(Number {
                                value: Numeric::Float(num.value.to_f64().log(base.value.to_f64())),
                                unit: num.unit.clone(),
                            }))
                        }
                    }
                ),
                Function::Log2 => func!(
                    fn log2(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().log2()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Log10 => func!(
                    fn ln(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().log10()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Hypot => func!(
                    fn hypot(x: Number, y: Number) {
                        if x.unit != y.unit {
                            Err("Arguments to hypot must have matching dimensionality".to_string())
                        } else {
                            Ok(Value::Number(Number {
                                value: Numeric::Float(x.value.to_f64().hypot(y.value.to_f64())),
                                unit: x.unit.clone(),
                            }))
                        }
                    }
                ),
                Function::Sin => func!(
                    fn sin(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().sin()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Cos => func!(
                    fn cos(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().cos()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Tan => func!(
                    fn tan(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().tan()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Asin => func!(
                    fn asin(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().asin()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Acos => func!(
                    fn acos(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().acos()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Atan => func!(
                    fn atan(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().atan()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Atan2 => func!(
                    fn atan2(x: Number, y: Number) {
                        if x.unit != y.unit {
                            Err("Arguments to atan2 must have matching dimensionality".to_string())
                        } else {
                            Ok(Value::Number(Number {
                                value: Numeric::Float(x.value.to_f64().atan2(y.value.to_f64())),
                                unit: x.unit.clone(),
                            }))
                        }
                    }
                ),
                Function::Sinh => func!(
                    fn sinh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().sinh()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Cosh => func!(
                    fn cosh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().cosh()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Tanh => func!(
                    fn tanh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().tanh()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Asinh => func!(
                    fn asinh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().asinh()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Acosh => func!(
                    fn acosh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().acosh()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
                Function::Atanh => func!(
                    fn atanh(num: Number) {
                        Ok(Value::Number(Number {
                            value: Numeric::Float(num.value.to_f64().atanh()),
                            unit: num.unit.clone(),
                        }))
                    }
                ),
            }
        }
        Expr::Error { ref message } => Err(QueryError::generic(message.clone())),
    }
}

pub fn eval_unit_name(
    ctx: &Context,
    expr: &Expr,
) -> Result<(BTreeMap<String, isize>, Numeric), QueryError> {
    match *expr {
        Expr::Call { .. } => Err(QueryError::generic(
            "Calls are not allowed in the right hand side of conversions".to_string(),
        )),
        Expr::Unit { ref name } | Expr::Quote { string: ref name } => {
            let mut map = BTreeMap::new();
            map.insert(ctx.canonicalize(&**name).unwrap_or_else(|| name.clone()), 1);
            Ok((map, Numeric::one()))
        }
        Expr::Const { ref value } => Ok((BTreeMap::new(), value.clone())),
        Expr::BinOp(ref binop) => match binop.op {
            BinOpType::Equals => match *binop.left {
                Expr::Unit { ref name } => {
                    let mut map = BTreeMap::new();
                    map.insert(name.clone(), 1);
                    Ok((map, Numeric::one()))
                }
                ref x => Err(QueryError::generic(format!(
                    "Expected identifier, got {:?}",
                    x
                ))),
            },
            BinOpType::Add | BinOpType::Sub => {
                let (left_unit, left) = eval_unit_name(ctx, &binop.left)?;
                let (right_unit, _right) = eval_unit_name(ctx, &binop.right)?;

                if left_unit != right_unit {
                    return Err(QueryError::generic(
                        "Add of values with differing \
                                 dimensions is not meaningful"
                            .to_string(),
                    ));
                }
                Ok((left_unit, left))
            }
            BinOpType::Frac => {
                let (left_unit, left) = eval_unit_name(ctx, &binop.left)?;
                let (right_unit, right) = eval_unit_name(ctx, &binop.right)?;

                let right_unit = right_unit
                    .into_iter()
                    .map(|(k, v)| (k, -v))
                    .collect::<BTreeMap<_, _>>();
                Ok((
                    crate::algorithms::btree_merge(&left_unit, &right_unit, |a, b| {
                        if a + b != 0 {
                            Some(a + b)
                        } else {
                            None
                        }
                    }),
                    &left / &right,
                ))
            }
            BinOpType::Pow => {
                let right = eval_expr(ctx, &binop.right)?;
                let right = match right {
                    Value::Number(ref num) => num,
                    _ => return Err(QueryError::generic("Exponents must be numbers".to_string())),
                };
                if !right.dimless() {
                    return Err(QueryError::generic(
                        "Exponents must be dimensionless".to_string(),
                    ));
                }
                let right = right.value.to_f64();
                let (left_unit, left_value) = eval_unit_name(ctx, &binop.left)?;
                Ok((
                    left_unit
                        .into_iter()
                        .filter_map(|(k, v)| {
                            let v = v * right as isize;
                            if v != 0 {
                                Some((k, v))
                            } else {
                                None
                            }
                        })
                        .collect::<BTreeMap<_, _>>(),
                    left_value.pow(right as i32),
                ))
            }
            BinOpType::ShiftL => todo!(),
            BinOpType::ShiftR => todo!(),
            BinOpType::Mod => {
                let (left_unit, left) = eval_unit_name(ctx, &binop.left)?;
                let (right_unit, _right) = eval_unit_name(ctx, &binop.right)?;

                if left_unit != right_unit {
                    return Err(QueryError::generic(
                        "Modulo of values with differing dimensions is not meaningful".to_string(),
                    ));
                }
                Ok((left_unit, left))
            }
            BinOpType::And | BinOpType::Or | BinOpType::Xor => {
                let (left_unit, left) = eval_unit_name(ctx, &binop.left)?;
                let (right_unit, _right) = eval_unit_name(ctx, &binop.right)?;

                if !left_unit.is_empty() || !right_unit.is_empty() {
                    return Err(QueryError::generic(format!(
                        "Arguments to {:?} must be dimensionless",
                        binop.op
                    )));
                }
                Ok((left_unit, left))
            }
        },
        Expr::Mul { ref exprs } => {
            exprs[1..]
                .iter()
                .fold(eval_unit_name(ctx, &exprs[0]), |acc, b| {
                    let (acc, av) = acc?;
                    let (b, bv) = eval_unit_name(ctx, b)?;
                    Ok((
                        crate::algorithms::btree_merge(&acc, &b, |a, b| {
                            if a + b != 0 {
                                Some(a + b)
                            } else {
                                None
                            }
                        }),
                        &av * &bv,
                    ))
                })
        }
        Expr::Of {
            ref property,
            ref expr,
        } => {
            let res = eval_expr(ctx, expr)?;
            let res = match res {
                Value::Substance(sub) => sub,
                _ => {
                    return Err(QueryError::generic(
                        "Property access on non-substance".to_string(),
                    ))
                }
            };
            let property = if let Some(prop) = res.properties.properties.get(property) {
                if prop.input == Number::one() {
                    &prop.input_name
                } else {
                    property
                }
            } else {
                property
            };
            let mut map = BTreeMap::new();
            map.insert(
                ctx.canonicalize(property)
                    .unwrap_or_else(|| property.clone()),
                1,
            );
            Ok((map, Numeric::one()))
        }
        Expr::UnaryOp(ref unaryop) => match unaryop.op {
            UnaryOpType::Positive => eval_unit_name(ctx, &unaryop.expr),
            UnaryOpType::Negative => eval_unit_name(ctx, &unaryop.expr).map(|(u, v)| (u, -&v)),
            UnaryOpType::Degree(_) => Err(QueryError::generic(
                "Temperature conversions must not be compound units".to_string(),
            )),
        },
        Expr::Date { .. } => Err(QueryError::generic(
            "Dates are not allowed in the right hand side of conversions".to_string(),
        )),
        Expr::Error { ref message } => Err(QueryError::generic(message.clone())),
    }
}

fn conformance_err(ctx: &Context, top: &Number, bottom: &Number) -> ConformanceError {
    fn multiply_or_divide(recip: bool) -> &'static str {
        if recip {
            "divide"
        } else {
            "multiply"
        }
    }

    let mut topu = top.clone();
    topu.value = Numeric::one();
    let mut bottomu = bottom.clone();
    bottomu.value = Numeric::one();
    let mut suggestions = vec![];
    let diff = (&topu * &bottomu).unwrap();
    if diff.dimless() {
        suggestions.push("Reciprocal conversion, invert one side".to_string());
    } else {
        let diff = (&topu / &bottomu).unwrap();
        let (recip, desc) = ctx.describe_unit(&diff.invert());
        let word = multiply_or_divide(recip);
        suggestions.push(format!("{word} left side by {}", desc.trim(), word = word));
        let (recip, desc) = ctx.describe_unit(&diff);
        let word = multiply_or_divide(recip);
        suggestions.push(format!("{word} right side by {}", desc.trim(), word = word));
    }

    ConformanceError {
        left: top.to_parts(ctx),
        right: bottom.to_parts(ctx),
        suggestions,
    }
}

fn to_list(ctx: &Context, top: &Number, list: &[&str]) -> Result<Vec<NumberParts>, QueryError> {
    let units = list
        .iter()
        .map(|x| ctx.lookup(x).ok_or_else(|| ctx.unknown_unit_err(x)))
        .collect::<Result<Vec<Number>, _>>()?;
    {
        let first = units
            .first()
            .ok_or_else(|| "Expected non-empty unit list".to_string())?;
        units
            .iter()
            .skip(1)
            .map(|x| {
                if first.unit != x.unit {
                    Err(format!(
                        "Units in unit list must conform: <{}> ; <{}>",
                        first.show(ctx),
                        x.show(ctx)
                    ))
                } else {
                    Ok(())
                }
            })
            .collect::<Result<Vec<()>, _>>()?;
        if top.unit != first.unit {
            return Err(QueryError::Conformance(Box::new(conformance_err(
                ctx, top, first,
            ))));
        }
    }
    let mut value = top.value.clone();
    let mut out = vec![];
    let len = units.len();
    for (i, unit) in units.into_iter().enumerate() {
        if i == len - 1 {
            out.push(&value / &unit.value);
        } else {
            let (div, rem) = value.div_rem(&unit.value);
            out.push(div);
            value = rem;
        }
    }
    Ok(list
        .iter()
        .zip(out.into_iter())
        .map(|(name, value)| {
            let raw_number = Number {
                value,
                unit: Number::one_unit(BaseUnit::new(name)).unit,
            };
            let pretty = raw_number.to_parts(ctx);
            let unit: String = pretty
                .unit
                .or(pretty.dimensions)
                .map(|x| ctx.canonicalize(&*x).unwrap_or(x))
                .expect("to_parts returned no dimensions");
            let raw = Dimensionality::base_unit(BaseUnit::new(&unit));
            NumberParts {
                raw_value: Some(raw_number),
                unit: Some(unit),
                raw_unit: Some(raw),
                exact_value: Some(
                    pretty
                        .approx_value
                        .or(pretty.exact_value)
                        .expect("to_parts returned neither exact nor approx value"),
                ),
                ..Default::default()
            }
        })
        .collect())
}

/// Returns true if this unit has a definition that can be shown. Units
/// with prefixes can't be.
fn can_show_definition(ctx: &Context, name: &str) -> bool {
    if ctx.registry.definitions.contains_key(name) {
        return true;
    }

    if ctx.registry.base_units.contains(name) {
        return true;
    }

    if let Some(name) = ctx.canonicalize(name) {
        // Canonicalizing doesn't always result in a unit that actually exists, because it can return units that still have a prefix, e.g. micrometer.
        if ctx.registry.definitions.contains_key(&name) {
            return true;
        }
        if ctx.registry.base_units.contains(&name[..]) {
            return true;
        }
    }

    false
}

/// Recursively expands aliases to find the canonical version of the
/// unit, so that its most fundamental definition can be shown.
///
/// # Examples
///
/// - `ft` -> (`foot`, `foot`)
fn expand_aliases(ctx: &Context, name: &str) -> (String, String) {
    let mut name = name.to_owned();
    let mut canon = ctx.canonicalize(&name).unwrap_or_else(|| name.clone());

    while let Some(&Expr::Unit { name: ref unit }) = {
        ctx.registry
            .definitions
            .get(&name)
            .or_else(|| ctx.registry.definitions.get(&*canon))
    } {
        if ctx.registry.base_units.contains(&*name) {
            break;
        }
        let unit_canon = ctx.canonicalize(unit).unwrap_or_else(|| unit.clone());
        if ctx.registry.base_units.contains(&**unit) {
            name = unit.clone();
            canon = unit_canon;
            break;
        }
        if ctx.registry.definitions.get(unit).is_none() {
            if ctx.registry.definitions.get(&unit_canon).is_none() {
                if !ctx.registry.base_units.contains(&**unit) {
                    break;
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

    (name, canon)
}

/// Given a name, returns the dimensionality if it is a quantity, or None if it isn't.
fn find_quantity(ctx: &Context, name: &str) -> Option<Dimensionality> {
    for (dims, quantity_name) in &ctx.registry.quantities {
        if quantity_name == name {
            return Some(dims.clone());
        }
    }
    None
}

pub(crate) fn eval_query(ctx: &Context, expr: &Query) -> Result<QueryReply, QueryError> {
    match *expr {
        Query::Expr(Expr::Unit { ref name }) if can_show_definition(ctx, name) => {
            let (name, canon_name) = expand_aliases(ctx, name);

            let (def, def_expr, value) =
                if let Some(base_unit) = ctx.registry.base_units.get(&*name) {
                    let parts = Number::one_unit(base_unit.clone()).to_parts(ctx);
                    let def = if let Some(ref q) = parts.quantity {
                        format!("base unit of {}", q)
                    } else {
                        "base unit".to_string()
                    };
                    (Some(def), None, None)
                } else if let Some(dims) = find_quantity(ctx, &name) {
                    let def = ctx
                        .registry
                        .definitions
                        .get(&name)
                        .expect("quantities should always have definitions");
                    let description = format!("physical quantity for {def} ({dims})");

                    (Some(description), None, None)
                } else {
                    let def = ctx.registry.definitions.get(&name);
                    (
                        def.as_ref().map(|x| x.to_string()),
                        def,
                        ctx.lookup(&name).map(|x| x.to_parts(ctx)),
                    )
                };
            Ok(QueryReply::Def(Box::new(DefReply {
                def_expr: def_expr.as_ref().map(|x| ExprReply::from(*x)),
                doc: ctx.registry.docs.get(&name).cloned(),
                canon_name,
                def,
                value,
            })))
        }
        Query::Convert(ref top, Conversion::None, Some(base), digits) => {
            let top = eval_expr(ctx, top)?;
            let top = match top {
                Value::Number(top) => top,
                _ => {
                    return Err(QueryError::generic(format!(
                        "<{}> in base {} is not defined",
                        top.show(ctx),
                        base
                    )))
                }
            };
            let (exact, approx) = top.numeric_value(base, digits);
            let parts = NumberParts {
                raw_value: Some(top.clone()),
                exact_value: exact,
                approx_value: approx,
                ..top.to_parts(ctx)
            };
            Ok(QueryReply::Conversion(Box::new(ConversionReply {
                value: parts,
            })))
        }
        Query::Convert(ref top, Conversion::None, base, digits @ Digits::Digits(_))
        | Query::Convert(ref top, Conversion::None, base, digits @ Digits::FullInt) => {
            let top = eval_expr(ctx, top)?;
            let top = match top {
                Value::Number(top) => top,
                _ => {
                    return Err(QueryError::generic(format!(
                        "<{}> to {} is not defined",
                        top.show(ctx),
                        match digits {
                            Digits::Default => unreachable!(),
                            Digits::FullInt => "digits".to_owned(),
                            Digits::Digits(n) => format!("{} digits", n),
                        }
                    )))
                }
            };
            let (exact, approx) = top.numeric_value(base.unwrap_or(10), digits);
            let parts = NumberParts {
                raw_value: Some(top.clone()),
                exact_value: exact,
                approx_value: approx,
                ..top.to_parts(ctx)
            };
            Ok(QueryReply::Conversion(Box::new(ConversionReply {
                value: parts,
            })))
        }
        Query::Convert(ref top, Conversion::Expr(ref bottom), base, digits) => match (
            eval_expr(ctx, top)?,
            eval_expr(ctx, bottom)?,
            eval_unit_name(ctx, bottom)?,
        ) {
            (Value::Number(top), Value::Number(bottom), (bottom_name, bottom_const)) => {
                if top.unit == bottom.unit {
                    let raw = match &top / &bottom {
                        Some(raw) => raw,
                        None => {
                            return Err(QueryError::generic(format!(
                                "Division by zero: {} / {}",
                                top.show(ctx),
                                bottom.show(ctx)
                            )))
                        }
                    };
                    Ok(QueryReply::Conversion(Box::new(ctx.show(
                        &raw,
                        &bottom,
                        bottom_name,
                        bottom_const,
                        base.unwrap_or(10),
                        digits,
                    ))))
                } else {
                    Err(QueryError::Conformance(Box::new(conformance_err(
                        ctx, &top, &bottom,
                    ))))
                }
            }
            (Value::Substance(sub), Value::Number(bottom), (bottom_name, bottom_const)) => sub
                .get_in_unit(
                    bottom,
                    ctx,
                    bottom_name,
                    bottom_const,
                    base.unwrap_or(10),
                    digits,
                )
                .map_err(QueryError::generic)
                .map(QueryReply::Substance),
            (Value::Number(top), Value::Substance(mut sub), (bottom_name, bottom_const)) => {
                let unit = sub.amount.clone();
                sub.amount = top;
                sub.get_in_unit(
                    unit,
                    ctx,
                    bottom_name,
                    bottom_const,
                    base.unwrap_or(10),
                    digits,
                )
                .map_err(QueryError::generic)
                .map(QueryReply::Substance)
            }
            (x, y, _) => Err(QueryError::generic(format!(
                "Operation is not defined: <{}> -> <{}>",
                x.show(ctx),
                y.show(ctx)
            ))),
        },
        Query::Convert(ref top, Conversion::List(ref list), None, Digits::Default) => {
            let top = eval_expr(ctx, top)?;
            let top = match top {
                Value::Number(num) => num,
                _ => {
                    return Err(QueryError::generic(format!(
                        "Cannot convert <{}> to {:?}",
                        top.show(ctx),
                        list
                    )))
                }
            };
            to_list(
                ctx,
                &top,
                &list.iter().map(|x| &**x).collect::<Vec<_>>()[..],
            )
            .map(|list| {
                QueryReply::UnitList(UnitListReply {
                    rest: NumberParts {
                        quantity: ctx.registry.quantities.get(&top.unit).cloned(),
                        ..Default::default()
                    },
                    list,
                })
            })
        }
        Query::Convert(ref top, Conversion::Offset(off), None, Digits::Default) => {
            let top = eval_expr(ctx, top)?;
            let top = match top {
                Value::DateTime(date) => date,
                _ => {
                    return Err(QueryError::generic(format!(
                        "Cannot convert <{}> to timezone offset {:+}",
                        top.show(ctx),
                        off
                    )))
                }
            };
            let top = top.with_timezone(&FixedOffset::east_opt(off as i32).unwrap());
            Ok(QueryReply::Date(DateReply::new(ctx, top)))
        }
        Query::Convert(ref top, Conversion::Timezone(tz), None, Digits::Default) => {
            let top = eval_expr(ctx, top)?;
            let top = match top {
                Value::DateTime(date) => date,
                _ => {
                    return Err(QueryError::generic(format!(
                        "Cannot convert <{}> to timezone {:?}",
                        top.show(ctx),
                        tz
                    )))
                }
            };
            let top = top.with_timezone(&tz);
            Ok(QueryReply::Date(DateReply::new(ctx, top)))
        }
        Query::Convert(ref top, Conversion::Degree(ref deg), None, digits) => {
            let (name, base, scale) = deg.name_base_scale();

            let top = eval_expr(ctx, top)?;
            let top = match top {
                Value::Number(ref num) => num,
                _ => {
                    return Err(QueryError::generic(format!(
                        "Cannot convert <{}> to °{}",
                        top.show(ctx),
                        name
                    )))
                }
            };
            let bottom = ctx
                .lookup(scale)
                .expect(&*format!("Unit {} missing", scale));
            if top.unit != bottom.unit {
                Err(QueryError::Conformance(Box::new(conformance_err(
                    ctx, top, &bottom,
                ))))
            } else {
                let res = (top
                    - &ctx
                        .lookup(base)
                        .expect(&*format!("Constant {} missing", base)))
                    .unwrap();
                let res = (&res / &bottom).unwrap();
                let mut name = BTreeMap::new();
                name.insert(deg.to_string(), 1);
                Ok(QueryReply::Conversion(Box::new(ctx.show(
                    &res,
                    &bottom,
                    name,
                    Numeric::one(),
                    10,
                    digits,
                ))))
            }
        }
        Query::Convert(ref _expr, ref which, Some(base), _digits) => Err(QueryError::generic(
            format!("Conversion to {} is not defined in base {}", which, base),
        )),
        Query::Convert(ref _expr, ref which, _base, Digits::Digits(digits)) => {
            Err(QueryError::generic(format!(
                "Conversion to {} is not defined to {} digits",
                which, digits
            )))
        }
        Query::Convert(ref _expr, ref which, _base, Digits::FullInt) => Err(QueryError::generic(
            format!("Conversion to digits of {} is not defined", which),
        )),
        Query::Factorize(ref expr) => {
            let mut val = None;
            if let Expr::Unit { ref name } = *expr {
                for (u, k) in &ctx.registry.quantities {
                    if name == k {
                        val = Some(Number {
                            value: Numeric::one(),
                            unit: u.clone(),
                        });
                        break;
                    }
                }
            }
            let val = match val {
                None => {
                    let val = eval_expr(ctx, expr)?;
                    match val {
                        Value::Number(val) => val,
                        _ => {
                            return Err(QueryError::generic(format!(
                                "Cannot find derivatives of <{}>",
                                val.show(ctx)
                            )))
                        }
                    }
                }
                Some(val) => val,
            };
            let quantities = ctx
                .registry
                .quantities
                .iter()
                .map(|(a, b)| (a.clone(), Rc::new(b.clone())))
                .collect::<BTreeMap<_, _>>();
            let results = commands::factorize(&val, &quantities);
            let mut results = results.into_sorted_vec();
            results.dedup();
            let results = results
                .into_iter()
                .map(|commands::Factors(_score, names)| {
                    let mut next = BTreeMap::<Rc<String>, usize>::new();
                    for name in names.into_iter() {
                        *next.entry(name).or_insert(0) += 1;
                    }
                    Factorization { units: next }
                })
                .collect::<Vec<_>>();
            Ok(QueryReply::Factorize(FactorizeReply {
                factorizations: results,
            }))
        }
        Query::UnitsFor(ref expr) => {
            let mut val = None;
            if let Expr::Unit { ref name } = *expr {
                for (u, k) in &ctx.registry.quantities {
                    if name == k {
                        val = Some(Number {
                            value: Numeric::one(),
                            unit: u.clone(),
                        });
                        break;
                    }
                }
            }
            let val = match val {
                None => {
                    let val = eval_expr(ctx, expr)?;
                    match val {
                        Value::Number(val) => val,
                        _ => {
                            return Err(QueryError::generic(format!(
                                "Cannot find units for <{}>",
                                val.show(ctx)
                            )))
                        }
                    }
                }
                Some(val) => val,
            };
            let dim_name;
            let mut out = vec![];
            for (name, unit) in ctx.registry.units.iter() {
                if let Some(&Expr::Unit { .. }) = ctx.registry.definitions.get(name) {
                    continue;
                }
                let category = ctx.registry.categories.get(name);
                if val.unit == unit.unit {
                    out.push((category, name));
                }
            }
            if let Some((dim, _power)) = val.unit.as_single() {
                dim_name = ctx
                    .canonicalize(dim.as_str())
                    .unwrap_or_else(|| dim.to_string());
                let category = ctx.registry.categories.get(&dim_name);
                out.push((category, &dim_name));
            }
            out.sort_by(|&(ref c1, ref n1), &(ref c2, ref n2)| {
                use std::cmp::Ordering;
                match (c1, c2) {
                    (&None, &None) => Ordering::Equal,
                    (&Some(_), &None) => Ordering::Less,
                    (&None, &Some(_)) => Ordering::Greater,
                    (&Some(ref a), &Some(ref b)) => (a, n1).cmp(&(b, n2)),
                }
            });
            let mut categories = vec![];
            let mut cur = vec![];
            let mut cur_cat = None;
            for (category, name) in out {
                if category != cur_cat {
                    if !cur.is_empty() {
                        let cat_name = cur_cat.and_then(|x| ctx.registry.category_names.get(x));
                        categories.push(UnitsInCategory {
                            category: cat_name.map(ToOwned::to_owned),
                            units: cur.drain(..).collect(),
                        });
                    }
                    cur_cat = category;
                }
                cur.push(name.clone());
            }
            if !cur.is_empty() {
                let cat_name = cur_cat.and_then(|x| ctx.registry.category_names.get(x));
                categories.push(UnitsInCategory {
                    category: cat_name.map(ToOwned::to_owned),
                    units: cur,
                });
            }
            let parts = val.to_parts(ctx);
            Ok(QueryReply::UnitsFor(UnitsForReply {
                units: categories,
                of: parts,
            }))
        }
        Query::Search(ref string) => Ok(QueryReply::Search(commands::search(ctx, &**string, 5))),
        Query::Expr(ref expr)
        | Query::Convert(ref expr, Conversion::None, None, Digits::Default) => {
            let val = eval_expr(ctx, expr)?;
            match val {
                Value::Number(ref n) if n.unit == Number::one_unit(BaseUnit::new("s")).unit => {
                    let units = &["year", "week", "day", "hour", "minute", "second"];
                    let list = to_list(ctx, n, units)?;
                    let mut list = list.into_iter();
                    Ok(QueryReply::Duration(Box::new(DurationReply {
                        raw: n.to_parts(ctx),
                        years: list.next().expect("Unexpected end of iterator"),
                        //months: list.next().expect("Unexpected end of iterator"),
                        months: NumberParts {
                            exact_value: Some("0".to_owned()),
                            unit: Some("month".to_owned()),
                            raw_unit: Some(Dimensionality::base_unit(BaseUnit::new("month"))),
                            ..Default::default()
                        },
                        weeks: list.next().expect("Unexpected end of iterator"),
                        days: list.next().expect("Unexpected end of iterator"),
                        hours: list.next().expect("Unexpected end of iterator"),
                        minutes: list.next().expect("Unexpected end of iterator"),
                        seconds: list.next().expect("Unexpected end of iterator"),
                    })))
                }
                Value::Number(n) => Ok(QueryReply::Number(n.to_parts(ctx))),
                Value::DateTime(d) => match d {
                    GenericDateTime::Fixed(d) => Ok(QueryReply::Date(DateReply::new(ctx, d))),
                    GenericDateTime::Timezone(d) => Ok(QueryReply::Date(DateReply::new(ctx, d))),
                },
                Value::Substance(s) => Ok(QueryReply::Substance(
                    s.to_reply(ctx).map_err(QueryError::generic)?,
                )),
            }
        }
        Query::Error(ref e) => Err(QueryError::generic(e.clone())),
    }
}
