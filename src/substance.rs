// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use context::Context;
use number::{Number, Dim};
use num::Num;
use value::Show;
use std::collections::BTreeMap;
use reply::{PropertyReply, SubstanceReply};
use std::ops::{Mul, Div, Add};
use std::iter::once;
use std::rc::Rc;
use ast::Digits;

macro_rules! try_div {
    ($x:expr, $y:expr, $context:expr) => {
        (&$x / &$y).ok_or_else(|| {
            format!(
                "Division by zero: <{}> / <{}>",
                $x.show($context),
                $y.show($context)
            )
        })?
    };
}

#[derive(Debug, Clone)]
pub struct Property {
    pub input: Number,
    pub input_name: String,
    pub output: Number,
    pub output_name: String,
    pub doc: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Properties {
    pub name: String,
    pub properties: BTreeMap<String, Property>,
}

#[derive(Debug, Clone)]
pub struct Substance {
    pub amount: Number,
    pub properties: Rc<Properties>,
}

pub enum SubstanceGetError {
    Generic(String),
    Conformance(Number, Number),
}

impl Substance {
    pub fn rename(self, name: String) -> Substance {
        Substance {
            amount: self.amount,
            properties: Rc::new(Properties {
                name,
                properties: self.properties.properties.clone()
            })
        }
    }

    pub fn get(&self, name: &str) -> Result<Number, SubstanceGetError> {
        if self.amount.dimless() {
            self.properties.properties.get(name)
                .ok_or_else(|| SubstanceGetError::Generic(format!(
                    "No such property {} of {}",
                    name, self.properties.name)))
                .map(|prop| {
                    (&(&self.amount * &prop.output).unwrap() / &prop.input)
                        .expect("Non-zero property")
                })
        } else {
            for prop in self.properties.properties.values() {
                if name == prop.output_name {
                    let input = try!(
                        (&prop.input / &self.amount).ok_or_else(
                            || SubstanceGetError::Generic(
                                "Division by zero".to_owned())));
                    if input.dimless() {
                        let res = try!((&prop.output / &input).ok_or_else(
                            || SubstanceGetError::Generic(
                                "Division by zero".to_owned())));
                        return Ok(res)
                    } else {
                        return Err(SubstanceGetError::Conformance(
                            self.amount.clone(), prop.input.clone()))
                    }
                } else if name == prop.input_name {
                    let output = try!(
                        (&prop.output / &self.amount).ok_or_else(
                            || SubstanceGetError::Generic(
                                "Division by zero".to_owned())));
                    if output.dimless() {
                        let res = try!((&prop.input / &output).ok_or_else(
                            || SubstanceGetError::Generic(
                                "Division by zero".to_owned())));
                        return Ok(res)
                    } else {
                        return Err(SubstanceGetError::Conformance(
                            self.amount.clone(), prop.output.clone()))
                    }
                }
            }
            Err(SubstanceGetError::Generic(format!(
                "No such property {} of {}",
                name, self.properties.name
            )))
        }
    }

    /// Analogous to Context::show()
    pub fn get_in_unit(
        &self,
        unit: Number,
        context: &Context,
        bottom_name: BTreeMap<String, isize>,
        bottom_const: Num,
        base: u8,
        digits: Digits,
    ) -> Result<SubstanceReply, String> {
        if self.amount.dimless() {
            Ok(SubstanceReply {
                name: self.properties.name.clone(),
                doc: context.docs.get(&self.properties.name).cloned(),
                amount: self.amount.to_parts(context),
                properties: try!(self.properties.properties.iter().map(|(k, v)| {
                    let (input, output) = if v.input.dimless() {
                        let res = (&v.output * &self.amount).unwrap();
                        (None, try_div!(res, v.input, context))
                    } else {
                        (Some(v.input.clone()), v.output.clone())
                    };
                    let (input, output) = if output.unit != unit.unit {
                        if let Some(input) = input {
                            if input.unit == unit.unit {
                                (Some(output), input)
                            } else {
                                return Ok(None)
                            }
                        } else {
                            return Ok(None)
                        }
                    } else {
                        (input, output)
                    };
                    let output_show = context.show(
                        &try_div!(output, unit, context),
                        &unit,
                        bottom_name.clone(),
                        bottom_const.clone(),
                        base,
                        digits
                    ).value;
                    let output = try_div!(output, unit, context);
                    let input: Option<Number> = input;
                    Ok(Some(PropertyReply {
                        name: k.clone(),
                        value: if let Some(input) = input.as_ref() {
                            let input_pretty = input.prettify(context);
                            let mut output_pretty = output.clone();
                            output_pretty.unit = bottom_name.iter()
                                .map(|(k,v)| (Dim::new(&k), *v as i64)).collect();
                            let mut res = try_div!(output_pretty, input_pretty, context).to_parts(context);
                            let value = (&unit / input)
                                .expect("Already known safe").to_parts(context);
                            res.quantity = value.quantity;
                            res
                        } else {
                            output_show.clone()
                        },
                        doc: v.doc.clone()
                    }))
                }).filter_map(
                    |x| x.map(|x| x.map(Ok)).unwrap_or_else(|e| Some(Err(e)))
                ).collect::<Result<Vec<PropertyReply>, String>>()),
            })
        } else {
            let func = |(_k, v): (&String, &Property)| {
                let input = try_div!(v.input, self.amount, context);
                let output = try_div!(v.output, self.amount, context);
                let (name, input, output) = if input.dimless() {
                    if v.output.unit != unit.unit {
                        return Ok(None)
                    }
                    let div = try_div!(v.output, input, context);
                    (v.output_name.clone(), None, div)
                } else if output.dimless() {
                    if v.input.unit != unit.unit {
                        return Ok(None)
                    }
                    let div = try_div!(v.input, output, context);
                    (v.input_name.clone(), None, div)
                } else {
                    return Ok(None)
                };
                let output_show = context.show(
                    &try_div!(output, unit, context),
                    &unit,
                    bottom_name.clone(),
                    bottom_const.clone(),
                    base,
                    digits
                ).value;
                let output = try_div!(output, unit, context);
                let input: Option<Number> = input;
                Ok(Some(PropertyReply {
                    name,
                    value: if let Some(input) = input.as_ref() {
                        let input_pretty = input.prettify(context);
                        let mut output_pretty = output.clone();
                        output_pretty.unit = bottom_name.iter()
                            .map(|(k,v)| (Dim::new(&k), *v as i64)).collect();
                        let mut res = try_div!(output_pretty, input_pretty, context).to_parts(context);
                        let value = (&unit / input)
                            .expect("Already known safe").to_parts(context);
                        res.quantity = value.quantity;
                        res
                    } else {
                        output_show.clone()
                    },
                    doc: v.doc.clone(),
                }))
            };
            let amount = PropertyReply {
                name: self.amount.to_parts(context).quantity
                    .unwrap_or_else(|| "amount".to_owned()),
                value: self.amount.to_parts(context),
                doc: None,
            };
            Ok(SubstanceReply {
                name: self.properties.name.clone(),
                doc: context.docs.get(&self.properties.name).cloned(),
                amount: self.amount.to_parts(context),
                properties: try!(
                    once(Ok(Some(amount)))
                        .chain(self.properties.properties.iter().map(func))
                        .collect::<Result<Vec<Option<PropertyReply>>, String>>())
                    .into_iter()
                    .filter_map(|x| x)
                    .collect(),
            })
        }
    }

    pub fn to_reply(&self, context: &Context) -> Result<SubstanceReply, String> {
        if self.amount.dimless() {
            Ok(SubstanceReply {
                name: self.properties.name.clone(),
                doc: context.docs.get(&self.properties.name).cloned(),
                amount: self.amount.to_parts(context),
                properties: try!(self.properties.properties.iter().map(|(k, v)| {
                    let (input, output) = if v.input.dimless() {
                        let res = (&v.output * &self.amount).unwrap();
                        (None, try_div!(res, v.input, context))
                    } else {
                        (Some(v.input.clone()), v.output.clone())
                    };
                    Ok(PropertyReply {
                        name: k.clone(),
                        value: if let Some(input) = input.as_ref() {
                            let input_pretty = input.prettify(context);
                            let output_pretty = output.prettify(context);
                            let mut res = try_div!(output_pretty, input_pretty, context).to_parts(context);
                            let value = (&output / input)
                                .expect("Already known safe").to_parts(context);
                            res.quantity = value.quantity;
                            res
                        } else {
                            output.to_parts(context)
                        },
                        doc: v.doc.clone()
                    })
                }).collect::<Result<Vec<PropertyReply>, String>>()),
            })
        } else {
            let func = |(_k, v): (&String, &Property)| {
                let input = try_div!(v.input, self.amount, context);
                let output = try_div!(v.output, self.amount, context);
                let (name, input, output) = if input.dimless() {
                    let div = try_div!(v.output, input, context);
                    (v.output_name.clone(), None, div)
                } else if output.dimless() {
                    let div = try_div!(v.input, output, context);
                    (v.input_name.clone(), None, div)
                } else {
                    return Ok(None)
                };
                let input: Option<Number> = input;
                Ok(Some(PropertyReply {
                    name,
                    value: if let Some(input) = input.as_ref() {
                        let input_pretty = input.prettify(context);
                        let output_pretty = output.prettify(context);
                        let mut res = try_div!(output_pretty, input_pretty, context).to_parts(context);
                        let value = (&output / input)
                            .expect("Already known safe").to_parts(context);
                        res.quantity = value.quantity;
                        res
                    } else {
                        output.to_parts(context)
                    },
                    doc: v.doc.clone(),
                }))
            };
            let amount = PropertyReply {
                name: self.amount.to_parts(context).quantity
                    .unwrap_or_else(|| "amount".to_owned()),
                value: self.amount.to_parts(context),
                doc: None,
            };
            Ok(SubstanceReply {
                name: self.properties.name.clone(),
                doc: context.docs.get(&self.properties.name).cloned(),
                amount: self.amount.to_parts(context),
                properties: try!(
                    once(Ok(Some(amount)))
                        .chain(self.properties.properties.iter().map(func))
                        .collect::<Result<Vec<Option<PropertyReply>>, String>>())
                    .into_iter()
                    .filter_map(|x| x)
                    .collect(),
            })
        }
    }
}

impl Show for Substance {
    fn show(&self, context: &Context) -> String {
        format!("{} {}", self.amount.to_parts(context).format("n u p"), self.properties.name)
    }
}

impl<'a, 'b> Mul<&'b Number> for &'a Substance {
    type Output = Result<Substance, String>;

    fn mul(self, other: &'b Number) -> Self::Output {
        Ok(Substance {
            amount: try!((&self.amount * other).ok_or_else(
                || "Multiplication of numbers should not fail".to_owned())),
            properties: self.properties.clone(),
        })
    }
}

impl<'a, 'b> Div<&'b Number> for &'a Substance {
    type Output = Result<Substance, String>;

    fn div(self, other: &'b Number) -> Self::Output {
        Ok(Substance {
            amount: try!((&self.amount / other).ok_or_else(
                || "Division by zero".to_owned())),
            properties: self.properties.clone(),
        })
    }
}

impl<'a, 'b> Add<&'b Substance> for &'a Substance {
    type Output = Result<Substance, String>;

    fn add(self, other: &'b Substance) -> Self::Output {
        let res = Substance {
            amount: Number::one(),
            properties: Rc::new(Properties {
                name: format!(
                    "{} {} + {} {}",
                    self.amount.to_parts_simple().format("n u"),
                    self.properties.name,
                    other.amount.to_parts_simple().format("n u"),
                    other.properties.name,
                ),
                properties: self.properties.properties.iter().filter_map(|(k, prop1)| {
                    let prop2 = match other.properties.properties.get(k) {
                        Some(v) => v,
                        None => return None
                    };
                    let mol = Number::one_unit(Dim::new("mol"));
                    if
                        prop1.input_name != prop2.input_name ||
                        prop1.output_name != prop2.output_name ||
                        prop1.input.unit != prop2.input.unit ||
                        prop1.output.unit != prop2.output.unit ||
                        prop1.input != mol ||
                        prop2.input != mol
                    {
                        return None
                    }
                    Some((k.clone(), Property {
                        output: (
                            &(&self.amount * &prop1.output).unwrap() +
                                &(&other.amount * &prop2.output).unwrap()
                        ).expect("Add"),
                        input_name: prop1.input_name.clone(),
                        input: mol,
                        output_name: prop1.output_name.clone(),
                        doc: None,
                    }))
                }).collect()
            })
        };
        if res.properties.properties.is_empty() {
            Err("No shared properties".to_string())
        } else {
            Ok(res)
        }
    }
}
