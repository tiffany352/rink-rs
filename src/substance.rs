// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use context::Context;
use number::{Number, Num};
use value::Show;
use std::collections::BTreeMap;
use reply::{PropertyReply, SubstanceReply};
use std::ops::{Mul, Div};
use std::iter::once;
use std::rc::Rc;

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
    pub fn get(&self, name: &str) -> Result<Number, SubstanceGetError> {
        if self.amount.1.len() == 0 {
            self.properties.properties.get(name)
                .ok_or_else(|| SubstanceGetError::Generic(format!(
                    "No such property {} of {}",
                    name, self.properties.name)))
                .map(|prop| {
                    (&(&self.amount * &prop.output).unwrap() / &prop.input)
                        .expect("Non-zero property")
                })
        } else {
            for (_name, prop) in &self.properties.properties {
                if name == prop.output_name {
                    let input = try!(
                        (&prop.input / &self.amount).ok_or_else(
                            || SubstanceGetError::Generic(
                                "Division by zero".to_owned())));
                    if input.1.len() == 0 {
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
                    if output.1.len() == 0 {
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
                name, self.properties.name)))
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
    ) -> Result<SubstanceReply, String> {
        if self.amount.1.len() == 0 {
            Ok(SubstanceReply {
                properties: try!(self.properties.properties.iter().map(|(k, v)| {
                    let (input, output) = if v.input.1.len() == 0 {
                        let res = (&v.output * &self.amount).unwrap();
                        (None, try!((&res / &v.input)
                         .ok_or_else(|| format!(
                             "Division by zero: <{}> / <{}>",
                             res.show(context),
                             v.input.show(context)
                         ))))
                    } else {
                        (Some(v.input.clone()), v.output.clone())
                    };
                    if output.1 != unit.1 {
                        return Ok(None)
                    }
                    Ok(Some(PropertyReply {
                        name: k.clone(),
                        input: input.map(|x| x.to_parts(context)),
                        output: context.show(
                            &try!((
                                &output / &unit
                            ).ok_or_else(|| format!(
                                "Division by zero: <{}> / <{}>",
                                output.show(context),
                                unit.show(context)
                            ))),
                            &unit,
                            bottom_name.clone(),
                            bottom_const.clone(),
                            base
                        ).value,
                        doc: v.doc.clone()
                    }))
                }).filter_map(
                    |x| x.map(|x| x.map(Ok)).unwrap_or_else(|e| Some(Err(e)))
                ).collect::<Result<Vec<PropertyReply>, String>>()),
            })
        } else {
            let func = |(_k, v): (&String, &Property)| {
                let input = try!((&v.input / &self.amount).ok_or_else(|| format!(
                    "Division by zero: <{}> / <{}>",
                    v.input.show(context),
                    self.amount.show(context)
                )));
                let output = try!((&v.output / &self.amount).ok_or_else(|| format!(
                    "Division by zero: <{}> / <{}>",
                    v.output.show(context),
                    self.amount.show(context)
                )));
                let (name, input, output) = if input.1.len() == 0 {
                    let div = try!(
                        (&v.output / &input).ok_or_else(|| format!(
                            "Division by zero: <{}> / <{}>",
                            v.output.show(context),
                            input.show(context)
                        ))
                    );
                    (v.output_name.clone(), None, div)
                } else if output.1.len() == 0 {
                    let div = try!(
                        (&v.input / &output).ok_or_else(|| format!(
                            "Division by zero: <{}> / <{}>",
                            v.input.show(context),
                            output.show(context)
                        ))
                    );
                    (v.input_name.clone(), None, div)
                } else {
                    return Ok(None)
                };
                Ok(Some(PropertyReply {
                    name: name,
                    input: input.map(|x| context.show(
                        &x, &unit,
                        bottom_name.clone(),
                        bottom_const.clone(),
                        base
                    ).value),
                    output: context.show(
                        &output, &unit,
                        bottom_name.clone(),
                        bottom_const.clone(),
                        base
                    ).value,
                    doc: v.doc.clone(),
                }))
            };
            let amount = PropertyReply {
                name: self.amount.to_parts(context).quantity
                    .unwrap_or_else(|| "amount".to_owned()),
                input: None,
                output: self.amount.to_parts(context),
                doc: None,
            };
            Ok(SubstanceReply {
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
        if self.amount.1.len() == 0 {
            Ok(SubstanceReply {
                properties: try!(self.properties.properties.iter().map(|(k, v)| {
                    let (input, output) = if v.input.1.len() == 0 {
                        let res = (&v.output * &self.amount).unwrap();
                        (None, try!((&res / &v.input)
                         .ok_or_else(|| format!(
                             "Division by zero: <{}> / <{}>",
                             res.show(context),
                             v.input.show(context)
                         ))))
                    } else {
                        (Some(v.input.clone()), v.output.clone())
                    };
                    Ok(PropertyReply {
                        name: k.clone(),
                        input: input.map(|x| x.to_parts(context)),
                        output: output.to_parts(context),
                        doc: v.doc.clone()
                    })
                }).collect::<Result<Vec<PropertyReply>, String>>()),
            })
        } else {
            let func = |(_k, v): (&String, &Property)| {
                let input = try!((&v.input / &self.amount).ok_or_else(|| format!(
                    "Division by zero: <{}> / <{}>",
                    v.input.show(context),
                    self.amount.show(context)
                )));
                let output = try!((&v.output / &self.amount).ok_or_else(|| format!(
                    "Division by zero: <{}> / <{}>",
                    v.output.show(context),
                    self.amount.show(context)
                )));
                let (name, input, output) = if input.1.len() == 0 {
                    let div = try!(
                        (&v.output / &input).ok_or_else(|| format!(
                            "Division by zero: <{}> / <{}>",
                            v.output.show(context),
                            input.show(context)
                        ))
                    );
                    (v.output_name.clone(), None, div.to_parts(context))
                } else if output.1.len() == 0 {
                    let div = try!(
                        (&v.input / &output).ok_or_else(|| format!(
                            "Division by zero: <{}> / <{}>",
                            v.input.show(context),
                            output.show(context)
                        ))
                    );
                    (v.input_name.clone(), None, div.to_parts(context))
                } else {
                    return Ok(None)
                };
                Ok(Some(PropertyReply {
                    name: name,
                    input: input,
                    output: output,
                    doc: v.doc.clone(),
                }))
            };
            let amount = PropertyReply {
                name: self.amount.to_parts(context).quantity
                    .unwrap_or_else(|| "amount".to_owned()),
                input: None,
                output: self.amount.to_parts(context),
                doc: None,
            };
            Ok(SubstanceReply {
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
