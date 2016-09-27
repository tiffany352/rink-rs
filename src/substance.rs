// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use context::Context;
use number::Number;
use value::Show;
use std::collections::BTreeMap;
use reply::{PropertyReply, SubstanceReply};
use std::ops::{Mul, Div};
use std::iter::once;

#[derive(Debug, Clone)]
pub struct Property {
    pub input: Number,
    pub input_name: String,
    pub output: Number,
    pub output_name: String,
    pub doc: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Substance {
    pub amount: Number,
    pub properties: BTreeMap<String, Property>,
}

impl Substance {
    pub fn to_reply(&self, context: &Context) -> Result<SubstanceReply, String> {
        Ok(SubstanceReply {
            properties: try!(
                once(Ok(PropertyReply {
                    name: "amount".to_owned(),
                    input: None,
                    output: self.amount.to_parts(context),
                    doc: None,
                })).chain(self.properties.iter().map(|(k, x)| {
                    let input = try!((&x.input / &self.amount).ok_or_else(|| format!(
                        "Division by zero: <{}> / <{}>",
                        x.input.show(context),
                        self.amount.show(context)
                    )));
                    let (name, input, output) = if input.1.len() == 0 {
                        let div = try!(
                            (&x.output / &input).ok_or_else(|| {
                                format!(
                                    "Division by zero: <{}> / <{}>",
                                    x.output.show(context),
                                    input.show(context)
                                )
                            })
                        );
                        (x.output_name.clone(), None, div.to_parts(context))
                    } else {
                        (k.clone(), Some(x.input.to_parts(context)),
                         x.output.to_parts(context))
                    };
                    Ok(PropertyReply {
                        name: name,
                        input: input,
                        output: output,
                        doc: x.doc.clone(),
                    })
                })).collect::<Result<Vec<PropertyReply>, String>>()
            ),
        })
    }
}

impl Show for Substance {
    fn show(&self, context: &Context) -> String {
        match self.to_reply(context) {
            Ok(v) => format!("{}", v),
            Err(e) => e
        }
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
