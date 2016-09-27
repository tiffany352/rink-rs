// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use context::Context;
use number::Number;
use value::Show;
use std::collections::BTreeMap;
use reply::{PropertyReply, SubstanceReply};

#[derive(Debug, Clone)]
pub struct Property {
    pub input: Number,
    pub output: Number,
    pub doc: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Substance {
    pub properties: BTreeMap<String, Property>,
}

impl Substance {
    pub fn to_reply(&self, context: &Context) -> Result<SubstanceReply, String> {
        Ok(SubstanceReply {
            properties: try!(self.properties.iter().map(|(k, x)| {
                let (input, output) = if x.input.1.len() == 0 {
                    let div = try!(
                        (&x.output / &x.input).ok_or_else(|| {
                            format!(
                                "Division by zero: <{}> / <{}>",
                                x.output.show(context),
                                x.input.show(context)
                            )
                        })
                    );
                    (None, div.to_parts(context))
                } else {
                    (Some(x.input.to_parts(context)),
                     x.output.to_parts(context))
                };
                Ok(PropertyReply {
                    name: k.clone(),
                    input: input,
                    output: output,
                    doc: x.doc.clone(),
                })
            }).collect::<Result<Vec<PropertyReply>, String>>()),
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
