// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::ast::{Def, DefEntry, Defs, Expr};
use crate::numeric::Numeric;
use std::io::Read;
use std::rc::Rc;
use xml::reader::XmlEvent;
use xml::EventReader;

pub static URL: &str = "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml";

pub fn parse<R: Read>(reader: R) -> Result<Defs, String> {
    let reader = EventReader::new(reader);
    let mut out = vec![];
    for ev in reader {
        match ev {
            Ok(XmlEvent::StartElement {
                ref name,
                attributes: ref attrs,
                ..
            }) if name.local_name == "Cube" => {
                let mut currency = None;
                let mut rate = None;
                for attr in attrs {
                    match &*attr.name.local_name {
                        "currency" => currency = Some(&*attr.value),
                        "rate" => rate = Some(&*attr.value),
                        _ => (),
                    }
                }
                if let (Some(currency), Some(rate)) = (currency, rate) {
                    let mut iter = rate.split('.');
                    let integer = iter.next().unwrap();
                    let frac = iter.next();
                    if let Ok(num) = crate::number::Number::from_parts(integer, frac, None) {
                        out.push(DefEntry {
                            name: currency.to_owned(),
                            def: Rc::new(Def::Unit(Expr::new_mul(vec![
                                Expr::new_frac(
                                    Expr::new_const(Numeric::one()),
                                    Expr::new_const(num),
                                ),
                                Expr::new_unit("EUR".to_string()),
                            ]))),
                            doc: Some("Sourced from European Central Bank.".to_string()),
                            category: Some("currencies".to_owned()),
                        });
                    }
                }
            }
            Err(e) => return Err(e.to_string()),
            _ => (),
        }
    }
    Ok(Defs { defs: out })
}
