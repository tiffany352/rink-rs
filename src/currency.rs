// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::fs::File;
use std::time::Duration;
use xml::EventReader;
use xml::reader::XmlEvent;
use ast::{Defs, Def, Expr, DefEntry};
use std::rc::Rc;
use num::Num;

static URL: &'static str = "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml";

pub fn parse(f: File) -> Result<Defs, String> {
    let reader = EventReader::new(f);
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
                        "currency" =>
                            currency = Some(&*attr.value),
                        "rate" =>
                            rate = Some(&*attr.value),
                        _ => (),
                    }
                }
                if let (Some(currency), Some(rate)) = (currency, rate) {
                    let mut iter = rate.split('.');
                    let integer = iter.next().unwrap();
                    let frac = iter.next();
                    if let Ok(num) = ::number::Number::from_parts(integer, frac, None) {
                        out.push(DefEntry {
                            name: currency.to_owned(),
                            def: Rc::new(Def::Unit(
                                Expr::Mul(vec![
                                    Expr::Frac(Box::new(Expr::Const(Num::one())),
                                               Box::new(Expr::Const(num))),
                                    Expr::Unit("EUR".to_string())
                                ]))),
                            doc: Some("Sourced from European Central Bank.".to_string()),
                            category: Some("currencies".to_owned()),
                        });
                    }
                }
            },
            Err(e) => return Err(e.to_string()),
            _ => (),
        }
    }
    Ok(Defs {
        defs: out
    })
}

pub fn load() -> Result<Defs, String> {
    ::cached("currency.xml", URL, Duration::from_secs(23*60*60)).and_then(parse)
}
