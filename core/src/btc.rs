// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::ast::{Def, DefEntry, Defs, Expr};
use json;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;
use std::time::Duration;

static URL: &str = "https://blockchain.info/stats?format=json";

pub fn parse(mut f: File) -> Result<Defs, String> {
    let mut buf = String::new();
    f.read_to_string(&mut buf).map_err(|x| x.to_string())?;
    let parsed = json::parse(&*buf).map_err(|x| x.to_string())?;
    let mut out = vec![];
    if let Some(price) = parsed["market_price_usd"].as_number() {
        let (sign, mantissa, exp) = price.as_parts();
        let integer = format!("{}{}", if sign { "" } else { "-" }, mantissa);
        if let Ok(price) = crate::Number::from_parts(&*integer, None, Some(&*exp.to_string())) {
            out.push(DefEntry {
                name: "BTC".to_owned(),
                def: Rc::new(Def::Unit(Expr::Mul(vec![
                    Expr::Const(price),
                    Expr::Unit("USD".to_owned()),
                ]))),
                doc: Some("Sourced from blockchain.info.".to_string()),
                category: Some("currencies".to_owned()),
            });
        }
    }
    Ok(Defs { defs: out })
}

pub fn load() -> Result<Defs, String> {
    crate::cached("btc.json", URL, Duration::from_secs(3 * 60 * 60)).and_then(parse)
}
