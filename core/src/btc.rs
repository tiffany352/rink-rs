// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::ast::{DefEntry, Defs, Expr};
use json;

pub static URL: &str = "https://blockchain.info/stats?format=json";

pub fn parse(buf: String) -> Result<Defs, String> {
    let parsed = json::parse(&*buf).map_err(|x| x.to_string())?;
    let mut out = vec![];
    if let Some(price) = parsed["market_price_usd"].as_number() {
        let (sign, mantissa, exp) = price.as_parts();
        let integer = format!("{}{}", if sign { "" } else { "-" }, mantissa);
        if let Ok(price) = crate::Number::from_parts(&*integer, None, Some(&*exp.to_string())) {
            out.push(DefEntry::new_unit(
                "BTC",
                Some("Sourced from blockchain.info"),
                Some("currencies"),
                Expr::new_mul(vec![
                    Expr::new_const(price),
                    Expr::new_unit("USD".to_owned()),
                ]),
            ));
        }
    }
    Ok(Defs { defs: out })
}
