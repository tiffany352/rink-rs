// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use assert_json_diff::assert_json_eq;
use rink_core::ast::{Def, DefEntry, Expr, ExprString, Property, Query};
use rink_core::*;
use serde_json;
use serde_json::{json, to_value};

fn expr(input: &str) -> Expr {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_expr(&mut iter);
    expr
}

fn query(input: &str) -> Query {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let query = text_query::parse_query(&mut iter);
    query
}

#[test]
fn check_simple() {
    assert_json_eq!(
        to_value(query("1 + 2")).unwrap(),
        json!({
            "type": "expr",
            "value": {
                "type": "binop",
                "op": "add",
                "left": {
                    "type": "const",
                    "value": {
                        "numer": "1",
                        "denom": "1",
                        "exactValue": "1",
                        "approxValue": null
                    },
                },
                "right": {
                    "type": "const",
                    "value": {
                        "numer": "2",
                        "denom": "1",
                        "exactValue": "2",
                        "approxValue": null
                    },
                },
            }
        })
    )
}

#[test]
fn check_defs() {
    // Straightforward case with a unit def.
    assert_json_eq!(
        serde_json::to_value(DefEntry::new_unit(
            "horsepower",
            Some("unit of horses"),
            Some("some_category"),
            expr("760 watt")
        ))
        .unwrap(),
        json!({
            "name": "horsepower",
            "doc": "unit of horses",
            "category": "some_category",
            "type": "unit",
            "expr": "760 watt"
        })
    );

    // Prefix and SPrefix.
    assert_json_eq!(
        serde_json::to_value([
            DefEntry::new(
                "kilo",
                None,
                None,
                Def::SPrefix {
                    expr: ExprString(expr("1000"))
                },
            ),
            DefEntry::new(
                "k",
                None,
                None,
                Def::Prefix {
                    expr: ExprString(expr("kilo"))
                }
            )
        ])
        .unwrap(),
        json!([
            {
                "name": "kilo",
                "doc": null,
                "category": null,
                "type": "sprefix",
                "expr": "1000"
            },
            {
                "name": "k",
                "doc": null,
                "category": null,
                "type": "prefix",
                "expr": "kilo"
            }
        ])
    );

    // Base units.
    assert_json_eq!(
        serde_json::to_value([
            DefEntry::new("m", Some("base unit of length"), None, Def::Dimension),
            DefEntry::new(
                "meter",
                None,
                None,
                Def::Canonicalization { of: "m".to_owned() }
            )
        ])
        .unwrap(),
        json!([
            {
                "name": "m",
                "doc": "base unit of length",
                "category": null,
                "type": "dimension"
            },
            {
                "name": "meter",
                "doc": null,
                "category": null,
                "type": "canonicalization",
                "of": "m"
            }
        ])
    );

    // Substances
    assert_json_eq!(
        serde_json::to_value(DefEntry::new(
            "water",
            Some("dihydrogen monoxide"),
            None,
            Def::Substance {
                symbol: Some("H2O".to_owned()),
                properties: vec![Property {
                    doc: None,
                    name: "density".to_owned(),
                    input: ExprString(expr("gram")),
                    input_name: "mass".to_owned(),
                    output: ExprString(expr("cm^3")),
                    output_name: "volume".to_owned(),
                }],
            }
        ))
        .unwrap(),
        json!({
            "name": "water",
            "doc": "dihydrogen monoxide",
            "category": null,
            "type": "substance",
            "symbol": "H2O",
            "properties": [{
                "name": "density",
                "doc": null,
                "input": "gram",
                "inputName": "mass",
                "output": "cm^3",
                "outputName": "volume"
            }]
        })
    );
}
