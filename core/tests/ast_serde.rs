// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use assert_json_diff::assert_json_eq;
use rink_core::ast::{Def, DefEntry, Expr, ExprString, Property, Query};
use rink_core::parsing::text_query;
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

    // Prefixes
    assert_json_eq!(
        serde_json::to_value([
            DefEntry::new(
                "kilo",
                None,
                None,
                Def::Prefix {
                    expr: ExprString(expr("1000")),
                    is_long: true,
                },
            ),
            DefEntry::new(
                "k",
                None,
                None,
                Def::Prefix {
                    expr: ExprString(expr("kilo")),
                    is_long: false,
                }
            )
        ])
        .unwrap(),
        json!([
            {
                "name": "kilo",
                "doc": null,
                "category": null,
                "type": "prefix",
                "isLong": true,
                "expr": "1000"
            },
            {
                "name": "k",
                "doc": null,
                "category": null,
                "type": "prefix",
                "isLong": false,
                "expr": "kilo"
            }
        ])
    );

    // Base units.
    assert_json_eq!(
        serde_json::to_value([DefEntry::new(
            "m",
            Some("base unit of length"),
            None,
            Def::BaseUnit {
                long_name: Some("meter".to_owned())
            }
        ),])
        .unwrap(),
        json!([
            {
                "name": "m",
                "longName": "meter",
                "doc": "base unit of length",
                "category": null,
                "type": "baseUnit"
            }
        ])
    );

    // Quantities
    assert_json_eq!(
        serde_json::to_value(DefEntry::new(
            "watt",
            Some("SI derived unit for power"),
            None,
            Def::Quantity {
                expr: ExprString(expr("energy / time"))
            }
        ))
        .unwrap(),
        json!({
            "name": "watt",
            "doc": "SI derived unit for power",
            "category": null,
            "type": "quantity",
            "expr": "energy / time",
        })
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

    // Categories
    assert_json_eq!(
        serde_json::to_value(DefEntry::new(
            "cool_beans",
            Some("Units that are cool beans."),
            None,
            Def::Category {
                display_name: "Cool Beans".to_owned(),
            }
        ))
        .unwrap(),
        json!({
            "name": "cool_beans",
            "doc": "Units that are cool beans.",
            "category": null,
            "type": "category",
            "displayName": "Cool Beans",
        })
    );

    // Errors
    assert_json_eq!(
        serde_json::to_value(DefEntry::new(
            "foo",
            Some("Definition of foo"),
            None,
            Def::Error {
                message: "Syntax error".to_owned()
            }
        ))
        .unwrap(),
        json!({
            "name": "foo",
            "doc": "Definition of foo",
            "category": null,
            "type": "error",
            "message": "Syntax error",
        })
    );
}
