// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use assert_json_diff::assert_json_eq;
use rink_core::*;
use serde_json;
use serde_json::json;
use serde_json::Value;

fn to_json(input: &str) -> Value {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    serde_json::to_value(&expr).unwrap()
}

#[test]
fn check_simple() {
    assert_json_eq!(
        to_json("1 + 2"),
        json!({
            "type": "expr",
            "value": {
                "type": "binop",
                "op": "add",
                "left": {
                    "type": "const",
                    "value": "1",
                },
                "right": {
                    "type": "const",
                    "value": "2",
                },
            }
        })
    )
}
