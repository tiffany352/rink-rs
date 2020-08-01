//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use rink_web::{Context, Query};
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn pass() {
    assert_eq!(1 + 1, 2);
}

#[wasm_bindgen_test]
fn check_query() {
    let query = Query::new("3 feet to meters");
    let mut context = Context::new();
    let result = context.eval(&query);
    assert_eq!(result, "0.9144 meter (length)");
}
