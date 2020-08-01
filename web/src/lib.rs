use rink_core;
use wasm_bindgen::prelude::*;

// Use `wee_alloc` as the global allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn exec_query(query: &str) -> String {
    let mut context = rink_core::Context::new();
    match rink_core::one_line(&mut context, query) {
        Ok(value) => value,
        Err(value) => value,
    }
}
