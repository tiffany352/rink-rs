use chrono::{TimeZone, Utc};
use js_sys::Date;
use rink_core;
use rink_core::ast;
use rink_core::text_query;
use wasm_bindgen::prelude::*;

// Use `wee_alloc` as the global allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

pub fn set_panic_hook() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub struct Query {
    query: ast::Query,
}

#[wasm_bindgen]
impl Query {
    #[wasm_bindgen(constructor)]
    pub fn new(input: &str) -> Query {
        set_panic_hook();
        let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
        let query = text_query::parse_query(&mut iter);
        Query { query }
    }

    #[wasm_bindgen(js_name = getExpr)]
    pub fn get_expr(&self) -> JsValue {
        JsValue::from_serde(&self.query).unwrap()
    }
}

#[wasm_bindgen]
pub struct Context {
    context: rink_core::Context,
}

#[wasm_bindgen]
impl Context {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Context {
        set_panic_hook();
        let mut context = rink_core::simple_context().unwrap();
        // Will panic if this is set.
        context.use_humanize = false;
        Context { context }
    }

    #[wasm_bindgen(js_name = setTime)]
    pub fn set_time(&mut self, date: Date) {
        let year = date.get_utc_full_year();
        let month = date.get_utc_month();
        let day = date.get_utc_date();
        let hour = date.get_utc_hours();
        let min = date.get_utc_minutes();
        let sec = date.get_utc_seconds();
        let millis = date.get_utc_milliseconds();
        self.context.set_time(
            Utc.ymd(year as i32, month, day)
                .and_hms_milli(hour, min, sec, millis),
        );
    }

    #[wasm_bindgen]
    pub fn eval(&mut self, expr: &Query) -> JsValue {
        JsValue::from_serde(&self.context.eval_outer(&expr.query)).unwrap()
    }
}
