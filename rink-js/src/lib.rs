// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use js_sys::Date;
use rink_core::ast;
use rink_core::output::fmt::{FmtToken, Span, TokenFmt};
use rink_core::output::QueryReply;
use rink_core::parsing::text_query;
use rink_core::types::DateTime;
use serde_derive::*;
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

/// Wrapper around Result because serde produces ugly output by default.
#[derive(Serialize, Debug)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "result")]
enum Success<Ok, Err> {
    Ok(Ok),
    Err(Err),
}

impl<Ok, Err> From<Result<Ok, Err>> for Success<Ok, Err> {
    fn from(result: Result<Ok, Err>) -> Success<Ok, Err> {
        match result {
            Ok(value) => Success::Ok(value),
            Err(value) => Success::Err(value),
        }
    }
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
        serde_wasm_bindgen::to_value(&self.query).unwrap()
    }
}

#[wasm_bindgen]
pub struct Context {
    context: rink_core::Context,
}

#[derive(Serialize, Debug)]
struct Token {
    pub text: String,
    pub fmt: FmtToken,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
#[serde(rename_all = "lowercase")]
enum SpanOrList {
    Span(Token),
    List { children: Vec<SpanOrList> },
}

fn visit_tokens(spans: &[Span]) -> Vec<SpanOrList> {
    spans
        .iter()
        .map(|span| match span {
            Span::Content { text, token } => SpanOrList::Span(Token {
                text: text.to_string(),
                fmt: *token,
            }),
            Span::Child(child) => SpanOrList::List {
                children: visit_tokens(&child.to_spans()),
            },
        })
        .collect()
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

    #[wasm_bindgen(js_name = setSavePreviousResult)]
    pub fn set_save_previous_result(&mut self, value: bool) {
        self.context.save_previous_result = value;
    }

    #[wasm_bindgen(js_name = setTime)]
    pub fn set_time(&mut self, date: Date) {
        self.context
            .set_time(DateTime::from_millis_local(date.value_of() as i64));
    }

    #[wasm_bindgen(js_name = loadCurrency)]
    pub fn load_currency(&mut self, live_defs: String) -> Result<(), JsValue> {
        let base_defs = rink_core::CURRENCY_FILE.unwrap();
        self.context.load_currency(&live_defs, base_defs)?;

        Ok(())
    }

    #[wasm_bindgen]
    pub fn eval(&mut self, expr: &Query) -> JsValue {
        let value = self.context.eval_query(&expr.query);
        if self.context.save_previous_result {
            if let Ok(QueryReply::Number(ref number_parts)) = value {
                if let Some(ref raw) = number_parts.raw_value {
                    self.context.previous_result = Some(raw.clone());
                }
            }
        }
        let value = Success::from(value);
        match serde_wasm_bindgen::to_value(&value) {
            Ok(value) => value,
            Err(err) => format!("Failed to serialize: {}\n{:#?}", err, value).into(),
        }
    }

    #[wasm_bindgen]
    pub fn eval_tokens(&mut self, expr: &Query) -> JsValue {
        let value = self.context.eval_query(&expr.query);
        if self.context.save_previous_result {
            if let Ok(QueryReply::Number(ref number_parts)) = value {
                if let Some(ref raw) = number_parts.raw_value {
                    self.context.previous_result = Some(raw.clone());
                }
            }
        }
        let spans = value.to_spans();
        let tokens = visit_tokens(&spans);

        match serde_wasm_bindgen::to_value(&tokens) {
            Ok(value) => value,
            Err(err) => format!("Failed to serialize: {}\n{:#?}", err, tokens).into(),
        }
    }
}

#[wasm_bindgen]
pub fn version() -> String {
    rink_core::version().to_owned()
}
