use rink_core;
use rink_core::ast::Query;
use rink_core::reply::QueryReply;
use rink_core::text_query;
use wasm_bindgen::prelude::*;

// Use `wee_alloc` as the global allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn exec_query(query: &str) -> String {
    let mut context = rink_core::simple_context().unwrap();
    match rink_core::one_line(&mut context, query) {
        Ok(value) => value,
        Err(value) => value,
    }
}

#[wasm_bindgen]
pub struct Statement {
    query: Query,
}

#[wasm_bindgen]
impl Statement {
    #[wasm_bindgen(constructor)]
    pub fn new(input: &str) -> Statement {
        let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
        let query = text_query::parse_query(&mut iter);
        Statement { query }
    }

    #[wasm_bindgen(js_name = getExpr)]
    pub fn get_expr(&self) -> JsValue {
        JsValue::from_serde(&self.query).unwrap()
    }
}

#[wasm_bindgen]
pub fn parse_expr(input: &str) -> Query {}

#[wasm_bindgen]
pub struct Session {
    context: rink_core::Context,
}

#[wasm_bindgen]
impl Session {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Session {
        Session {
            context: rink_core::Context::new(),
        }
    }
}

#[wasm_bindgen]
pub fn eval_expr(session: &mut Session, expr: Query) -> QueryReply {
    ctx.eval_outer(&expr)
}
