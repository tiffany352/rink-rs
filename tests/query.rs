extern crate rink;

use rink::*;

thread_local! {
    static CONTEXT: Context = load().unwrap();
}

fn test(input: &str, output: &str) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_expr(&mut iter);
    CONTEXT.with(|ctx| {
        let res = ctx.eval_outer(&expr);
        assert_eq!(res.as_ref().map(|x| x.as_ref()), Ok(output));
    });
}

#[test]
fn test_queries() {
    test("watt", "Definition: watt = J / s = kg m^2 / s^3 (power)");
    test("5 inch", "0.127 m (length)");
    test("5 inch -> cm", "12.7 cm (length)");
}
