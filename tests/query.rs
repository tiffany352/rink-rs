extern crate rink;

use rink::*;

thread_local! {
    static CONTEXT: Context = load().unwrap();
}

fn test(input: &str, output: &str) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        let res = ctx.eval_outer(&expr);
        assert_eq!(res.as_ref().map(|x| x.as_ref()), Ok(output));
    });
}

#[test]
fn test_definition() {
    test("watt", "Definition: watt = J / s = 1 watt (power; kg m^2 / s^3)");
}

#[test]
fn test_eval() {
    test("5 inch", "0.127 m (length)");
}

#[test]
fn test_convert() {
    test("5 inch -> cm", "12.7 centim (length)");
}

#[test]
fn test_temp() {
    test("2 degC 2 -> degC", "277.15 °C (temperature)");
}

#[test]
fn test_determinism() {
    test("pascal m", "1 A tesla (spectral_irradiance_frequency)");
}
