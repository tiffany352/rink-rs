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
        let res = match res {
            Ok(v) => v,
            Err(v) => v
        };
        assert_eq!(res, output);
    });
}

#[test]
fn test_definition() {
    test("watt", "Definition: watt = J / s = 1 watt (power; kg m^2 / s^3)");
}

#[test]
fn test_eval() {
    test("5 inch", "127 millimeter (length)");
}

#[test]
fn test_convert() {
    test("5 inch -> cm", "12.7 centimeter (length)");
}

#[test]
fn test_temp() {
    test("2 degC 2 -> degC", "277.15 °C (temperature)");
}

#[test]
fn test_determinism() {
    test("weber / m", "1 meter tesla");
}

#[test]
fn test_sqrt_errors() {
    test("sqrt -1",
         "Complex numbers are not implemented: sqrt <-1 (dimensionless)>");
    test("sqrt(2m)",
         "Result must have integer dimensions: sqrt <2 meter (length)>");
}

#[test]
fn test_number_regress() {
    test("953 mega",
         "9.53e8 (dimensionless)");
}

#[test]
fn test_lookup() {
    test("ks", "1 kilosecond (time)");
    test("pcs", "approx. 32.31314 petameter (length)");
}

#[test]
fn test_consts_in_conversion() {
    test("1/mpg -> L / 100km", "112903/480, approx. 235.2145 liter / 100 kilometer (area)");
}

#[test]
fn negative_prefixes() {
    test("-1ms", "-1 millisecond (time)");
}
