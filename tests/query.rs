// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
            Ok(v) => v.to_string(),
            Err(v) => v.to_string()
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

#[test]
fn negative_now() {
    test("-#jan 01, 1970#", "Operation is not defined: - <1970-01-01 00:00:00 +00:00 (46 years ago)>");
}

#[test]
fn negative_conversion() {
    test("1 m -> -meter", "-1 * -1 meter (length)");
}

#[test]
fn test_units_for() {
    test("units for electrical_potential",
         "Units for kg m^2 / A s^3 (electrical_potential): \
          abvolt, daniell, intvolt, statvolt, volt");
}

#[test]
fn test_factorize() {
    test("factorize velocity",
         "Factorizations: velocity;  area viscosity;  \
          frequency length;  angular_momentum fuel_efficiency;  \
          acceleration time;  length^2 viscosity;  jerk time^2");
}

#[test]
fn test_conformance() {
    test("W -> J",
         "Conformance error: 1 watt (power) != 1 joule (energy)\n\
          Suggestions: multiply left side by time, multiply left side by frequency");
}

#[test]
fn test_dates() {
    test("#jan 01, 1970#",
         "1970-01-01 00:00:00 +00:00 (46 years ago)");
}

#[test]
fn test_lists() {
    test("pi hour -> hr;min;sec", "3 hour, 8 minute, 29.73355 second (time)");
    test("meter -> ft;in;line", "3 intfoot, 3 intinch, 4.440944 intline (length)");
}

#[test]
fn test_volume_prefix() {
    test("mm^3", "1 millimeter^3 (volume)");
    test("1000000 m^2", "1 kilometer^2 (area)");
}
