// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;

use rink::*;

thread_local! {
    static CONTEXT: Context = {
        let mut ctx = load().unwrap();
        ctx.use_humanize = false;
        ctx
    };
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
         "Complex numbers are not implemented: sqrt(-1 (dimensionless))");
    test("sqrt(2m)",
         "Result must have integer dimensions: sqrt(2 meter (length))");
}

#[test]
fn test_number_regress() {
    test("953 mega",
         "9.53e8 (dimensionless)");
}

#[test]
fn test_lookup() {
    test("pcs", "Definition: parsec = approx. 32.31314 petameter (length; m)");
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
    test("-#jan 01, 1970#", "Operation is not defined: - <1970-01-01 00:00:00 +00:00>");
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
         "Factorizations: velocity;  acceleration time;  \
          flow_rate fuel_efficiency;  \
          frequency length;  jerk time^2");
}

#[test]
fn test_conformance() {
    test("W -> J",
         "Conformance error: 1 watt (power) != 1 joule (energy)\n\
          Suggestions: multiply left side by time, multiply right side by frequency");
}

#[test]
fn test_dates() {
    test("#jan 01, 1970#",
         "1970-01-01 00:00:00 +00:00");
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

#[test]
fn test_offset_conversion() {
    test("#jan 01, 1970# -> -05:00",
         "1969-12-31 19:00:00 -05:00");
}

#[test]
fn test_time_hms() {
    test("ks", "16 minute, 40 second (time)");
    test("nanosecond", "1 nanosecond (time)");
}

#[test]
fn test_bases() {
    test("pi -> hex", "approx. 3.243f6a (dimensionless)");
    test("pi -> oct", "approx. 3.110375 (dimensionless)");
    test("pi -> bin", "approx. 11.00100 (dimensionless)");
    test("pi m -> hex m", "approx. 3.243f6a meter (length)");
    test("pi m -> oct m", "approx. 3.110375 meter (length)");
    test("pi m -> bin m", "approx. 11.00100 meter (length)");
    test("100K -> hex °C", "Conversion to °C is not defined in base 16");
    test("now -> hex +00:00", "Conversion to 00:00 is not defined in base 16");
}

#[test]
fn test_typos() {
    test("rsi", "Unknown unit rsi, did you mean RSI?");
}

#[test]
fn test_convert_from_substances() {
    test("density of water",
         "0.001 meter^3 / kilogram (specific_volume)");
    test("mass of ml water",
         "1 gram (mass)");
    test("volume of g water",
         "1000 millimeter^3 (volume)");
    test("ml water -> g",
         "water: volume = 1000 millimeter^3; mass = 1 gram");
    test("g water -> ml",
         "water: mass = 1 gram; \
          volume = 1 milliliter; \
          fusion_energy = 334106640 milliliter; \
          vaporization_energy = 1.16e9 milliliter");
}

#[test]
fn test_convert_to_substances() {
    test("kg -> egg",
         "egg: USA large egg. \
          mass = 1 kilogram; \
          egg_shelled = 20; \
          egg_white = 100/3, approx. 33.33333; \
          egg_yolk = 5000/93, approx. 53.76344");
}

#[test]
fn test_substance_add() {
    test("air",
         "air: Average molecular weight of air. \
          molar_mass = approx. 28.96790 gram -> 1 mole");
}

#[test]
fn test_duration_add() {
    test("#jan 01, 1970# + 1 s",
         "1970-01-01 00:00:01 +00:00");
    test("#jan 01, 1970# + 1.123 s",
         "1970-01-01 00:00:01.123 +00:00");
}

#[test]
fn test_0_seconds() {
    test("0 s", "0 second (time)");
}

#[test]
fn right_hand_property() {
    test("kg -> mass_shelled of egg",
         "20 egg_shelled (mass)");
    test("nauticalmile -> arcmin radius of earth / radian",
         "approx. 0.9993245 arcmin earth_radius / radian (length)");
}
