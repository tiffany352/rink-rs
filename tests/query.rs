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

fn test_starts_with(input: &str, output: &str) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        let res = ctx.eval_outer(&expr);
        let res = match res {
            Ok(v) => v.to_string(),
            Err(v) => v.to_string(),
        };
        assert!(
            res.starts_with(output),
            format!("\n'{}' !=\n'{}'", res, output)
        );
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
          CGS Units: abvolt, daniell, intvolt, statvolt; SI Derived Units: volt");
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
    test(
        "W -> J",
        "Conformance error: 1 watt (power) != 1 joule (energy)\n\
         Suggestions: multiply left side by time, multiply right side by frequency",
    );
    test(
        "W/s -> J^2",
        "Conformance error: 1 newton^2 / kilogram != 1 joule^2\n\
         Suggestions: multiply left side by moment_of_inertia, divide right side by moment_of_inertia",
    );
    test(
        "m^2 -> kg^2",
        "Conformance error: 1 meter^2 (area) != 1 kilogram^2 (kg^2)\n\
         Suggestions: multiply left side by linear_density^2, multiply right side by area / mass^2",
    );
    test(
        "c -> kg",
        "Conformance error: 299792458 meter / second (velocity) != 1 kilogram (mass)\n\
         Suggestions: multiply left side by mass time / length, multiply right side by length / mass time"
    );
    test(
        "1/m -> 'abc'",
        "Conformance error: 1 / meter (m^-1) != 1 abc (abc)\n\
         Suggestions: multiply left side by 'abc' length, divide right side by 'abc' length",
    );
}

#[test]
fn test_dates() {
    test("#jan 01, 1970#",
         "1970-01-01 00:00:00 +00:00");
}

#[test]
fn test_lists() {
    test("pi hour -> hr;min;sec", "3 hour, 8 minute, 29.73355 second (time)");
    test("meter -> ft;inch;line", "3 foot, 3 inch, 4.440944 line (length)");
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
    test("256 -> base 16", "100 (dimensionless)");

    test(
        "123 -> base 37",
        "Unsupported base 37, must be from 2 to 36",
    );
    test("123 -> base 0xf", "Expected decimal base, got hex");
}

#[test]
fn test_typos() {
    test("rsi", "No such unit rsi, did you mean RSI?");
}

#[test]
fn test_convert_from_substances() {
    test("density of water",
         "1000 kilogram / meter^3 (density)");
    test("mass of ml water",
         "1 gram (mass)");
    test("volume of g water",
         "1000 millimeter^3 (volume)");
    test("ml water -> g",
         "water: volume = 1000 millimeter^3; mass = 1 gram");
    test("g water -> ml",
         "water: mass = 1 gram; \
          volume = 1 milliliter");
}

#[test]
fn test_convert_to_substances() {
    test("kg -> egg",
         "egg: USA large egg. \
          mass = 1 kilogram; \
          egg_shelled = 20 egg; \
          egg_white = 100/3, approx. 33.33333 egg; \
          egg_yolk = 5000/93, approx. 53.76344 egg");
}

#[test]
fn test_substance_add() {
    test("air",
         "air: Average molecular weight of air. \
          molar_mass = approx. 28.96790 gram / mole");
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

#[test]
fn percent_operator() {
    test("100%", "1 (dimensionless)");
    test("100%%", "0.01 (dimensionless)");
    test("200% ** 2", "4 (dimensionless)");
    test("120% 2", "2.4 (dimensionless)");
    test("% 1", "0.01 (dimensionless)");
}

#[test]
fn test_kilosecond() {
    test("1ks", "16 minute, 40 second (time)");
    test("1kss", "16 minute, 40 second (time)");
}

#[test]
#[should_panic]
fn test_second_double_prefix() {
    let mut iter = text_query::TokenIterator::new("mks").peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        ctx.eval_outer(&expr).unwrap();
    });
}

#[test]
fn test_missing_substance() {
    test(
        "density of flubber",
        "No such unit flubber, did you mean flour?",
    );
}

#[test]
fn test_missing_property() {
    test("mass of flour", "No such property mass of flour");
}

#[test]
fn test_unary_operators() {
    test("+--+42", "42 (dimensionless)");
    test("++-+42", "-42 (dimensionless)");
}

#[test]
fn test_equals() {
    test("a = kg N / W^2", "1 second^2 / gray meter");
    test(
        "1 = kg",
        "= is currently only used for inline unit definitions: expected unit, got 1",
    );
}

#[test]
fn mismatched_units() {
    test(
        "W - kg",
        "Subtraction of units with mismatched units is not meaningful: \
         <1 watt (power)> - <1 kilogram (mass)>",
    );
}

#[test]
fn temperature_with_dimension() {
    test("kg °C", "Expected dimensionless, got: <1 kilogram (mass)>");
}

#[test]
fn test_functions() {
    test("exp(ln(10))", "approx. 10.00000 (dimensionless)");
    test("log2(65536)", "approx. 16 (dimensionless)");
    test("10^log10(123)", "approx. 123.0000 (dimensionless)");
    test("log(27, 3)", "approx. 3 (dimensionless)");

    test("sin(pi/2)", "approx. 1 (dimensionless)");
    test("cos(asin(0.5) - pi/2)", "approx. 0.5000000 (dimensionless)");
    test("atan(tan(0.42))", "approx. 0.4199999 (dimensionless)");
    test("acos(1)", "approx. 0 (dimensionless)");
    test("acosh(cosh(1))", "approx. 1 (dimensionless)");
    test("asinh(sinh(0.123))", "approx. 0.1230000 (dimensionless)");
    test("atanh(tanh(1.23))", "approx. 1.230000 (dimensionless)");

    test("hypot(3 m, 4 m)", "approx. 5 meter (length)");
    test("atan2(7, 6)", "approx. 0.8621700 (dimensionless)");
}

#[test]
fn test_equal_rhs() {
    test("1 -> a=3", "1/3, approx. 0.3333333 a (dimensionless)");
}

#[test]
fn test_pow_with_dimension() {
    test(
        "2^m",
        "Exponent must be dimensionless: <2 (dimensionless)> ^ <1 meter (length)>",
    );
}

#[test]
fn test_reciprocal_conversion() {
    test(
        "miles / gallon -> l / 100km",
        "Conformance error: approx. 425143.7 / meter^2 (fuel_efficiency) != 10000 micrometer^2 (area)\n\
        Suggestions: Reciprocal conversion, invert one side",
    );
}

#[test]
fn test_non_conversion_input() {
    test("g", "Definition: gram = (1 / 1000) kg = 1 gram (mass; kg)");
}

#[test]
fn test_of_non_substance() {
    test("mass of 1kg", "Not defined: mass of <1 kilogram (mass)>");
}

#[test]
fn test_mul_not_defined() {
    test(
        "#2018-10-03# * kg",
        "Operation is not defined: <1 (dimensionless)> * <2018-10-03 00:00:00 +00:00>",
    );
}

#[test]
fn test_log_base_with_dimension() {
    test(
        "log(10, 5m)",
        "Base must be dimensionless: log(10 (dimensionless), 5 meter (length))",
    );
}

#[test]
fn test_hypot_dimension_mismatch() {
    test(
        "hypot(3s, 4m)",
        "Arguments to hypot must have matching dimensionality: \
         hypot(3 second (time), 4 meter (length))",
    );
}

#[test]
fn test_radix() {
    test("0xff", "255 (dimensionless)");
    test(
        "0off",
        "Expected term, got <Malformed octal literal: No digits after 0o>",
    );
    test("0b101010", "42 (dimensionless)");
    test("0o10lux", "8 lux (illuminance)");
}

#[test]
fn test_comments() {
    test("1 // *3", "1 (dimensionless)");
    test("1 + /*2*/ 3", "4 (dimensionless)");
    test("1 + /*2", "Expected term, got <Expected `*/`, got EOF>");
}

#[test]
fn test_leading_dot() {
    test(".12345Ee3", "123.45 (dimensionless)");
}

#[test]
fn test_underscores_in_number() {
    test("123_456\u{2009}789", "123456789 (dimensionless)");
}

#[test]
fn test_date_input() {
    test_starts_with(
        "#2018-10-04T09:13:25.123   +2:00#",
        "2018-10-04 11:13:25.123 +02:00",
    );
}

#[test]
fn test_unicode_arrow() {
    test("pound → kg", "approx. 0.4535923 kilogram (mass)");
}

#[test]
fn test_attributes() {
    test(
        "roman mile",
        "Definition: romanmile = 8 stadia = 1.48 kilometer (length; m)",
    );
    test(
        "romanmile",
        "Definition: romanmile = 8 stadia = 1.48 kilometer (length; m)",
    );
    test(
        "international",
        "Attribute must be followed by ident, got eof",
    );
}

#[test]
fn test_search() {
    test(
        "search cm",
        "Search results: CM¥ (money), cmil (area), cminv (energy), \
         cmcapacitance (capacitance), sccm (power)",
    );
}

#[test]
fn test_digits() {
    test(
        "ln(1234) -> digits 100",
        "approx. 7.11801620446533345187845043255947530269622802734375 (dimensionless)",
    );
}

#[test]
fn test_escapes() {
    test("'ab\\'cd\\n\\t'", "1 ab'cd\n\t (ab'cd\n\t)");
    test("'x\\a'", "Expected term, got <Invalid escape sequence \\a>");
}

#[test]
fn test_missing_bracket() {
    test("(1+2", "Expected `)`, got eof");
}

#[test]
fn test_to_timezone() {
    test_starts_with(
        "#2000-01-01 12:46 Asia/Tokyo# -> GMT",
        "2000-01-01 03:46:00 GMT",
    );
}

#[test]
fn test_missing_base() {
    test("3 -> base", "Expected decimal base, got eof");
}

#[test]
fn test_date_difference() {
    test_starts_with("now - (now - 3days)", "2 day, 23 hour, 59 minute, 59.99");
}

#[test]
fn test_date_time_formats() {
    test_starts_with("#1970-01-01 10:30 GMT#", "1970-01-01 10:30:00 GMT");
    test_starts_with("(now-#10:30#) - (now-#11:30#)", "59 minute, 59.99");
}

#[test]
fn test_no_calls_on_rhs() {
    test(
        "1 -> sin(2)",
        "Calls are not allowed in the right hand side of conversions",
    );
}

#[test]
fn test_conversion_to_list() {
    test(
        "ly -> teram,Gm,Mm,km,m",
        "9.46 kiloteram, 730 gigameter, 472 megameter, 580 kilometer, 800 meter (length)",
    );
    test(
        "1 -> m, hour",
        "Units in unit list must conform: <1 meter (length)> ; <3.6 kilosecond (time)>",
    );
    test(
        "1g -> m, cm",
        "Conformance error: 1 gram (mass) != 1 meter (length)\n\
         Suggestions: divide left side by linear_density, multiply right side by linear_density",
    );
}

#[test]
fn test_definition_with_doc() {
    test(
        "mass",
        "Definition: kilogram = base unit of mass. Equal to the mass of the \
         international prototype of the kilogram. 3rd CGPM (1901, CR, 70).",
    );
}

#[test]
fn test_try_decode_fail() {
    test(
        "#abc#",
        "Most likely pattern `--monthnum-day[ hour24:min[:sec][ offset]]` failed: \
         Expected `-`, got `abc`",
         )
}

#[test]
fn test_formula() {
    test(
        "methane=CH4",
        "CH4: molar_mass = 0.01604276 kilogram / mole",
    );
    test(
        "NaCl",
        "NaCl: molar_mass = approx. 0.05844246 kilogram / mole",
    );
    test(
        "C8H10N4O2",
        "C8H10N4O2: molar_mass = approx. 0.1941931 kilogram / mole",
    );
    test("C60", "C60: molar_mass = 0.72066 kilogram / mole");
}

#[test]
fn test_unicode_minus() {
    test("\u{2212}10", "-10 (dimensionless)");
}
