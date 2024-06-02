// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use chrono::{FixedOffset, TimeZone};
use rink_core::parsing::text_query;
use rink_core::Context;

thread_local! {
    static CONTEXT: Context = {
        let mut ctx = rink_core::simple_context().unwrap();
        // Use a fixed time, this one is the timestamp of the first
        // commit to Rink (in -04:00 originally, but use local time here
        // for determinism.)
        let date = FixedOffset::east_opt(-4*60*60).unwrap().with_ymd_and_hms(2016, 8, 2, 15, 33, 19).unwrap();
        ctx.set_time(date.into());
        ctx.use_humanize = true;
        ctx
    };
}

fn test(input: &str, output: &str) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        let res = ctx.eval_query(&expr);
        let res = match res {
            Ok(v) => v.to_string(),
            Err(v) => v.to_string(),
        };
        assert_eq!(res, output);
    });
}

fn test_starts_with(input: &str, output: &str) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        let res = ctx.eval_query(&expr);
        let res = match res {
            Ok(v) => v.to_string(),
            Err(v) => v.to_string(),
        };
        assert!(res.starts_with(output), "\n'{}' !=\n'{}'", res, output);
    });
}

#[test]
fn test_definition() {
    test(
        "watt",
        "Definition: watt = J / s = 1 watt (power; kg m^2 / s^3). SI derived unit for power or radiant flux.",
    );
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
    test(
        "sqrt -1",
        "Complex numbers are not implemented: sqrt(-1 (dimensionless))",
    );
    test(
        "sqrt(2m)",
        "Result must have integer dimensions: sqrt(2 meter (length))",
    );
}

#[test]
fn test_number_regress() {
    test("953 mega", "953000000 (dimensionless)");
}

#[test]
fn test_lookup() {
    test(
        "pcs",
        "Definition: parsec = approx. 30.85677 petameter (length; m)",
    );
}

#[test]
fn test_consts_in_conversion() {
    test(
        "1/mpg -> L / 100km",
        "112903/480, approx. 235.2145 liter / 100 kilometer (area)",
    );
}

#[test]
fn negative_prefixes() {
    test("-1ms", "-1 millisecond (time)");
}

#[test]
fn negative_now() {
    test(
        "-#jan 01, 1970#",
        "Operation is not defined: - <1970-01-01 00:00:00 +00:00 (46 years ago)>",
    );
}

#[test]
fn negative_conversion() {
    test("1 m -> -meter", "-1 * -1 meter (length)");
}

#[test]
fn test_units_for() {
    test(
        "units for electrical_potential",
        "Units for kg m^2 / A s^3 (electrical_potential): \
         CGS Units: abvolt, daniell, intvolt, statvolt; SI Derived Units: volt",
    );
}

#[test]
fn test_factorize() {
    test(
        "factorize velocity",
        "Factorizations: velocity; acceleration time; \
         flow_rate fuel_efficiency; \
         frequency length; jerk time^2",
    );
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
    test(
        "#jan 01, 1970#",
        "1970-01-01 00:00:00 +00:00 (46 years ago)",
    );
}

#[test]
fn test_lists() {
    test(
        "pi hour -> hr;min;sec",
        "3 hour, 8 minute, 29.73355 second (time)",
    );
    test(
        "meter -> ft;inch;line",
        "3 foot, 3 inch, 4.440944 line (length)",
    );
}

#[test]
fn test_volume_prefix() {
    test("mm^3", "1 millimeter^3 (volume)");
    test("1000000 m^2", "1 kilometer^2 (area)");
}

#[test]
fn test_offset_conversion() {
    test(
        "#jan 01, 1970# -> -05:00",
        "1969-12-31 19:00:00 -05:00 (46 years ago)",
    );
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
    test(
        "100K -> hex °C",
        "Conversion to °C is not defined in base 16",
    );
    test(
        "now -> hex +00:00",
        "Conversion to 00:00 is not defined in base 16",
    );
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
    test("density of water", "1000 kilogram / meter^3 (density)");
    test("mass of ml water", "1 gram (mass)");
    test("volume of g water", "1000 millimeter^3 (volume)");
    test(
        "ml water -> g",
        "water: volume = 1000 millimeter^3 (volume); mass = 1 gram (mass)",
    );
    test(
        "g water -> ml",
        "water: mass = 1 gram (mass); \
         volume = 1 milliliter (volume)",
    );
}

#[test]
fn test_convert_to_substances() {
    test(
        "kg -> egg",
        "egg: USA large egg. \
         mass = 1 kilogram (mass); \
         egg_shelled = 20 egg (dimensionless); \
         egg_white = 33.[3]... egg (dimensionless); \
         egg_yolk = 5000/93, approx. 53.76344 egg (dimensionless)",
    );
}

#[test]
fn test_substance_add() {
    test(
        "air",
        "air: Average molecular weight of air. \
         molar_mass = approx. 28.96790 gram / mole (molar_mass)",
    );
}

#[test]
fn test_duration_add() {
    test(
        "#jan 01, 1970# + 1 s",
        "1970-01-01 00:00:01 +00:00 (46 years ago)",
    );
    test(
        "#jan 01, 1970# + 1.123 s",
        "1970-01-01 00:00:01.123 +00:00 (46 years ago)",
    );
}

#[test]
fn test_0_seconds() {
    test("0 s", "0 second (time)");
}

#[test]
fn right_hand_property() {
    test("kg -> mass_shelled of egg", "20 egg_shelled (mass)");
    test(
        "nauticalmile -> arcmin radius of earth / radian",
        "approx. 0.9993245 arcmin earth_radius / radian (length)",
    );
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
        ctx.eval_query(&expr).unwrap();
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
    test("a = kg N / W^2", "1 second^4 / meter^3");
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
    test(
        "cos(asin(0.5) - 0.5 pi radian)",
        "approx. 0.5000000 (dimensionless)",
    );
    test("atan(tan(0.42))", "approx. 420 milliradian (angle)");
    test("acos(1)", "approx. 0 radian (angle)");
    test("acosh(cosh(1))", "approx. 1 (dimensionless)");
    // test("asinh(sinh(0.123))", "approx. 0.1230000 (dimensionless)");
    // test("atanh(tanh(1.23))", "approx. 1.230000 (dimensionless)");

    test("hypot(3 m, 4 m)", "approx. 5 meter (length)");
    test("atan2(7, 6)", "approx. 862.1700 milliradian (angle)");

    test("sin(90deg)", "approx. 1 (dimensionless)");
    test("cos(180deg)", "approx. -1 (dimensionless)");
    test("tan(45deg)", "approx. 0.9999999 (dimensionless)");

    test("asin(1) to deg", "approx. 90 degree (angle)");
    test("acos(0) to deg", "approx. 90 degree (angle)");
    test("atan(1) to deg", "approx. 45 degree (angle)");

    test(
        "atan2(6inch, 12foot) to deg",
        "approx. 2.385944 degree (angle)",
    );
}

#[test]
fn test_equal_rhs() {
    test("1 -> a=3", "0.[3]... a (dimensionless)");
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
        "Operation is not defined: <1 (dimensionless)> * <2018-10-03 00:00:00 +00:00 (in 2 years)>",
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
        "2018-10-04 09:13:25.123 +02:00",
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
        "Search results: cmil (area), cminv (energy), cmcapacitance (capacitance), sccm (power), mcm (area)",
    );
    test("search water", "Search results: water (substance), waterton (volume), waterhorsepower (power), watt (power), watch (time)");
}

#[test]
fn test_digits() {
    test(
        "ln(1234) -> digits 100",
        "approx. 7.11801620446533345187845043255947530269622802734375 (dimensionless)",
    );
    test("1/7 -> digits 50", "0.[142857]... (dimensionless)");
    test("trillion / 7", "1.[428571]...e11 (dimensionless)");
    test(
        "trillion / 7 to digits",
        "142857142857.[142857]... (dimensionless)",
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
fn test_time_range_checking() {
    test("#00:00#", "2016-08-02 00:00:00 +00:00 (19 hours ago)");
    test("#23:59#", "2016-08-02 23:59:00 +00:00 (in 4 hours)");
    test("#12:00 am#", "2016-08-02 00:00:00 +00:00 (19 hours ago)");
    test("#12:00 pm#", "2016-08-02 12:00:00 +00:00 (7 hours ago)");
    test("#24:00#", "Most likely pattern `year-monthnum-fullday['T'hour24:min[:sec][ offset]]` failed: Expected `-`, got `:`");
    test(
        "#00:00 pm",
        "Most likely pattern `hour24:min[:sec][ offset]` failed: Expected eof, got  pm",
    );
    test(
        "#13:00 am#",
        "Most likely pattern `hour24:min[:sec][ offset]` failed: Expected eof, got  am",
    );
    test("#0000-00-00#", "Most likely pattern `year-monthnum-fullday['T'hour24:min[:sec][ offset]]` failed: Expected monthnum in range 1..=12, got 00");
    test("#0000-01-00#", "Most likely pattern `year-monthnum-fullday['T'hour24:min[:sec][ offset]]` failed: Expected fullday in range 1..=31, got 00");
    test("#00:00:00#", "2016-08-02 00:00:00 +00:00 (19 hours ago)");
    test(
        "#00:00:61#",
        "Most likely pattern `hour24:min[:sec][ offset]` failed: Expected eof, got :61",
    );
    // used to panic
    test("#99999999999999#", "Most likely pattern `year-monthnum-fullday['T'hour24:min[:sec][ offset]]` failed: Expected year, got out of range value");
}

#[test]
fn test_gb_is_gigabytes() {
    test(
        "56kbps * 1 year -> GB",
        "approx. 220.8984 gigabyte (information)",
    )
}

#[test]
fn test_missing_base() {
    test("3 -> base", "Expected decimal base, got eof");
}

#[test]
fn test_date_difference() {
    test_starts_with("now - (now - 3days)", "3 day, 0 second (time)");
}

#[test]
fn test_date_time_formats() {
    test_starts_with("#1970-01-01 10:30 GMT#", "1970-01-01 10:30:00 GMT");
    test_starts_with("(now-#10:30#) - (now-#11:30#)", "1 hour, 0 second (time)");
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
        "Definition: kilogram = base unit of mass. The kilogram, symbol kg, \
            is the SI unit of mass. It is defined by taking the fixed \
            numerical value of the Planck constant h to be 6.626 070 15 × \
            10^–34 when expressed in the unit J s, which is equal to kg m^2 \
            s^–1, where the metre and the second are defined in terms of c \
            and Δν_Cs. 26th CGPM (2018, CR; 211)",
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
        "CH4: molar_mass = 0.01604276 kilogram / mole (molar_mass)",
    );
    test(
        "NaCl",
        "NaCl: molar_mass = approx. 0.05844246 kilogram / mole (molar_mass)",
    );
    test(
        "C8H10N4O2",
        "C8H10N4O2: molar_mass = approx. 0.1941931 kilogram / mole (molar_mass)",
    );
    test(
        "C60",
        "C60: molar_mass = 0.72066 kilogram / mole (molar_mass)",
    );
}

#[test]
fn test_unicode_minus() {
    test("\u{2212}10", "-10 (dimensionless)");
}

#[test]
fn test_offset_date_math() {
    test(
        "#2020-01-01 05:00:00 +05:00# - #2020-01-01 00:00:00 +00:00#",
        "0 second (time)",
    );
}

#[test]
fn test_bad_floats() {
    // Log10
    test("log10(-1)", "approx. NaN (dimensionless)");
    test("log10(0)", "approx. -Inf (dimensionless)");
    test("-log10(0)", "approx. Inf (dimensionless)");
    test("(log10(-1) * 12 + 2) meters", "approx. NaN meter (length)");

    // Log2
    test("log2(-1)", "approx. NaN (dimensionless)");
    test("log2(0)", "approx. -Inf (dimensionless)");

    // Sqrt
    test(
        "sqrt(-1)",
        "Complex numbers are not implemented: sqrt(-1 (dimensionless))",
    );
}

#[test]
fn test_large_floats() {
    test("5.2*10^15*300^(3/2)", "approx. 2.701999e19 (dimensionless)");
}

#[test]
fn test_atom_symbol() {
    test(
        "Og",
        "oganesson: atomic_number = 118 (dimensionless); molar_mass = approx. 294.2139 gram / mole (molar_mass)",
    );
}

#[test]
fn gold_density_should_be_error() {
    test(
        "gold density",
        "No such unit density, did you mean paperdensity?",
    );
}

#[test]
fn quantities_disallowed() {
    test(
        "energy / time",
        "No such unit energy, did you mean mass_energy?",
    );
}

#[test]
fn quantity_defs() {
    test(
        "power",
        "Definition: power = physical quantity for energy / time (kg m^2 / s^3)",
    );
    test(
        "energy",
        "Definition: energy = physical quantity for force length (kg m^2 / s^2)",
    );
}

#[test]
fn test_tim() {
    // Issue #151, rink crashing due to stack overflow
    test(
        "Tim",
        "Definition: Tim = 12^-4 hour = 173.6[1]... millisecond (time; s)",
    );
}

#[test]
fn test_extra_operators() {
    test("37 inches mod feet to inch", "1 inch (length)");
    test("3 meters << 4", "48 meter (length)");
    test(
        "0xf78d286b099b xor 0x500431f2abf4 to digits hex",
        "a7891999a26f (dimensionless)",
    );
    test(
        "0xf78d286b099b and 0x500431f2abf4 to digits hex",
        "500420620990 (dimensionless)",
    );
    test(
        "0xf78d286b099b or 0x500431f2abf4 to digits hex",
        "f78d39fbabff (dimensionless)",
    );
    test("12 meter mod 16 second", "Arguments to `mod` must have matching dimensionality: <12 meter (length)> mod <16 second (time)>");
    test(
        "meter << meter",
        "Right-hand to << must be dimensionless: <1 meter (length)> << <1 meter (length)>",
    );
    test(
        "meter >> meter",
        "Right-hand to >> must be dimensionless: <1 meter (length)> >> <1 meter (length)>",
    );
    test(
        "meter and meter",
        "Arguments to `and` must be dimensionless: <1 meter (length)> and <1 meter (length)>",
    );
    test(
        "meter or meter",
        "Arguments to `or` must be dimensionless: <1 meter (length)> or <1 meter (length)>",
    );
    test(
        "meter xor meter",
        "Arguments to `xor` must be dimensionless: <1 meter (length)> xor <1 meter (length)>",
    );
}

#[test]
fn test_bytes() {
    test("1 mebibyte", "1.048576 megabyte (information)");
    test("1 GiB", "approx. 1.073741 gigabyte (information)");
    test("128 bit", "16 byte (information)");
    test("100 byte^2", "6400 bit^2 (bit^2)");
    test("1/byte", "0.125 / bit (bit^-1)");
}

#[test]
fn test_output_formats() {
    test("surveyfoot to digits", "0.[304800609601219202438404876809753619507239014478028956057912115824231648463296926593853187706375412750825501651003302006604013208026416052832105664211328422656845313690627381254762509525019050038100076200152400, period 210]... meter (length)");
    test("surveyfoot to frac", "1200/3937 meter (length)");
    test("surveyfoot to sci", "approx. 3.048006e-1 meter (length)");
    test("foot to frac", "381/1250 meter (length)");
    test("foot to sci", "3.048e-1 meter (length)");
    test("1 to frac", "1 (dimensionless)");
    test("1 to sci", "1.0e0 (dimensionless)");
    test("1/7 to frac", "1/7 (dimensionless)");
    test("1/7 to fraction", "1/7 (dimensionless)");
    test("1/7 to ratio", "1/7 (dimensionless)");
    test("1/7 to sci", "1.[428571]...e-1 (dimensionless)");
    test("1/7 to scientific", "1.[428571]...e-1 (dimensionless)");
    test("0.5 to eng", "0.5 (dimensionless)");
    test("0.5 to engineering", "0.5 (dimensionless)");

    // engineering
    test("1e9 to eng", "1.0e9 (dimensionless)");
    test("1e10 to eng", "10.0e9 (dimensionless)");
    test("1e11 to eng", "100.0e9 (dimensionless)");
    test("1e12 to eng", "1.0e12 (dimensionless)");
    test("1e13 to eng", "10.0e12 (dimensionless)");
    test("1e14 to eng", "100.0e12 (dimensionless)");
    test("1e15 to eng", "1.0e15 (dimensionless)");
    test("1e16 to eng", "10.0e15 (dimensionless)");
    test("1e17 to eng", "100.0e15 (dimensionless)");

    test("1e-9 to eng", "1.0e-9 (dimensionless)");
    test("1e-10 to eng", "100.0e-12 (dimensionless)");
    test("1e-11 to eng", "10.0e-12 (dimensionless)");
    test("1e-12 to eng", "1.0e-12 (dimensionless)");
    test("1e-13 to eng", "100.0e-15 (dimensionless)");
    test("1e-14 to eng", "10.0e-15 (dimensionless)");
    test("1e-15 to eng", "1.0e-15 (dimensionless)");
}

#[test]
fn conversion_to_digit_errors() {
    test(
        "egg to digits",
        "<1 (dimensionless) egg> to digits is not defined",
    );
    test(
        "egg to digits 50",
        "<1 (dimensionless) egg> to 50 digits is not defined",
    );
    test(
        "egg to frac",
        "<1 (dimensionless) egg> to fraction is not defined",
    );
    test(
        "egg to sci",
        "<1 (dimensionless) egg> to scientific is not defined",
    );
    test(
        "egg to eng",
        "<1 (dimensionless) egg> to engineering is not defined",
    );
    test(
        "now to digits \"US/Pacific\"",
        "Conversion to digits of US/Pacific is not defined",
    );
}
