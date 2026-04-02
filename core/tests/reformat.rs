use jiff::Zoned;
use rink_core::output::fmt::{FmtToken, Span, TokenFmt};
use rink_core::Context;
use std::cell::RefCell;

thread_local! {
    static CONTEXT: RefCell<Context> = {
        let mut ctx = rink_core::simple_context().unwrap();
        // Use a fixed time, this one is the timestamp of the first
        // commit to Rink (in -04:00 originally, but use local time here
        // for determinism.)
        let date: Zoned = "2016-08-02 15:33:19[America/New_York]".parse().unwrap();
        ctx.set_time(date.into());
        ctx.use_humanize = true;
        RefCell::new(ctx)
    };
}

fn write_string<'a>(string: &mut String, last_token: &mut FmtToken, obj: &'a dyn TokenFmt<'a>) {
    let spans = obj.to_spans();
    for span in spans {
        match span {
            Span::Content { text, token } => {
                if token != *last_token {
                    string.push('<');
                    string.push_str(token.as_str());
                    string.push('>');
                    *last_token = token;
                }
                string.push_str(&text)
            }
            Span::Child(obj) => write_string(string, last_token, obj),
        }
    }
}

fn reformat(input: &str) -> String {
    let expr = CONTEXT.with(|ctx| rink_core::reformat(&mut *ctx.borrow_mut(), input));
    let mut string = String::new();
    let mut last_token = FmtToken::Plain;
    write_string(&mut string, &mut last_token, &expr);
    string
}

#[test]
fn test_temperature() {
    assert_eq!(reformat("5degC"), "<number>5<plain> °C");
    assert_eq!(reformat("5degF"), "<number>5<plain> °F");
    assert_eq!(reformat("5degN"), "<number>5<plain> °N");
    assert_eq!(reformat("5degRe"), "<number>5<plain> °Ré");
    assert_eq!(reformat("5degRo"), "<number>5<plain> °Rø");
    assert_eq!(reformat("5degDe"), "<number>5<plain> °De");
}

#[test]
fn test_conversions() {
    // Why is this Conversion::None and not an error?
    assert_eq!(reformat("m ->"), "<unit>meter<plain> ->");
    assert_eq!(reformat("m to ft"), "<unit>meter<plain> -> <unit>foot");
    assert_eq!(
        reformat("20degC to degF"),
        "<number>20<plain> °C -> <keyword>°F"
    );
    assert_eq!(
        reformat("smoot to ft;inch"),
        "<unit>smoot<plain> -> <unit>foot<plain>, <unit>inch"
    );
    assert_eq!(
        reformat("now to +04:00"),
        "<unit>now<plain> -> <number>+4:00"
    );
    assert_eq!(
        reformat(r#"now to "US/Pacific""#),
        "<unit>now<plain> -> <time_zone>[US/Pacific]"
    );
    assert_eq!(
        reformat(r#"now to "Etc/GMT+8""#),
        "<unit>now<plain> -> <time_zone>[Etc/GMT+8]"
    );
}

#[test]
fn test_conversion_modifiers() {
    assert_eq!(
        reformat("pi to digits"),
        "<unit>π<plain> -> <keyword>digits"
    );
    assert_eq!(
        reformat("pi to digits 50"),
        "<unit>π<plain> -> <keyword>digits<plain> <number>50"
    );
    assert_eq!(
        reformat("pi to frac"),
        "<unit>π<plain> -> <keyword>fraction"
    );
    assert_eq!(
        reformat("pi to sci"),
        "<unit>π<plain> -> <keyword>scientific"
    );
    assert_eq!(
        reformat("pi to eng"),
        "<unit>π<plain> -> <keyword>engineering"
    );
    assert_eq!(
        reformat("pi to base 2"),
        "<unit>π<plain> -> <keyword>binary"
    );
    assert_eq!(reformat("pi to base 8"), "<unit>π<plain> -> <keyword>octal");
    assert_eq!(
        reformat("pi to base 16"),
        "<unit>π<plain> -> <keyword>hexadecimal"
    );
    assert_eq!(
        reformat("pi to base 12"),
        "<unit>π<plain> -> <keyword>base<plain> <number>12"
    );
}

#[test]
fn test_conversion_modifiers_with_unit() {
    assert_eq!(
        reformat("ft to digits inch"),
        "<unit>foot<plain> -> <keyword>digits<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to digits 50 inch"),
        "<unit>foot<plain> -> <keyword>digits<plain> <number>50<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to frac inch"),
        "<unit>foot<plain> -> <keyword>fraction<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to sci inch"),
        "<unit>foot<plain> -> <keyword>scientific<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to eng inch"),
        "<unit>foot<plain> -> <keyword>engineering<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to base 2 inch"),
        "<unit>foot<plain> -> <keyword>binary<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to base 8 inch"),
        "<unit>foot<plain> -> <keyword>octal<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to base 16 inch"),
        "<unit>foot<plain> -> <keyword>hexadecimal<plain> <unit>inch"
    );
    assert_eq!(
        reformat("ft to base 12 inch"),
        "<unit>foot<plain> -> <keyword>base<plain> <number>12<plain> <unit>inch"
    );
}

#[test]
fn test_other_queries() {
    assert_eq!(
        reformat("factorize watt"),
        "<keyword>factorize<plain> <unit>watt"
    );
    assert_eq!(
        reformat("units for energy"),
        "<keyword>units for<plain> <unit>energy"
    );
    assert_eq!(
        reformat("search asdf"),
        "<keyword>search<plain> <user_input>asdf"
    );
    assert_eq!(reformat("/"), "<error><error: Expected term, got `/`>");
}

#[test]
fn test_expressions() {
    assert_eq!(reformat("ft + inch"), "<unit>foot<plain> + <unit>inch");
    assert_eq!(reformat("+inch"), "+<unit>inch");
    assert_eq!(reformat("ft * ft"), "<unit>foot<plain> <unit>foot");
    assert_eq!(
        reformat("density of water"),
        "<prop_name>density<plain> <keyword>of<plain> <unit>water"
    );
    assert_eq!(reformat("+1"), "+<number>1");
    assert_eq!(reformat("cm^3"), "<unit>centimeter<plain>^<number>3");
    assert_eq!(
        reformat("tan(1deg)"),
        "tan(<number>1<plain> <unit>degree<plain>)"
    );
}

#[test]
fn test_canonicalizations() {
    assert_eq!(reformat("c"), "<unit>c");
}

#[test]
fn test_dates() {
    assert_eq!(reformat("#jan 01, 1970#"), "<date_time>#jan 01, 1970#");
}

#[test]
fn test_unaryop_prec() {
    // LHS
    assert_eq!(reformat("(-5)^2"), "(-<number>5<plain>)^<number>2");
    assert_eq!(
        reformat("5 * (-1)^2"),
        "<number>5<plain> * (-<number>1<plain>)^<number>2"
    );
    assert_eq!(reformat("(-meter)^2"), "(-<unit>meter<plain>)^<number>2");

    // RHS
    assert_eq!(reformat("5^(-2)"), "<number>5<plain>^(-<number>2<plain>)");
    assert_eq!(
        reformat("meter^(-1)"),
        "<unit>meter<plain>^(-<number>1<plain>)"
    );
}

#[test]
fn test_associativity_prec() {
    assert_eq!(
        reformat("1 - (2 - 3)"),
        "<number>1<plain> - (<number>2<plain> - <number>3<plain>)"
    );
    assert_eq!(
        reformat("1 - (2 + 3)"),
        "<number>1<plain> - (<number>2<plain> + <number>3<plain>)"
    );
    assert_eq!(
        reformat("1 / (2 / 3)"),
        "<number>1<plain> / (<number>2<plain> / <number>3<plain>)"
    );
    assert_eq!(
        reformat("10 / (meter/second)"),
        "<number>10<plain> / (<unit>meter<plain> / <unit>second<plain>)"
    );
    assert_eq!(
        reformat("5 << (2 << 1)"),
        "<number>5<plain> << (<number>2<plain> << <number>1<plain>)"
    );
    assert_eq!(
        reformat("5 >> (2 >> 1)"),
        "<number>5<plain> >> (<number>2<plain> >> <number>1<plain>)"
    );
    assert_eq!(
        reformat("5 and (2 or 1)"),
        "<number>5<plain> and (<number>2<plain> or <number>1<plain>)"
    );
}

#[test]
fn test_mul_juxt_or_not() {
    assert_eq!(
        reformat("5 / (m s)"),
        "<number>5<plain> / <unit>meter<plain> <unit>second"
    );
    assert_eq!(
        reformat("5 / (2 m s)"),
        "<number>5<plain> / <number>2<plain> <unit>meter<plain> <unit>second"
    );
    assert_eq!(
        reformat("5 / (2 3)"),
        "<number>5<plain> / (<number>2<plain> * <number>3<plain>)"
    );
}
