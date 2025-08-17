use nu_ansi_term::{AnsiString, AnsiStrings, Style};
use rink_core::{parsing::text_query, simple_context, Context};

use rink::config::Config;
use rink::fmt::to_ansi;

thread_local! {
    static CONTEXT: Context = {
        let mut ctx = simple_context().unwrap();
        ctx.use_humanize = false;
        ctx
    };
}

fn test(input: &str, output: &[AnsiString]) {
    let mut iter = text_query::TokenIterator::new(input.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    CONTEXT.with(|ctx| {
        let mut config = Config::default();
        config.rink.long_output = true;
        let res = ctx.eval_query(&expr);
        let strings = match res {
            Ok(ref res) => to_ansi(&config, res),
            Err(ref res) => to_ansi(&config, res),
        };
        similar_asserts::assert_eq!(AnsiStrings(output), AnsiStrings(&strings));
    });
}

#[test]
fn test_to_ansi() {
    use nu_ansi_term::Color::*;

    test(
            "meter",
            &[
                Style::new().paint("Definition: "),
                Style::new().fg(Cyan).paint("meter"),
                Style::new().paint(" = "),
                Style::new().paint("base unit of length"),
                Style::new().paint(". "),
                Style::new().italic().paint("The metre, symbol m, is the SI unit of length. It is defined by taking the fixed numerical value of the speed of light in vacuum c to be 299 792 458 when expressed in the unit m/s, where the second is defined in terms of Δν_Cs. 26th CGPM (2018, CR; 211)"),
            ],
        );

    test(
        "foot",
        &[
            Style::new().paint("Definition: "),
            Style::new().fg(Cyan).paint("foot"),
            Style::new().paint(" = "),
            Style::new().paint("12 inch"),
            Style::new().paint(" = "),
            Style::new().paint("304.8"),
            Style::new().paint(" "),
            Style::new().fg(Cyan).paint("millimeter"),
            Style::new().paint(" "),
            Style::new().paint("("),
            Style::new().fg(Cyan).dimmed().paint("length"),
            Style::new().paint("; "),
            Style::new().fg(Cyan).paint("m"),
            Style::new().paint(")"),
            Style::new().paint(". "),
            Style::new()
                .italic()
                .paint("International yard and pound, since "),
            Style::new().paint("July 1, 1959"),
            Style::new().italic().paint("."),
        ],
    );

    test("floppy", &[
                Style::new().paint("Definition: "),
                Style::new().fg(Cyan).paint("floppydisk"),
                Style::new().paint(" = "),
                Style::new().paint("1440 KiB"),
                Style::new().paint(" = "),
                Style::new().paint("1.47456"),
                Style::new().paint(" "),
                Style::new().fg(Cyan).paint("megabyte"),
                Style::new().paint(" "),
                Style::new().paint("("),
                Style::new().fg(Cyan).dimmed().paint("information"),
                Style::new().paint("; "),
                Style::new().fg(Cyan).paint("bit"),
                Style::new().paint(")"),
                Style::new().paint(". "),
                Style::new()
                    .italic()
                    .paint("The common 3.5 inch floppy disk in \"1.44 Meg\" format. The 1.44 comes from mixing decimal and binary prefixes (1000*1024 bytes). Equal to 512 B x 80 tracks x 18 sectors x 2 sides. Source: "),
                Style::new().fg(Blue).paint("http://www.manmrk.net/tutorials/DOS/PSBOOK/book4/floppyd.htm").hyperlink("http://www.manmrk.net/tutorials/DOS/PSBOOK/book4/floppyd.htm"),
        ]);

    test(
        "aaaaaaaaaa",
        &[
            Style::new().fg(Red).paint("No such unit "),
            Style::new().bold().paint("aaaaaaaaaa"),
        ],
    );

    test(
        "search horse",
        &[
            Style::new().paint("Search results: "),
            Style::new().paint("\n• "),
            Style::new().fg(Cyan).paint("horsepower"),
            Style::new().paint(" "),
            Style::new().paint("("),
            Style::new().fg(Cyan).dimmed().paint("power"),
            Style::new().paint(")"),
            Style::new().paint("\n• "),
            Style::new().fg(Cyan).paint("brhorsepower"),
            Style::new().paint(" "),
            Style::new().paint("("),
            Style::new().fg(Cyan).dimmed().paint("power"),
            Style::new().paint(")"),
            Style::new().paint("\n• "),
            Style::new().fg(Cyan).paint("waterhorsepower"),
            Style::new().paint(" "),
            Style::new().paint("("),
            Style::new().fg(Cyan).dimmed().paint("power"),
            Style::new().paint(")"),
            Style::new().paint("\n• "),
            Style::new().fg(Cyan).paint("boilerhorsepower"),
            Style::new().paint(" "),
            Style::new().paint("("),
            Style::new().fg(Cyan).dimmed().paint("power"),
            Style::new().paint(")"),
            Style::new().paint("\n• "),
            Style::new().fg(Cyan).paint("metrichorsepower"),
            Style::new().paint(" "),
            Style::new().paint("("),
            Style::new().fg(Cyan).dimmed().paint("power"),
            Style::new().paint(")"),
        ],
    );

    test(
        "volume of moon * (19.283 g/cm^3) * G / (radius of moon)^2 to gravity",
        &[
            Style::new().paint("approx. "),
            Style::new().paint("0.9549987"),
            Style::new().paint(" "),
            Style::new().fg(Cyan).paint("gravity"),
            Style::new().paint(" "),
            Style::new().paint("("),
            Style::new().fg(Cyan).dimmed().paint("acceleration"),
            Style::new().paint(")"),
        ],
    );
}
