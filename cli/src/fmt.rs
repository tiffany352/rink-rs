// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use nu_ansi_term::{AnsiString, AnsiStrings};
use rink_core::output::fmt::{FmtToken, Span, TokenFmt};

use crate::config::{Config, Theme};

fn to_ansi_inner<'a>(
    theme: &Theme,
    long_output: bool,
    mut indent: usize,
    strings: &mut Vec<AnsiString<'a>>,
    obj: &'a dyn TokenFmt<'a>,
) {
    let spans = obj.to_spans();
    let mut nothing_printed = true;
    for span in spans {
        match span {
            Span::Content {
                token: FmtToken::ListBegin,
                text,
            } if long_output => {
                indent += 1;
                if text.is_empty() && nothing_printed {
                    strings.push(format!("{:width$}• ", "", width = indent * 2 - 2).into());
                } else {
                    strings.push(text.into());
                    strings.push(format!("\n{:width$}• ", "", width = indent * 2 - 2).into());
                }
                nothing_printed = false;
            }
            Span::Content {
                token: FmtToken::ListSep,
                ..
            } if long_output => {
                nothing_printed = false;
                strings.push(format!("\n{:width$}• ", "", width = indent * 2 - 2).into());
            }

            Span::Content {
                text,
                token: token @ FmtToken::DateTime,
            } => {
                let datetime =
                    chrono::naive::NaiveDateTime::parse_from_str(&text, "%Y-%m-%d %H:%M:%S");
                let date = chrono::naive::NaiveDate::parse_from_str(&text, "%Y-%m-%d");
                nothing_printed = false;
                if let Ok(date) = datetime {
                    strings.push(
                        theme
                            .get_style(token)
                            .paint(date.format("%B %-d, %Y %H:%M:%S").to_string()),
                    );
                } else if let Ok(date) = date {
                    strings.push(
                        theme
                            .get_style(token)
                            .paint(date.format("%B %-d, %Y").to_string()),
                    );
                } else {
                    strings.push(theme.get_style(token).paint(text));
                }
            }

            Span::Content {
                text,
                token: token @ FmtToken::Link,
            } => {
                nothing_printed = false;
                strings.push(theme.get_style(token).paint(text.clone()).hyperlink(text));
            }

            Span::Content { text, token } => {
                nothing_printed = false;
                strings.push(theme.get_style(token).paint(text));
            }

            Span::Child(obj) => {
                nothing_printed = false;
                to_ansi_inner(theme, long_output, indent, strings, obj);
            }
        }
    }
}

fn to_ansi<'a>(config: &Config, obj: &'a dyn TokenFmt<'a>) -> Vec<AnsiString<'a>> {
    let mut strings = vec![];
    to_ansi_inner(
        config.get_theme(),
        config.rink.long_output,
        0,
        &mut strings,
        obj,
    );
    strings
}

pub(crate) fn to_ansi_string<'a>(config: &Config, obj: &'a dyn TokenFmt<'a>) -> String {
    let strings = to_ansi(config, obj);
    let strings = AnsiStrings(&strings);
    format!("{}", strings)
}

pub(crate) fn print_fmt<'a>(config: &Config, obj: &'a dyn TokenFmt<'a>) {
    let strings = to_ansi(config, obj);
    let strings = AnsiStrings(&strings);
    print!("{}", strings);
}

#[cfg(test)]
mod tests {
    use nu_ansi_term::{AnsiString, AnsiStrings, Style};
    use rink_core::{parsing::text_query, simple_context, Context};

    use crate::{config::Config, fmt::to_ansi};

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
}
