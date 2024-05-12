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
