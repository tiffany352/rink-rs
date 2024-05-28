// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use rink_core::output::fmt::{FmtToken, Span};

use crate::config::Config;

pub(crate) fn to_irc_string(config: &Config, spans: &[Span]) -> String {
    let mut out = String::new();
    write_irc_string(&mut out, config, spans);
    out
}

fn write_irc_string(out: &mut String, config: &Config, spans: &[Span]) {
    for span in spans {
        match span {
            Span::Content { text, token } => write_irc_token(out, config, text, *token),
            Span::Child(child) => {
                write_irc_string(out, config, &child.to_spans());
            }
        }
    }
}

fn write_irc_token(out: &mut String, config: &Config, text: &str, token: FmtToken) {
    let (prefix, postfix) = match token {
        FmtToken::Plain => ("", ""),
        FmtToken::Error => ("\x0304", "\x03"),
        FmtToken::Unit | FmtToken::PropName => ("\x0311", "\x03"),
        FmtToken::Quantity => ("\x0310", "\x03"),
        FmtToken::Number => ("", ""),
        FmtToken::UserInput => ("\x02", "\x02"),
        FmtToken::ListBegin => ("", ""),
        FmtToken::ListSep => ("", ""),
        FmtToken::DocString => ("\x1D", "\x1D"),
        FmtToken::Pow => ("", ""),
        FmtToken::DateTime => ("", ""),
        FmtToken::Link => ("", ""),
    };
    out.push_str(prefix);
    out.push_str(text);
    out.push_str(postfix);
}
