// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use rink_core::output::fmt::{FmtToken, Span, TokenFmt};
use rink_core::{eval, Context};
use rink_sandbox::Service;
use std::sync::{Arc, Mutex};

use crate::{config::Config, GLOBAL};

pub struct RinkService {
    config: Config,
    ctx: Arc<Mutex<Context>>,
}

fn to_irc_string(config: &Config, spans: &[Span]) -> String {
    let mut out = String::new();
    for span in spans {
        match span {
            Span::Content {
                text,
                token: FmtToken::Plain,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::Error,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::Unit,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::Quantity,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::Number,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::UserInput,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::ListBegin,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::ListSep,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::DocString,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::Pow,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::PropName,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::DateTime,
            } => out.push_str(&text),
            Span::Content {
                text,
                token: FmtToken::Link,
            } => out.push_str(&text),
            Span::Child(child) => {
                let text = to_irc_string(config, &child.to_spans());
                out.push_str(&text);
            }
        }
    }
    out
}
