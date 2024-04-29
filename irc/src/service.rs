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
            Span::Child(child) => {
                let text = to_irc_string(config, &child.to_spans());
                out.push_str(&text);
            }
        }
    }
    out
}

impl Service for RinkService {
    type Req = String;
    type Res = Result<String, String>;
    type Config = Config;

    fn args(_config: &Self::Config) -> Vec<std::ffi::OsString> {
        vec!["--service".into()]
    }

    fn create(config: Self::Config) -> Result<Self, std::io::Error> {
        let memory = config.limits.memory.as_u64();
        // Saturate if the value is >4GB, on 32bit OSes.
        let memory = if memory > (usize::MAX as u64) {
            usize::MAX
        } else {
            memory as usize
        };
        GLOBAL.set_limit(memory);

        let ctx = Arc::new(Mutex::new(crate::config::load(&config)));
        Ok(RinkService { config, ctx })
    }

    fn handle(&self, request: Self::Req) -> Self::Res {
        let mut ctx = self.ctx.lock().unwrap();
        match eval(&mut ctx, &request) {
            Ok(value) => Ok(to_irc_string(&self.config, &value.to_spans())),
            Err(err) => Err(to_irc_string(&self.config, &err.to_spans())),
        }
    }

    fn timeout(config: &Self::Config) -> std::time::Duration {
        config.limits.timeout
    }
}

pub fn run_service() {
    rink_sandbox::become_child::<RinkService, _>(&crate::GLOBAL);
}
