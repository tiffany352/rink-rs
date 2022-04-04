// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::sync::{Arc, Mutex};

use rustyline::{
    completion::{extract_word, Completer, Pair},
    highlight::Highlighter,
    hint::Hinter,
    validate::Validator,
    Helper,
};
use rustyline::{Context as LineContext, Result};

use rink_core::{commands, Context};

use crate::{config::Config, fmt::to_ansi_string};

pub struct RinkHelper {
    context: Arc<Mutex<Context>>,
    config: Config,
}

impl RinkHelper {
    pub fn new(context: Arc<Mutex<Context>>, config: Config) -> RinkHelper {
        RinkHelper { context, config }
    }
}

impl Completer for RinkHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, _ctx: &LineContext) -> Result<(usize, Vec<Pair>)> {
        let (res_pos, name) = extract_word(line, pos, None, &[b' ']);

        let ctx = self.context.lock().unwrap();
        let reply = commands::search(&ctx, name, 100);

        let results = reply
            .results
            .into_iter()
            .filter(|result| result.unit.as_ref().unwrap().starts_with(name))
            .take(10)
            .map(|result| Pair {
                display: to_ansi_string(&self.config, &result),
                replacement: result.unit.unwrap(),
            })
            .collect();

        Ok((res_pos, results))
    }
}

impl Helper for RinkHelper {}

impl Validator for RinkHelper {}

impl Highlighter for RinkHelper {}

impl Hinter for RinkHelper {
    type Hint = String;
}
