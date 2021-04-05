use std::sync::{Arc, Mutex};

use linefeed::{Completer, Completion, Prompter, Suffix, Terminal};

use rink_core::{search::query, Context};

use crate::{config::Config, fmt::to_ansi_string};

pub struct RinkCompleter {
    context: Arc<Mutex<Context>>,
    config: Config,
}

impl RinkCompleter {
    pub fn new(context: Arc<Mutex<Context>>, config: Config) -> RinkCompleter {
        RinkCompleter { context, config }
    }
}

impl<Term: Terminal> Completer<Term> for RinkCompleter {
    fn complete(
        &self,
        name: &str,
        _prompter: &Prompter<Term>,
        _start: usize,
        _end: usize,
    ) -> Option<Vec<Completion>> {
        let ctx = self.context.lock().unwrap();
        let reply = query(&ctx, name, 10);

        if reply.results.len() > 0 {
            Some(
                reply
                    .results
                    .into_iter()
                    .map(|result| Completion {
                        // Todo: apply colors, doesn't currently work.
                        display: Some(result.to_string()),
                        completion: result.unit.unwrap(),
                        suffix: Suffix::Default,
                    })
                    .collect(),
            )
        } else {
            None
        }
    }
}
