// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::config::Config;
use crate::fmt::to_ansi_string;
use crate::service::{EvalResult, RinkService};
use eyre::Result;
use rink_core::output::QueryError;
use rink_sandbox::Sandbox;
use std::sync::{Arc, Mutex};

/// Abstracts over evaluating in a sandboxed
/// child process vs evaluating locally.
pub(crate) struct Runner {
    pub(crate) local: Arc<Mutex<rink_core::Context>>,
    sandbox: Option<Sandbox<RinkService>>,
    config: Config,
}

impl Runner {
    pub(crate) fn new(config: Config) -> Result<Runner> {
        let ctx = crate::config::load(&config)?;
        let ctx = Arc::new(Mutex::new(ctx));
        let sandbox = if config.limits.enabled {
            let mut sandbox = Sandbox::new(config.clone())?;
            sandbox.restart()?;
            Some(sandbox)
        } else {
            None
        };
        Ok(Runner {
            local: ctx,
            sandbox,
            config,
        })
    }

    pub(crate) fn execute(&mut self, line: String) -> (EvalResult, Option<String>) {
        if let Some(ref mut sandbox) = self.sandbox {
            match sandbox.execute(line) {
                Ok(res) => (
                    res.result,
                    if self.config.limits.show_metrics {
                        Some(format!(
                            "Finished in {:?} using {}K of memory",
                            res.time_taken,
                            res.memory_used / 1_000
                        ))
                    } else {
                        None
                    },
                ),
                Err(e) => (EvalResult::AnsiString(format!("{e}")), None),
            }
        } else {
            match rink_core::eval(&mut *self.local.lock().unwrap(), &*line) {
                Ok(v) => (
                    EvalResult::AnsiString(to_ansi_string(&self.config, &v)),
                    None,
                ),
                Err(QueryError::MissingDeps(deps)) => (EvalResult::MissingDeps(deps), None),
                Err(e) => (
                    EvalResult::AnsiString(to_ansi_string(&self.config, &e)),
                    None,
                ),
            }
        }
    }

    pub(crate) fn restart(&mut self) -> Result<()> {
        let ctx = crate::config::load(&self.config)?;
        *self.local.lock().unwrap() = ctx;
        if let Some(mut existing) = self.sandbox.take() {
            existing.terminate()?;
        }
        if self.config.limits.enabled {
            let mut sandbox = Sandbox::new(self.config.clone())?;
            sandbox.restart()?;
            self.sandbox = Some(sandbox);
        };
        Ok(())
    }

    pub(crate) fn terminate(&mut self) -> Result<()> {
        if let Some(ref mut sandbox) = self.sandbox {
            sandbox.terminate()?;
        }
        Ok(())
    }
}
