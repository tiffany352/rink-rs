// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::config::Config;
use crate::fmt::to_ansi_string;
use crate::GLOBAL;
use rink_core::output::QueryError;
use rink_core::runtime::MissingDeps;
use rink_core::{eval, Context};
use rink_sandbox::Service;
use serde_derive::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};

pub struct RinkService {
    config: Config,
    ctx: Arc<Mutex<Context>>,
}

#[derive(Serialize, Deserialize)]
pub enum EvalResult {
    AnsiString(String),
    MissingDeps(MissingDeps),
}

impl Service for RinkService {
    type Req = String;
    type Res = EvalResult;
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

        let ctx = Arc::new(Mutex::new(crate::config::load(&config).unwrap()));
        Ok(RinkService { config, ctx })
    }

    fn handle(&self, request: Self::Req) -> Self::Res {
        let mut ctx = self.ctx.lock().unwrap();
        match eval(&mut ctx, &request) {
            Ok(value) => EvalResult::AnsiString(to_ansi_string(&self.config, &value)),
            Err(QueryError::MissingDeps(deps)) => EvalResult::MissingDeps(deps),
            Err(err) => EvalResult::AnsiString(to_ansi_string(&self.config, &err)),
        }
    }

    fn timeout(config: &Self::Config) -> std::time::Duration {
        config.limits.timeout
    }
}

pub fn run_service() -> eyre::Result<()> {
    rink_sandbox::become_child::<RinkService, _>(&crate::GLOBAL);
}
