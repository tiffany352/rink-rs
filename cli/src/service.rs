use crate::{config::Config, fmt::to_ansi_string, GLOBAL};
use rink_core::{eval, Context};
use rink_sandbox::Service;
use std::sync::{Arc, Mutex};

pub struct RinkService {
    config: Config,
    ctx: Arc<Mutex<Context>>,
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

        let ctx = Arc::new(Mutex::new(crate::config::load(&config).unwrap()));
        Ok(RinkService { config, ctx })
    }

    fn handle(&self, request: Self::Req) -> Self::Res {
        let mut ctx = self.ctx.lock().unwrap();
        match eval(&mut ctx, &request) {
            Ok(value) => Ok(to_ansi_string(&self.config, &value)),
            Err(err) => Err(to_ansi_string(&self.config, &err)),
        }
    }

    fn timeout(config: &Self::Config) -> std::time::Duration {
        config.limits.timeout
    }
}

pub fn run_service() -> eyre::Result<()> {
    rink_sandbox::become_child::<RinkService, _>(&crate::GLOBAL);
}
