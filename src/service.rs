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
        GLOBAL.set_limit(config.limits.memory);

        let ctx = Arc::new(Mutex::new(crate::config::load(&config).unwrap()));
        Ok(RinkService { ctx, config })
    }

    fn handle(&self, request: Self::Req) -> Self::Res {
        let mut ctx = self.ctx.lock().unwrap();
        match eval(&mut ctx, &request) {
            Ok(value) => Ok(to_ansi_string(&self.config, &value)),
            Err(err) => Err(to_ansi_string(&self.config, &err)),
        }
    }
}

pub fn run_service() -> eyre::Result<()> {
    rink_sandbox::become_child::<RinkService>(&crate::GLOBAL);
}
