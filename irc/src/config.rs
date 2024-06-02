// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use irc::client::data::Config as IrcConfig;
use rink_core::{parsing::datetime, Context};
use serde_derive::{Deserialize, Serialize};
use std::{path::PathBuf, time::Duration};
use ubyte::ByteUnit;

#[derive(Serialize, Deserialize, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Limits {
    /// Whether support for sandboxing is enabled.
    pub enabled: bool,
    /// Whether performance metrics are shown after each query.
    pub show_metrics: bool,
    /// The maximum amount of heap that can be used per query, in megabytes.
    pub memory: ByteUnit,
    /// How long to wait before cancelling a query.
    #[serde(with = "humantime_serde")]
    pub timeout: Duration,
}

impl Default for Limits {
    fn default() -> Self {
        Limits {
            enabled: true,
            show_metrics: true,
            memory: ByteUnit::Megabyte(20),
            timeout: Duration::from_secs(10),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Currency {
    pub enabled: bool,
    pub path: PathBuf,
}

impl Default for Currency {
    fn default() -> Self {
        Currency {
            enabled: false,
            path: PathBuf::from("./currency.json"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Behavior {
    pub follow_invites: bool,
}

impl Default for Behavior {
    fn default() -> Self {
        Behavior {
            follow_invites: true,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Default)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    pub limits: Limits,
    pub currency: Currency,
    pub behavior: Behavior,
    pub servers: Vec<IrcConfig>,
}

fn try_load_currency(config: &Currency, ctx: &mut Context) {
    let base = rink_core::CURRENCY_FILE.unwrap();
    let live = std::fs::read_to_string(&config.path).unwrap();

    ctx.load_currency(&live, &base).unwrap();
}

/// Creates a context by searching standard directories
pub fn load(config: &Config) -> Context {
    // Read definitions.units
    let units = rink_core::DEFAULT_FILE.unwrap();
    // Read datepatterns.txt
    let dates = rink_core::DATES_FILE.unwrap();

    let mut ctx = Context::new();
    ctx.save_previous_result = true;
    ctx.load_definitions(&units).unwrap();
    ctx.load_dates(datetime::parse_datefile(&dates));

    // Load currency data.
    if config.currency.enabled {
        try_load_currency(&config.currency, &mut ctx);
    }

    ctx
}
