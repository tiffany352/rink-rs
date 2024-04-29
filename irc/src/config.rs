use irc::client::data::Config as IrcConfig;
use serde_derive::{Deserialize, Serialize};
use std::time::Duration;
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

#[derive(Serialize, Deserialize, Clone, Default)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    pub limits: Limits,
    pub servers: Vec<IrcConfig>,
}
