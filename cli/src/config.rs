// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::currency::try_load_currency;
use crate::style_ser;
use color_eyre::Result;
use eyre::{eyre, WrapErr};
use nu_ansi_term::{Color, Style};
use rink_core::loader::gnu_units;
use rink_core::output::fmt::FmtToken;
use rink_core::parsing::datetime;
use rink_core::Context;
use rink_core::{DATES_FILE, DEFAULT_FILE};
use serde_derive::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::fs::read_to_string;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::time::Duration;
use ubyte::ByteUnit;

pub fn config_toml_path() -> Result<PathBuf> {
    let mut path = dirs::config_dir().ok_or_else(|| eyre!("Could not find config directory"))?;
    path.push("rink");
    path.push("config.toml");
    Ok(path)
}

pub fn currency_json_path() -> Result<PathBuf> {
    let mut path = dirs::cache_dir().ok_or_else(|| eyre!("Could not find cache directory"))?;
    path.push("rink");
    path.push("currency.json");
    Ok(path)
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    pub rink: Rink,
    pub currency: Currency,
    pub colors: Colors,
    pub limits: Limits,
    pub themes: HashMap<String, Theme>,
    // Hack because none of ansi-term's functionality is const safe.
    default_theme: Theme,
    disabled_theme: Theme,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Rink {
    /// Which prompt to render when run interactively.
    pub prompt: String,
    /// Use multi-line output for lists.
    pub long_output: bool,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CurrencyBehavior {
    Prompt,
    Always,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Currency {
    /// Set behavior for currency fetching, whether to prompt.
    pub behavior: CurrencyBehavior,
    /// Set to false to disable currency loading entirely.
    pub enabled: bool,
    /// Set to false to only reuse the existing cached currency data.
    #[serde(alias = "fetch_on_startup")]
    pub cache_expiration_enabled: bool,
    /// Which web endpoint should be used to download currency data?
    pub endpoint: String,
    /// How long to cache for.
    #[serde(with = "humantime_serde")]
    pub cache_duration: Duration,
    /// How long to wait before timing out requests.
    #[serde(with = "humantime_serde")]
    pub timeout: Duration,
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Colors {
    /// Whether support for colored output should be enabled.
    pub enabled: Option<bool>,
    /// The name of the current theme.
    pub theme: String,
}

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

#[derive(Serialize, Deserialize, Default, Clone)]
#[serde(default, deny_unknown_fields)]
pub struct Theme {
    #[serde(with = "style_ser")]
    plain: Style,
    #[serde(with = "style_ser")]
    error: Style,
    #[serde(with = "style_ser")]
    unit: Style,
    #[serde(with = "style_ser")]
    quantity: Style,
    #[serde(with = "style_ser")]
    number: Style,
    #[serde(with = "style_ser")]
    user_input: Style,
    #[serde(with = "style_ser")]
    doc_string: Style,
    #[serde(with = "style_ser")]
    pow: Style,
    #[serde(with = "style_ser")]
    prop_name: Style,
    #[serde(with = "style_ser")]
    date_time: Style,
    #[serde(with = "style_ser")]
    link: Style,
}

impl Theme {
    pub fn get_style(&self, token: FmtToken) -> Style {
        match token {
            FmtToken::Plain => self.plain,
            FmtToken::Error => self.error,
            FmtToken::Unit => self.unit,
            FmtToken::Quantity => self.quantity,
            FmtToken::Number => self.number,
            FmtToken::UserInput => self.user_input,
            FmtToken::DocString => self.doc_string,
            FmtToken::Pow => self.pow,
            FmtToken::PropName => self.prop_name,
            FmtToken::DateTime => self.date_time,
            FmtToken::Link => self.link,

            // Default styling since these are handled specially.
            FmtToken::ListBegin => self.plain,
            FmtToken::ListSep => self.plain,
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {
            rink: Default::default(),
            currency: Default::default(),
            colors: Default::default(),
            themes: Default::default(),
            limits: Default::default(),
            default_theme: Theme {
                plain: Style::default(),
                error: Style::new().fg(Color::Red),
                unit: Style::new().fg(Color::Cyan),
                quantity: Style::new().fg(Color::Cyan).dimmed(),
                number: Style::default(),
                user_input: Style::new().bold(),
                doc_string: Style::new().italic(),
                pow: Style::default(),
                prop_name: Style::new().fg(Color::Cyan),
                date_time: Style::default(),
                link: Style::new().fg(Color::Blue),
            },
            disabled_theme: Theme::default(),
        }
    }
}

impl Default for Currency {
    fn default() -> Self {
        Currency {
            behavior: CurrencyBehavior::Prompt,
            enabled: true,
            cache_expiration_enabled: true,
            endpoint: "https://rinkcalc.app/data/currency.json".to_owned(),
            cache_duration: Duration::from_secs(60 * 60), // 1 hour
            timeout: Duration::from_secs(15),
        }
    }
}

impl Default for Rink {
    fn default() -> Self {
        Rink {
            prompt: "> ".to_owned(),
            long_output: false,
        }
    }
}

impl Default for Colors {
    fn default() -> Self {
        Colors {
            enabled: None,
            theme: "default".to_owned(),
        }
    }
}

impl Default for Limits {
    fn default() -> Self {
        Limits {
            enabled: false,
            show_metrics: false,
            memory: ByteUnit::Megabyte(20),
            timeout: Duration::from_secs(10),
        }
    }
}

impl Config {
    pub fn get_theme(&self) -> &Theme {
        let default_enable_colors = env::var("NO_COLOR") == Err(env::VarError::NotPresent);
        let colors_enabled = self.colors.enabled.unwrap_or(default_enable_colors);

        if colors_enabled {
            let name = &self.colors.theme;
            let theme = self.themes.get(name);
            theme.unwrap_or(&self.default_theme)
        } else {
            &self.disabled_theme
        }
    }
}

pub(crate) fn read_from_search_path(
    filename: &str,
    paths: &[PathBuf],
    default: Option<&'static str>,
) -> Result<Vec<String>> {
    let result: Vec<String> = paths
        .iter()
        .filter_map(|path| {
            let mut buf = PathBuf::from(path);
            buf.push(filename);
            read_to_string(buf).ok()
        })
        .chain(default.map(ToOwned::to_owned))
        .collect();

    if result.is_empty() {
        Err(eyre!(
            "Could not find {}, and rink was not built with one bundled. Search path:{}",
            filename,
            paths
                .iter()
                .map(|path| format!("\n  {}", path.display()))
                .collect::<Vec<String>>()
                .join("")
        ))
    } else {
        Ok(result)
    }
}

pub fn read_config(override_path: Option<&str>) -> Result<Config> {
    let path = if let Some(path) = override_path {
        PathBuf::from(path)
    } else {
        config_toml_path()?
    };
    match read_to_string(path) {
        // Hard fail if the file has invalid TOML.
        Ok(result) => toml::from_str(&result).wrap_err("While parsing config.toml"),
        Err(err) if err.kind() == ErrorKind::NotFound => {
            if let Some(override_path) = override_path {
                // Hard fail if user-provided config path doesn't exist
                Err(eyre!(err).wrap_err(format!(
                    "Failed to read provided config file `{}`",
                    override_path
                )))
            } else {
                // Use default config if it doesn't exist.
                Ok(Config::default())
            }
        }
        // Hard fail for other IO errors (e.g. permissions).
        Err(err) => Err(eyre!(err).wrap_err("Failed to read config.toml")),
    }
}

/// Creates a context by searching standard directories
pub fn load(config: &Config) -> Result<Context> {
    let mut search_path = vec![PathBuf::from("./")];
    if let Some(mut config_dir) = dirs::config_dir() {
        config_dir.push("rink");
        search_path.push(config_dir);
    }
    if let Some(prefix) = option_env!("RINK_PATH") {
        search_path.push(prefix.into());
    }

    // Read definitions.units
    let units_files = read_from_search_path("definitions.units", &search_path, DEFAULT_FILE)?;

    let unit_defs: Vec<_> = units_files
        .iter()
        .map(|s| gnu_units::parse_str(s).defs)
        .flatten()
        .collect();

    // Read datepatterns.txt
    let dates = read_from_search_path("datepatterns.txt", &search_path, DATES_FILE)?
        .into_iter()
        .next()
        .unwrap();

    let mut ctx = Context::new();
    ctx.save_previous_result = true;
    ctx.load(rink_core::ast::Defs { defs: unit_defs })
        .map_err(|err| eyre!(err))?;
    ctx.load_dates(datetime::parse_datefile(&dates));

    // Load currency data.
    if config.currency.enabled {
        if let Err(err) = try_load_currency(&config.currency, &mut ctx, &search_path) {
            println!("{:?}", err.wrap_err("Failed to load currency data"));
        }
    }

    Ok(ctx)
}
