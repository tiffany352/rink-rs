// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use color_eyre::Result;
use eyre::{eyre, Report, WrapErr};
use reqwest::header::USER_AGENT;
use rink_core::context::Context;
use rink_core::{ast, date, gnu_units, CURRENCY_FILE, DATES_FILE, DEFAULT_FILE};
use serde_derive::Deserialize;
use serde_json;
use std::fs::{read_to_string, File};
use std::io::{ErrorKind, Read, Seek, SeekFrom};
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

fn file_to_string(mut file: File) -> Result<String> {
    let mut string = String::new();
    let _ = file.read_to_string(&mut string)?;
    Ok(string)
}

pub fn config_path(name: &'static str) -> Result<PathBuf> {
    let mut path = dirs::config_dir().ok_or_else(|| eyre!("Could not find config directory"))?;
    path.push("rink");
    path.push(name);
    Ok(path)
}

#[derive(Deserialize, Default)]
#[serde(default, deny_unknown_fields)]
pub struct Config {
    pub rink: Rink,
    pub currency: Currency,
}

#[derive(Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct Rink {
    /// Which prompt to render when run interactively.
    pub prompt: String,
}

#[derive(Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct Currency {
    /// Set to false to disable currency fetching entirely.
    pub enabled: bool,
    /// Which web endpoint should be used to download currency data?
    pub endpoint: String,
    /// How long to cache for.
    #[serde(with = "humantime_serde")]
    pub cache_duration: Duration,
    /// How long to wait before timing out requests.
    #[serde(with = "humantime_serde")]
    pub timeout: Duration,
}

impl Default for Currency {
    fn default() -> Self {
        Currency {
            enabled: true,
            endpoint: "https://rinkcalc.app/data/currency.json".to_owned(),
            cache_duration: Duration::from_secs(60 * 60), // 1 hour
            timeout: Duration::from_secs(2),
        }
    }
}

impl Default for Rink {
    fn default() -> Self {
        Rink {
            prompt: "> ".to_owned(),
        }
    }
}

fn read_from_search_path(filename: &str, paths: &[PathBuf]) -> Result<String> {
    for path in paths {
        let mut buf = PathBuf::from(path);
        buf.push(filename);
        if let Ok(result) = read_to_string(buf) {
            return Ok(result);
        }
    }

    Err(eyre!(
        "Could not find {} in search path. Paths:{}",
        filename,
        paths
            .iter()
            .map(|path| format!("{}", path.display()))
            .collect::<Vec<String>>()
            .join("\n  ")
    ))
}

fn load_live_currency(config: &Currency) -> Result<ast::Defs> {
    let file = cached(
        "currency.json",
        &config.endpoint,
        config.cache_duration,
        config.timeout,
    )?;
    let contents = file_to_string(file)?;
    serde_json::from_str(&contents).wrap_err("Invalid JSON")
}

pub fn read_config() -> Result<Config> {
    match read_to_string(config_path("config.toml")?) {
        // Hard fail if the file has invalid TOML.
        Ok(result) => toml::from_str(&result).wrap_err("While parsing config.toml"),
        // Use default config if it doesn't exist.
        Err(err) if err.kind() == ErrorKind::NotFound => Ok(Config::default()),
        // Hard fail for other IO errors (e.g. permissions).
        Err(err) => Err(eyre!(err).wrap_err("Failed to read config.toml")),
    }
}

/// Creates a context by searching standard directories
pub fn load(config: &Config) -> Result<Context> {
    let mut search_path = vec![PathBuf::from("./")];
    if let Some(config_dir) = dirs::config_dir() {
        search_path.push(config_dir);
    }

    // Read definitions.units
    let units = read_from_search_path("definitions.units", &search_path)
        .or_else(|err| DEFAULT_FILE.map(ToOwned::to_owned).ok_or(err).wrap_err("Rink was not built with a bundled definitions file, and one was not found in the search path."))?;

    // Read datepatterns.txt
    let dates = read_from_search_path("datepatterns.txt", &search_path)
        .unwrap_or_else(|_| DATES_FILE.to_owned());

    let mut ctx = Context::new();
    ctx.load(gnu_units::parse_str(&units));
    ctx.load_dates(date::parse_datefile(&dates));

    // Load currency data.
    if config.currency.enabled {
        match load_live_currency(&config.currency) {
            Ok(mut live_defs) => {
                let mut base_defs = gnu_units::parse_str(
                    &read_from_search_path("currency.units", &search_path)
                        .unwrap_or_else(|_| CURRENCY_FILE.to_owned()),
                );
                let mut defs = vec![];
                defs.append(&mut base_defs.defs);
                defs.append(&mut live_defs.defs);
                ctx.load(ast::Defs { defs });
            }
            Err(err) => {
                println!("{:?}", err.wrap_err("Failed to load currency data"));
            }
        }
    }

    Ok(ctx)
}

fn read_if_current(file: File, expiration: Duration) -> Result<File> {
    use std::time::SystemTime;

    let stats = file.metadata()?;
    let mtime = stats.modified()?;
    let now = SystemTime::now();
    let elapsed = now.duration_since(mtime)?;
    if elapsed > expiration {
        Err(eyre!("File is out of date"))
    } else {
        Ok(file)
    }
}

fn download_to_file(path: &Path, url: &str, timeout: Duration) -> Result<File> {
    use std::fs::create_dir_all;

    create_dir_all(path.parent().unwrap())?;

    let client = reqwest::blocking::Client::builder()
        .timeout(timeout)
        .build()
        .wrap_err("Creating HTTP client")?;

    let mut response = client
        .get(url)
        .header(
            USER_AGENT,
            format!(
                "rink-cli {} <{}>",
                env!("CARGO_PKG_VERSION"),
                env!("CARGO_PKG_REPOSITORY")
            ),
        )
        .send()?;

    let status = response.status();
    if !status.is_success() {
        return Err(eyre!(
            "Received status {} while downloading {}",
            status,
            url
        ));
    }
    let mut temp_file = tempfile::NamedTempFile::new()?;
    response
        .copy_to(temp_file.as_file_mut())
        .wrap_err_with(|| format!("While dowloading {}", url))?;

    temp_file.as_file_mut().sync_all()?;
    temp_file.as_file_mut().seek(SeekFrom::Start(0))?;
    Ok(temp_file
        .persist(path)
        .wrap_err("Failed to write to cache dir")?)
}

fn cached(filename: &str, url: &str, expiration: Duration, timeout: Duration) -> Result<File> {
    let mut path = dirs::cache_dir().ok_or_else(|| eyre!("Could not find cache directory"))?;
    path.push("rink");
    path.push(filename);

    // 1. Return file if it exists and is up to date.
    // 2. Try to download a new version of the file and return it.
    // 3. If that fails, return the stale file if it exists.

    if let Ok(file) = File::open(&path) {
        if let Ok(result) = read_if_current(file, expiration) {
            return Ok(result);
        }
    }

    let err = match download_to_file(&path, url, timeout) {
        Ok(result) => return Ok(result),
        Err(err) => err,
    };

    if let Ok(file) = File::open(&path) {
        // Indicate error even though we're returning success.
        println!(
            "{:?}",
            Report::wrap_err(
                err,
                format!("Failed to refresh {}, using stale version", filename)
            )
        );
        Ok(file)
    } else {
        Err(err).wrap_err_with(|| format!("Failed to fetch {}", filename))
    }
}
