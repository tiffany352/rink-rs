use std::ffi::OsString;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::PathBuf;
use std::time::Duration;
use std::{fs::File, path::Path};

use crate::config::{read_from_search_path, Currency};
use color_eyre::Result;
use curl::easy::Easy;
use eyre::{eyre, Context as _, Report};
use rink_core::{Context, CURRENCY_FILE};

/// Returns path to currency.json
pub fn currency_json_path() -> Result<PathBuf> {
    let mut path = dirs::cache_dir().ok_or_else(|| eyre!("Could not find cache directory"))?;
    path.push("rink");
    path.push("currency.json");
    Ok(path)
}

fn file_to_string(mut file: File) -> Result<String> {
    let mut string = String::new();
    let _size = file.read_to_string(&mut string)?;
    Ok(string)
}

/// Used when rink is run with `--fetch-currency`
pub fn force_fetch_currency(config: &Currency) -> Result<String> {
    println!("Fetching...");
    let start = std::time::Instant::now();
    let path = currency_json_path()?;
    let file = download_to_file(&path, &config.endpoint, config.timeout)
        .wrap_err("Fetching currency data failed")?;
    let delta = std::time::Instant::now() - start;
    let metadata = file
        .metadata()
        .wrap_err("Fetched currency file, but failed to read file metadata")?;
    let len = metadata.len();
    Ok(format!(
        "Fetched {len} byte currency file after {}ms",
        delta.as_millis()
    ))
}

/// Loads currency units into the Context. Uses live data if it's fresh.
pub(crate) fn load_cached_currency_if_current(
    config: &Currency,
    ctx: &mut Context,
    search_path: &[PathBuf],
) -> Result<()> {
    let base = read_from_search_path("currency.units", search_path, CURRENCY_FILE)?
        .into_iter()
        .collect::<Vec<_>>()
        .join("\n");
    let cached = load_cached_if_current(config.expiration())?;
    let contents = if let CurrencyStatus::Found(cached) = cached {
        Some(file_to_string(cached)?)
    } else {
        None
    };
    ctx.load_currency(contents.as_ref().map(|s| &s[..]), &base.as_str())
        .map_err(|err| eyre!("{err}"))?;
    Ok(())
}

pub enum CurrencyStatus {
    Found(File),
    NotFound,
    Expired,
}

/// Returns the cached currency.json if it was found and it wasn't expired.
pub fn load_cached_if_current(expiration: Option<Duration>) -> Result<CurrencyStatus> {
    use std::time::SystemTime;

    let path = currency_json_path()?;
    let file = match File::open(&path) {
        Ok(file) => file,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Ok(CurrencyStatus::NotFound)
        }
        Err(err) => return Err(err.into()),
    };
    let stats = file.metadata()?;
    let mtime = stats.modified()?;
    let now = SystemTime::now();
    let elapsed = now.duration_since(mtime)?;
    if let Some(expiration) = expiration {
        if elapsed > expiration {
            return Ok(CurrencyStatus::Expired);
        }
    }
    Ok(CurrencyStatus::Found(file))
}

pub fn download_to_file(path: &Path, url: &str, timeout: Duration) -> Result<File> {
    use std::fs::create_dir_all;

    create_dir_all(path.parent().unwrap())?;

    // Given a filename like `foo.json`, names the temp file something
    // similar, like `foo.ABCDEF.json`.
    let mut prefix = path.file_stem().unwrap().to_owned();
    prefix.push(".");
    let mut suffix = OsString::from(".");
    suffix.push(path.extension().unwrap());

    // The temp file should be created in the same directory as its
    // final home, as the default temporary file directory might not be
    // the same filesystem.
    let mut temp_file = tempfile::Builder::new()
        .prefix(&prefix)
        .suffix(&suffix)
        .tempfile_in(path.parent().unwrap())?;

    let mut easy = Easy::new();
    easy.url(url)?;
    easy.useragent(&format!(
        "rink-cli {} <{}>",
        env!("CARGO_PKG_VERSION"),
        env!("CARGO_PKG_REPOSITORY")
    ))?;
    easy.timeout(timeout)?;

    let mut write_handle = temp_file.as_file_mut().try_clone()?;
    easy.write_function(move |data| {
        write_handle
            .write(data)
            .map_err(|_| curl::easy::WriteError::Pause)
    })?;
    easy.perform()?;

    let status = easy.response_code()?;
    if status != 200 {
        return Err(eyre!(
            "Received status {} while downloading {}",
            status,
            url
        ));
    }

    temp_file.as_file_mut().sync_all()?;
    temp_file.as_file_mut().seek(SeekFrom::Start(0))?;

    temp_file
        .persist(path)
        .wrap_err("Failed to write to cache dir")
}

/// Returns the cached currency.json if it's fresh,
/// otherwise tries to download it.
/// If the download fails, tries to use the stale cached version.
pub(crate) fn load_live_currency(config: &Currency) -> Result<File> {
    let expiration = config.expiration();
    if let Ok(CurrencyStatus::Found(file)) = load_cached_if_current(expiration) {
        return Ok(file);
    }

    let path = currency_json_path()?;
    let err = match download_to_file(&path, &config.endpoint, config.timeout) {
        Ok(result) => return Ok(result),
        Err(err) => err,
    };

    if let Ok(file) = File::open(&path) {
        // Indicate error even though we're returning success.
        println!(
            "{:?}",
            Report::wrap_err(
                err,
                format!("Failed to fetch {}, using stale version", config.endpoint)
            )
        );
        Ok(file)
    } else {
        Err(err).wrap_err_with(|| format!("Failed to fetch {}", config.endpoint))
    }
}
