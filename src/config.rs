// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use dirs;
use rink_core::ast;
use rink_core::context::Context;
use rink_core::date;
use rink_core::gnu_units;
use rink_core::{CURRENCY_FILE, DATES_FILE, DEFAULT_FILE};
use serde_json;
use std::fs::File;
use std::io::ErrorKind;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

const DATA_FILE_URL: &str =
    "https://raw.githubusercontent.com/tiffany352/rink-rs/master/definitions.units";
const CURRENCY_URL: &str = "https://rinkcalc.app/data/currency.json";

pub fn config_dir() -> Result<PathBuf, String> {
    dirs::config_dir()
        .map(|mut x: PathBuf| {
            x.push("rink");
            x
        })
        .ok_or_else(|| "Could not find config directory".into())
}

fn read_to_string(mut file: File) -> Result<String, String> {
    let mut buf = String::new();
    match file.read_to_string(&mut buf) {
        Ok(_size) => Ok(buf),
        Err(e) => Err(e.to_string()),
    }
}

/// Creates a context by searching standard directories for definitions.units.
pub fn load() -> Result<Context, String> {
    let path = config_dir()?;
    let load = |name| {
        File::open(name).and_then(|mut f| {
            let mut buf = vec![];
            f.read_to_end(&mut buf)?;
            Ok(String::from_utf8_lossy(&*buf).into_owned())
        })
    };
    let units = load(Path::new("definitions.units").to_path_buf())
        .or_else(|_| load(path.join("definitions.units")))
        .or_else(|_| {
            DEFAULT_FILE.map(|x| x.to_owned()).ok_or_else(|| {
                "Did not exist in search path and binary is not compiled with `gpl` feature"
                    .to_string()
            })
        })
        .map_err(|e| {
            format!(
                "Failed to open definitions.units: {}\n\
                 If you installed with `gpl` disabled, then you need to obtain definitions.units \
                 separately. Here is the URL, download it and put it in {:?}.\n\
                 \n\
                 {}\n\
                 \n",
                e, &path, DATA_FILE_URL
            )
        });
    let units = units?;
    let dates = load(Path::new("datepatterns.txt").to_path_buf())
        .or_else(|_| load(path.join("datepatterns.txt")))
        .unwrap_or_else(|_| DATES_FILE.to_owned());

    let mut iter = gnu_units::TokenIterator::new(&*units).peekable();
    let units = gnu_units::parse(&mut iter);
    let dates = date::parse_datefile(&*dates);
    let currency = cached(
        "currency.json",
        CURRENCY_URL,
        Duration::from_secs(23 * 60 * 60),
    )
    .and_then(read_to_string)
    .and_then(|file| serde_json::from_str::<ast::Defs>(&file).map_err(|e| e.to_string()));
    let mut currency_defs = {
        let defs = load(Path::new("currency.units").to_path_buf())
            .or_else(|_| load(path.join("currency.units")))
            .unwrap_or_else(|_| CURRENCY_FILE.to_owned());
        let mut iter = gnu_units::TokenIterator::new(&*defs).peekable();
        gnu_units::parse(&mut iter)
    };
    let currency = {
        let mut defs = vec![];
        if let Ok(mut currency) = currency {
            defs.append(&mut currency.defs);
            defs.append(&mut currency_defs.defs);
        } else if let Err(e) = currency {
            println!("Failed to load live currency data: {}", e);
        }
        ast::Defs { defs }
    };

    let mut ctx = Context::new();
    ctx.load(units);
    ctx.load_dates(dates);
    ctx.load(currency);
    Ok(ctx)
}

fn cached(file: &str, url: &str, expiration: Duration) -> Result<File, String> {
    use std::fmt::Display;
    use std::fs;
    use std::time::SystemTime;

    fn ts<T: Display>(x: T) -> String {
        x.to_string()
    }
    let mut path = config_dir()?;
    let mut tmppath = path.clone();
    path.push(file);
    let tmpfile = format!("{}.part", file);
    tmppath.push(tmpfile);

    File::open(path.clone())
        .map_err(ts)
        .and_then(|f| {
            let stats = f.metadata().map_err(ts)?;
            let mtime = stats.modified().map_err(ts)?;
            let now = SystemTime::now();
            let elapsed = now.duration_since(mtime).map_err(ts)?;
            if elapsed > expiration {
                Err("File is out of date".to_string())
            } else {
                Ok(f)
            }
        })
        .or_else(|_| {
            fs::create_dir_all(path.parent().unwrap()).map_err(|x| x.to_string())?;

            let client = reqwest::Client::builder()
                .gzip(true)
                .timeout(Duration::from_secs(2))
                .build()
                .map_err(|err| format!("Failed to create http client: {}", err))?;

            let mut response = client
                .get(url)
                .send()
                .map_err(|err| format!("Request failed: {}", err))?;

            let status = response.status();
            if !status.is_success() {
                return Err(format!("Requested failed with {}", status));
            }
            let mut f = File::create(tmppath.clone()).map_err(|x| x.to_string())?;

            response
                .copy_to(&mut f)
                .map_err(|err| format!("Request failed: {}", err))?;

            f.sync_all().map_err(|x| format!("{}", x))?;
            drop(f);
            fs::rename(tmppath.clone(), path.clone()).map_err(|x| x.to_string())?;
            File::open(path.clone()).map_err(|x| x.to_string())
        })
        // If the request fails then try to reuse the already cached file
        .or_else(|orig| match File::open(path.clone()) {
            Ok(file) => Ok(file),
            Err(err) if err.kind() == ErrorKind::NotFound => Err(orig),
            Err(err) => Err(err.to_string()),
        })
}
