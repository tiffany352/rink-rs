// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

/*! The primary interface of this library is meant to expose a very
simple command-reply model for frontends, and to allow gradual
addition of more advanced functionality. For now, only basic
functionality exists.

Using Rink as a library for uses other than simple unit conversion
tools is not currently well supported, and if you wish to do so,
please make issues for any problems you have.

There are currently a number of hardcoded `println!`s and `unwrap()`s
because most of this code was written in a day without much thought
towards making it into a library.

For basic flow of the library, the `bins/` directory has a few
examples. The REPL is very simple, and the IRC bot shows how Rink can
work in a multi-user environment without sessions or meaningful
stdin/stdout.

## Example

```rust
use rink_core::*;

let mut ctx = load().unwrap();
println!("{}", one_line(&mut ctx, "kWh / year -> W").unwrap());
```
*/

// Maybe someday we can be clean against this.
#![allow(clippy::cognitive_complexity)]

#[cfg(feature = "sandbox")]
extern crate ipc_channel;
#[cfg(feature = "currency")]
extern crate json;
#[cfg(feature = "sandbox")]
extern crate libc;
#[cfg(feature = "currency")]
extern crate reqwest;
extern crate serde;

#[macro_use]
extern crate serde_derive;
use dirs;

pub mod ast;
pub mod bigint;
pub mod bigrat;
#[cfg(feature = "currency")]
pub mod btc;
pub mod context;
#[cfg(feature = "currency")]
pub mod currency;
pub mod date;
pub mod eval;
pub mod factorize;
pub mod formula;
pub mod gnu_units;
pub mod load;
pub mod num;
pub mod number;
pub mod reply;
pub mod search;
pub mod substance;
pub mod text_query;
pub mod value;

pub use crate::context::Context;
pub use crate::number::Number;
pub use crate::value::Value;

use std::collections::BTreeMap;
use std::fs::File;
use std::path::PathBuf;
use std::time::Duration;

const DATA_FILE_URL: &str =
    "https://raw.githubusercontent.com/tiffany352/rink-rs/master/definitions.units";

pub fn config_dir() -> Result<PathBuf, String> {
    dirs::config_dir()
        .map(|mut x: PathBuf| {
            x.push("rink");
            x
        })
        .ok_or_else(|| "Could not find config directory".into())
}

#[cfg(feature = "currency")]
fn load_currency() -> Option<Result<ast::Defs, String>> {
    Some(currency::load())
}

#[cfg(not(feature = "currency"))]
fn load_currency() -> Option<Result<ast::Defs, String>> {
    None
}

#[cfg(feature = "currency")]
fn load_btc() -> Option<Result<ast::Defs, String>> {
    Some(btc::load())
}

#[cfg(not(feature = "currency"))]
fn load_btc() -> Option<Result<ast::Defs, String>> {
    None
}

#[cfg(feature = "gpl")]
static DEFAULT_FILE: Option<&'static str> = Some(include_str!("../../definitions.units"));
#[cfg(not(feature = "gpl"))]
static DEFAULT_FILE: Option<&'static str> = None;

static DATES_FILE: &str = include_str!("../../datepatterns.txt");
static CURRENCY_FILE: &str = include_str!("../../currency.units");

/// Creates a context by searching standard directories for definitions.units.
pub fn load() -> Result<Context, String> {
    use std::io::Read;
    use std::path::Path;

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
    let ecb = load_currency();
    let btc = load_btc();
    let currency_defs = {
        let defs = load(Path::new("currency.units").to_path_buf())
            .or_else(|_| load(path.join("currency.units")))
            .unwrap_or_else(|_| CURRENCY_FILE.to_owned());
        let mut iter = gnu_units::TokenIterator::new(&*defs).peekable();
        gnu_units::parse(&mut iter)
    };
    let currency = {
        let mut defs = vec![];
        if let Some(Ok(mut ecb)) = ecb {
            defs.append(&mut ecb.defs)
        } else if let Some(Err(e)) = ecb {
            println!("Failed to load ECB currency data: {}", e);
        }
        if let Some(Ok(mut btc)) = btc {
            defs.append(&mut btc.defs)
        } else if let Some(Err(e)) = btc {
            println!("Failed to load BTC currency data: {}", e);
        }
        let mut currency_defs = currency_defs;
        defs.append(&mut currency_defs.defs);
        ast::Defs { defs }
    };

    let mut ctx = context::Context::new();
    ctx.load(units);
    ctx.load_dates(dates);
    ctx.load(currency);
    Ok(ctx)
}

/// Evaluates a single line within a context.
pub fn one_line(ctx: &mut Context, line: &str) -> Result<String, String> {
    let mut iter = text_query::TokenIterator::new(line.trim()).peekable();
    let expr = text_query::parse_query(&mut iter);
    let res = ctx.eval_outer(&expr);
    res.as_ref()
        .map(ToString::to_string)
        .map_err(ToString::to_string)
}

#[cfg(feature = "sandbox")]
pub fn one_line_sandbox(line: &str) -> String {
    use ipc_channel::ipc::{IpcOneShotServer, IpcSender};
    use libc;
    use std::io::Error;

    pub unsafe fn fork<F: FnOnce()>(child_func: F) -> libc::pid_t {
        match libc::fork() {
            -1 => panic!("Fork failed: {}", Error::last_os_error()),
            0 => {
                child_func();
                unreachable!()
            }
            pid => pid,
        }
    }

    println!("Executing query: {}", line);

    let (server, server_name) = IpcOneShotServer::new().unwrap();

    let child = || {
        println!("Child connecting..");
        let tx = IpcSender::connect(server_name).unwrap();
        println!("Child connected");

        tx.send("".to_owned()).unwrap();

        unsafe {
            let limit = libc::rlimit {
                // 100 megabytes
                rlim_cur: 100_000_000,
                rlim_max: 100_000_000,
            };
            let res = libc::setrlimit(libc::RLIMIT_AS, &limit);
            if res == -1 {
                panic!("Setrlimit RLIMIT_AS failed: {}", Error::last_os_error())
            }
            let limit = libc::rlimit {
                // 15 seconds
                rlim_cur: 15,
                rlim_max: 15,
            };
            let res = libc::setrlimit(libc::RLIMIT_CPU, &limit);
            if res == -1 {
                panic!("Setrlimit RLIMIT_AS failed: {}", Error::last_os_error())
            }
        }

        let mut ctx = load().unwrap();
        ctx.short_output = true;
        let reply = match one_line(&mut ctx, line) {
            Ok(v) => v,
            Err(e) => e,
        };
        tx.send(reply).unwrap();
        println!("Wrote reply");

        ::std::process::exit(0)
    };

    let pid = unsafe { fork(child) };

    println!("Child started");

    let (rx, _) = server.accept().unwrap();

    println!("Child accepted");

    let status = unsafe {
        let mut status = 0;
        let res = libc::waitpid(pid, &mut status, 0);
        if res == -1 {
            panic!("Waitpid failed: {}", Error::last_os_error())
        }
        if libc::WIFEXITED(status) {
            println!(
                "Child exited normally with status {}",
                libc::WEXITSTATUS(status)
            );
        }
        if libc::WIFSIGNALED(status) {
            println!("Child was killed by signal {}", libc::WTERMSIG(status));
        }
        status
    };

    let res = match rx.try_recv() {
        Ok(res) => res,
        Err(_)
            if unsafe { libc::WIFSIGNALED(status) && libc::WTERMSIG(status) == libc::SIGXCPU } =>
        {
            "Calculation timed out".to_string()
        }
        // :(
        Err(ref e) if e.to_string() == "IoError: Connection reset by peer (os error 104)" => {
            "Calculation ran out of memory".to_string()
        }
        Err(e) => e.to_string(),
    };

    println!("Calculation result: {:?}", res);

    res
}

fn btree_merge<K: ::std::cmp::Ord + Clone, V: Clone, F: Fn(&V, &V) -> Option<V>>(
    left: &BTreeMap<K, V>,
    right: &BTreeMap<K, V>,
    merge_func: F,
) -> BTreeMap<K, V> {
    let mut res = BTreeMap::new();
    let mut a = left.iter().peekable();
    let mut b = right.iter().peekable();
    loop {
        match (a.peek().cloned(), b.peek().cloned()) {
            (Some((akey, aval)), Some((bkey, bval))) if akey == bkey => {
                if let Some(v) = merge_func(aval, bval) {
                    res.insert(akey.clone(), v);
                }
                a.next();
                b.next();
            }
            (Some((akey, _)), Some((bkey, bval))) if akey > bkey => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            }
            (Some((akey, aval)), Some((bkey, _))) if akey < bkey => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            }
            (Some(_), Some(_)) => unreachable!(),
            (None, Some((bkey, bval))) => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            }
            (Some((akey, aval)), None) => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            }
            (None, None) => break,
        }
    }
    res
}

#[cfg(feature = "reqwest")]
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
            let mut f = File::create(tmppath.clone()).map_err(|x| x.to_string())?;

            let client = reqwest::Client::builder()
                .gzip(true)
                .timeout(Duration::from_secs(2))
                .build()
                .map_err(|err| format!("Failed to create http client: {}", err))?;

            client
                .get(url)
                .send()
                .map_err(|err| format!("Request failed: {}", err))?
                .copy_to(&mut f)
                .map_err(|err| format!("Request failed: {}", err))?;

            f.sync_all().map_err(|x| format!("{}", x))?;
            drop(f);
            fs::rename(tmppath.clone(), path.clone()).map_err(|x| x.to_string())?;
            File::open(path.clone()).map_err(|x| x.to_string())
        })
        // If the request fails then try to reuse the already cached file
        .or_else(|_| File::open(path.clone()).map_err(ts))
}
