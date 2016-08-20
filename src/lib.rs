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
use rink::*;

let mut ctx = load().unwrap();
println!("{}", one_line(&mut ctx, "kWh / year -> W").unwrap());
```
*/

extern crate gmp;
extern crate chrono;
#[cfg(feature = "chrono-humanize")]
extern crate chrono_humanize;
#[cfg(feature = "sandbox")]
extern crate libc;
#[cfg(feature = "sandbox")]
extern crate ipc_channel;

pub mod unit_defs;
pub mod eval;
pub mod number;
pub mod date;
pub mod factorize;
pub mod gnu_units;
pub mod ast;

pub use number::Number;
pub use eval::{Context, Value};

use std::env;
use std::convert::From;
use std::path::PathBuf;
use std::collections::BTreeMap;

const UNITS_TXT_URL: &'static str = "https://raw.githubusercontent.com/tiffany352/rink-rs/master/units.txt";

#[cfg(target_os = "linux")]
fn config_dir() -> Result<PathBuf, String> {
    env::var("XDG_CONFIG_HOME")
        .map(From::from)
        .or_else(|_| {
            env::home_dir()
                .ok_or("Home dir not present".to_owned())
                .map(From::from)
                .map(|mut x: PathBuf| { x.push(".config/"); x })
        })
}

#[cfg(target_os = "windows")]
fn config_dir() -> Result<PathBuf, String> {
    env::var("APPDATA")
        .map(From::from)
        .ok_or_else(|_| {
            env::home_dir()
                .ok_or("Home dir not present".to_owned())
                .map(From::from)
                .map(|mut x: PathBuf| { x.push("AppData\\Roaming"); x})
        })
}

#[cfg(target_os = "macos")]
fn config_dir() -> Result<PathBuf, String> {
    env::home_dir()
        .ok_or("Home dir not present".to_owned())
        .map(From::from)
        .map(|mut x: PathBuf| { x.push("Library/Application Support"); x})
}

/// Creates a context by searching standard directories for units.txt.
pub fn load() -> Result<Context, String> {
    use std::io::Read;
    use std::fs::File;

    let f = File::open("definitions.units");
    let mut f = match f {
        Ok(f) => f,
        Err(_) => {
            let mut path = try!(config_dir());
            path.push("rink/definitions.units");
            let f = File::open(&path);
            match f {
                Ok(f) => f,
                Err(e) => return Err(format!(
                    "Failed to open definitions.units: {}\nIf you installed using \
                     `cargo install`, then you need to obtain units.txt separately. Here is the \
                     URL, download it and put it in {:?}.\n\n{}\n\n",
                    e, &path, UNITS_TXT_URL))
            }
        }
    };

    let mut buf = vec![];
    f.read_to_end(&mut buf).unwrap();
    let string = String::from_utf8_lossy(&*buf);
    //let mut iter = unit_defs::TokenIterator::new(&*string).peekable();
    //let res = unit_defs::parse(&mut iter);
    let mut iter = gnu_units::TokenIterator::new(&*string).peekable();
    let res = gnu_units::parse(&mut iter);

    Ok(eval::Context::new(res))
}

/// Evaluates a single line within a context.
pub fn one_line(ctx: &mut Context, line: &str) -> Result<String, String> {
    let mut iter = unit_defs::TokenIterator::new(line.trim()).peekable();
    let expr = unit_defs::parse_expr(&mut iter);
    ctx.eval_outer(&expr)
}

#[cfg(feature = "sandbox")]
pub fn one_line_sandbox(line: &str) -> String {
    use libc;
    use std::io::Error;
    use ipc_channel::ipc::{IpcOneShotServer, IpcSender};

    pub unsafe fn fork<F: FnOnce()>(child_func: F) -> libc::pid_t {
        match libc::fork() {
            -1 => panic!("Fork failed: {}", Error::last_os_error()),
            0 => { child_func(); unreachable!() },
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
                rlim_max: 15
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
            Err(e) => e
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
            println!("Child exited normally with status {}", libc::WEXITSTATUS(status));
        }
        if libc::WIFSIGNALED(status) {
            println!("Child was killed by signal {}", libc::WTERMSIG(status));
        }
        status
    };

    let res = match rx.try_recv() {
        Ok(res) => res,
        Err(_) if unsafe { libc::WIFSIGNALED(status) && libc::WTERMSIG(status) == libc::SIGXCPU } =>
            format!("Calculation timed out"),
        // :(
        Err(ref e) if format!("{}", e) == "IoError: Connection reset by peer (os error 104)" =>
            format!("Calculation ran out of memory"),
        Err(e) => format!("{}", e)
    };

    println!("Calculation result: {:?}", res);

    res
}

fn btree_merge<K: ::std::cmp::Ord+Clone, V:Clone, F:Fn(&V, &V) -> Option<V>>(
    left: &BTreeMap<K, V>, right: &BTreeMap<K, V>, merge_func: F
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
            },
            (Some((akey, _)), Some((bkey, bval))) if akey > bkey => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            },
            (Some((akey, aval)), Some((bkey, _))) if akey < bkey => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            },
            (Some(_), Some(_)) => panic!(),
            (None, Some((bkey, bval))) => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            },
            (Some((akey, aval)), None) => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            },
            (None, None) => break,
        }
    }
    res
}
