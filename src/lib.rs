pub mod unit_defs;
pub mod eval;

pub use eval::{Context, Value};

use std::env;
use std::convert::From;
use std::path::PathBuf;

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

pub fn load() -> Result<Context, String> {
    use std::io::Read;
    use std::fs::File;

    let f = File::open("units.txt");
    let mut f = match f {
        Ok(f) => f,
        Err(_) => {
            let mut path = try!(config_dir());
            path.push("rink/units.txt");
            let f = File::open(&path);
            match f {
                Ok(f) => f,
                Err(e) => return Err(format!(
                    concat!("Failed to open units.txt: {}\nIf you installed using `cargo install`, ",
                            "then you need to obtain units.txt separately. Here is the URL, ",
                            "download it and put it in {:?}.\n\n{}\n\n"),
                    e, &path, UNITS_TXT_URL))
            }
        }
    };

    let mut buf = vec![];
    f.read_to_end(&mut buf).unwrap();
    let string = String::from_utf8_lossy(&*buf);
    let mut iter = unit_defs::TokenIterator::new(&*string).peekable();
    let res = unit_defs::parse(&mut iter);

    Ok(eval::Context::new(res))
}

pub fn one_line(ctx: &mut Context, line: &str) -> Result<Value, String> {
    let mut iter = unit_defs::TokenIterator::new(line.trim()).peekable();
    let expr = unit_defs::parse_expr(&mut iter);
    ctx.eval(&expr)
}
