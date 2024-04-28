// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use clap::{Arg, ArgAction, Command};
use eyre::{Result, WrapErr};
use rink_sandbox::Alloc;
use std::fs::File;
use std::io::{stdin, BufReader};

pub use helper::RinkHelper;

pub mod config;
pub(crate) mod fmt;
pub mod helper;
pub mod repl;
pub(crate) mod service;
pub(crate) mod style_ser;

#[global_allocator]
pub(crate) static GLOBAL: Alloc = Alloc::new(usize::MAX);

#[async_std::main]
async fn main() -> Result<()> {
    let matches = Command::new("Rink")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Rink Contributors")
        .about("Unit conversion tool")
        .arg(
            Arg::new("EXPR")
                .help("Evaluate a list of expressions. If no arguments are provided, an interactive session will start.")
                .multiple_values(true)
                .required(false),
        )
        .arg(
            Arg::new("file")
                .short('f')
                .long("file")
                .takes_value(true)
                .help("Reads expressions from a file"),
        )
        .arg(
            Arg::new("config-path")
                .long("config-path")
                .help("Prints a path to the config file, then exits")
                .action(ArgAction::SetTrue)
        )
        .arg(
            Arg::new("dump")
                .long("dump")
                .help("Generates a file containing the contents of the Context object, then exits")
                .takes_value(true)
                .default_missing_value("dump.txt")
                .hide(true)
        )
        .arg(
            Arg::new("service")
                .long("service")
                .help("Start in service mode")
                .action(ArgAction::SetTrue)
                .hide(true)
        )
        .get_matches();

    if matches.get_flag("service") {
        return service::run_service();
    }
    // The panic handler can't be installed if entering service mode, so
    // it's placed after that check.
    color_eyre::install()?;
    let config = config::read_config()?;

    if let Some(filename) = matches.get_one::<String>("dump") {
        use std::io::Write;

        let ctx = config::load(&config)?;
        let mut file = std::fs::File::create(filename)?;
        writeln!(&mut file, "{:#?}", ctx)?;
        return Ok(());
    }

    if matches.get_flag("config-path") {
        println!("{}", config::config_path("config.toml").unwrap().display());
        Ok(())
    } else if let Some(filename) = matches.get_one::<&str>("file") {
        match *filename {
            "-" => {
                let stdin_handle = stdin();
                repl::noninteractive(stdin_handle.lock(), &config, false)
            }
            _ => {
                let file = File::open(&filename).wrap_err("Failed to open input file")?;
                repl::noninteractive(BufReader::new(file), &config, false)
            }
        }
    } else if let Some(exprs) = matches.get_many::<String>("EXPR") {
        let mut ctx = config::load(&config)?;
        for expr in exprs {
            println!("> {}", expr);
            match rink_core::one_line(&mut ctx, expr) {
                Ok(v) => println!("{}", v),
                Err(e) => println!("{}", e),
            }
        }
        Ok(())
    } else if config.limits.enabled {
        repl::interactive_sandboxed(config).await
    } else {
        repl::interactive(&config)
    }
}
