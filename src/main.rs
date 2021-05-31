// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use alloc::RinkAlloc;
use clap::{App, Arg};
use eyre::{Result, WrapErr};
use std::alloc::System;
use std::fs::File;
use std::io::{stdin, BufReader};

pub use helper::RinkHelper;

pub(crate) mod alloc;
pub mod config;
pub(crate) mod fmt;
pub mod helper;
pub mod repl;
pub(crate) mod sandbox;
pub(crate) mod style_de;

#[global_allocator]
pub static GLOBAL: RinkAlloc = RinkAlloc::new(System, usize::MAX);

#[async_std::main]
async fn main() -> Result<()> {
    color_eyre::install()?;

    let matches = App::new("Rink")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Rink Contributors")
        .about("Unit conversion tool")
        .arg(
            Arg::with_name("EXPR")
                .help("Evaluate a list of expressions. If no arguments are provided, an interactive session will start.")
                .multiple(true)
                .required(false),
        )
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .takes_value(true)
                .help("Reads expressions from a file"),
        )
        .arg(
            Arg::with_name("config-path")
                .long("config-path")
                .help("Prints a path to the config file, then exits")
        )
        .arg(
            Arg::with_name("service")
                .long("service")
                .help("Start in service mode")
                .hidden(true)
        )
        .get_matches();

    let config = config::read_config()?;

    if matches.is_present("config-path") {
        println!("{}", config::config_path("config.toml").unwrap().display());
        Ok(())
    } else if matches.is_present("service") {
        GLOBAL.set_limit(config.limits.memory);
        repl::service(&config)
    } else if let Some(filename) = matches.value_of("file") {
        match filename {
            "-" => {
                let stdin_handle = stdin();
                repl::noninteractive(stdin_handle.lock(), &config, false)
            }
            _ => {
                let file = File::open(&filename).wrap_err("Failed to open input file")?;
                repl::noninteractive(BufReader::new(file), &config, false)
            }
        }
    } else if let Some(exprs) = matches.values_of("EXPR") {
        let mut ctx = config::load(&config)?;
        for expr in exprs {
            println!("> {}", expr);
            match rink_core::one_line(&mut ctx, expr) {
                Ok(v) => println!("{}", v),
                Err(e) => println!("{}", e),
            }
        }
        Ok(())
    } else {
        repl::interactive(config).await
    }
}
