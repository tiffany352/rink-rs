// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use clap::{Arg, ArgAction, Command};
use eyre::{Result, WrapErr};
use std::fs::File;
use std::io::{stdin, BufReader};
use std::process::ExitCode;

use rink::{config, repl, service};

fn main() -> Result<ExitCode> {
    let matches = Command::new("Rink")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Rink Contributors")
        .about("Unit conversion tool")
        .arg(
            Arg::new("EXPR")
                .help("Evaluate a list of expressions. If no arguments are provided, an interactive session will start.")
                .num_args(..)
                .required(false),
        )
        .arg(
            Arg::new("file")
                .short('f')
                .long("file")
                .help("Reads expressions from a file"),
        )
        .arg(
            Arg::new("config-path")
                .long("config-path")
                .help("Prints a path to the config file, then exits")
                .action(ArgAction::SetTrue)
        )
        .arg(
            Arg::new("fetch-currency")
                .long("fetch-currency")
                .help("Fetches latest version of currency data, then exits")
                .action(ArgAction::SetTrue)
        )
        .arg(
            Arg::new("dump")
                .long("dump")
                .help("Generates a file containing the contents of the Context object, then exits")
                .default_missing_value("dump.txt")
                .action(ArgAction::Set)
                .hide(true)
        )
        .arg(
            Arg::new("service")
                .long("service")
                .help("Start in service mode")
                .action(ArgAction::SetTrue)
                .hide(true)
        )
        .arg(
            Arg::new("config")
                .short('c')
                .num_args(1)
                .long("config").action(ArgAction::Set).help("Set path to config.toml")
        )
        .get_matches();

    if matches.get_flag("service") {
        return service::run_service().map(|_| ExitCode::SUCCESS);
    }
    // The panic handler can't be installed if entering service mode, so
    // it's placed after that check.
    color_eyre::install()?;
    let config = config::read_config(matches.get_one::<String>("config").map(|s| &**s))?;

    if let Some(filename) = matches.get_one::<String>("dump") {
        use std::io::Write;

        let ctx = config::load(&config)?;
        let mut file = std::fs::File::create(filename)?;
        writeln!(&mut file, "{:#?}", ctx)?;
        return Ok(ExitCode::SUCCESS);
    }

    if matches.get_flag("fetch-currency") {
        let result = config::force_refresh_currency(&config.currency);
        match result {
            Ok(msg) => {
                println!("{msg}");
                return Ok(ExitCode::SUCCESS);
            }
            Err(err) => return Err(err),
        }
    }

    if matches.get_flag("config-path") {
        println!("{}", config::config_path("config.toml").unwrap().display());
        Ok(ExitCode::SUCCESS)
    } else if let Some(filename) = matches.get_one::<String>("file") {
        match &filename[..] {
            "-" => {
                let stdin_handle = stdin();
                repl::noninteractive(stdin_handle.lock(), &config, false).map(|_| ExitCode::SUCCESS)
            }
            _ => {
                let file = File::open(&filename)
                    .wrap_err(format!("Failed to open input file `{filename}`"))?;
                repl::noninteractive(BufReader::new(file), &config, false)
                    .map(|_| ExitCode::SUCCESS)
            }
        }
    } else if let Some(exprs) = matches.get_many::<String>("EXPR") {
        let mut ctx = config::load(&config)?;
        let mut exit_code = ExitCode::SUCCESS;
        for expr in exprs {
            println!("> {}", expr);
            match rink_core::one_line(&mut ctx, expr) {
                Ok(v) => println!("{}", v),
                Err(e) => {
                    println!("{}", e);
                    exit_code = ExitCode::FAILURE;
                }
            }
        }
        Ok(exit_code)
    } else {
        repl::interactive(config).map(|_| ExitCode::SUCCESS)
    }
}
