// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use clap::{App, Arg};
use rink_core::load;
use std::fs::File;
use std::io::{stdin, BufReader};

pub use completer::RinkCompleter;

pub mod completer;
pub mod repl;

fn main() {
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
        .get_matches();

    if let Some(filename) = matches.value_of("file") {
        match filename {
            "-" => {
                let stdin_handle = stdin();
                repl::noninteractive(stdin_handle.lock(), false);
            }
            _ => {
                let file = File::open(&filename).unwrap_or_else(|e| {
                    eprintln!("Could not open input file '{}': {}", filename, e);
                    std::process::exit(1);
                });
                repl::noninteractive(BufReader::new(file), false);
            }
        }
    } else if let Some(exprs) = matches.values_of("EXPR") {
        let mut ctx = match load() {
            Ok(ctx) => ctx,
            Err(e) => {
                println!("{}", e);
                return;
            }
        };
        for expr in exprs {
            println!("> {}", expr);
            match rink_core::one_line(&mut ctx, expr) {
                Ok(v) => println!("{}", v),
                Err(e) => println!("{}", e),
            }
        }
    } else {
        repl::interactive()
    }
}
