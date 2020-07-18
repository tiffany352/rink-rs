// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use rink_core;

use linefeed::{Interface, ReadResult};
use std::fs::File;
use std::io::{stdin, BufRead, BufReader};
use std::sync::{Arc, Mutex};

pub use rink_core::*;

pub mod completer;
pub mod repl;

pub use completer::RinkCompleter;

fn usage() {
    println!(
        "{} {}\n{}\n{}\n\n\
         USAGE:\n    {0} [input file]\n\n\
         FLAGS:\n    -h, --help      Prints help information\n    \
         -V, --version   Prints version information\n\n\
         ARGS:\n    <input file>    Evaluate queries from this file",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        env!("CARGO_PKG_AUTHORS"),
        env!("CARGO_PKG_DESCRIPTION"),
    );
}

fn version() {
    println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
}

fn main() {
    use std::env::args;

    if args().any(|arg| arg == "-h" || arg == "--help") {
        usage();
        return;
    }

    if args().any(|arg| arg == "-V" || arg == "--version") {
        version();
        return;
    }

    if args().len() > 2 {
        usage();
        std::process::exit(1);
    }

    // Specify the file to parse commands from as a shell argument
    // i.e. "rink <file>"
    let input_file_name = args().nth(1);
    match input_file_name {
        // if we have an input, buffer it and call main_noninteractive
        Some(name) => {
            match name.as_ref() {
                "-" => {
                    let stdin_handle = stdin();
                    repl::noninteractive(stdin_handle.lock(), false);
                }
                _ => {
                    let file = File::open(&name).unwrap_or_else(|e| {
                        eprintln!("Could not open input file '{}': {}", name, e);
                        std::process::exit(1);
                    });
                    repl::noninteractive(BufReader::new(file), false);
                }
            };
        }
        // else call the interactive version
        None => repl::interactive(),
    };
}
