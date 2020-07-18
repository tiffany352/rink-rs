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

pub use completer::RinkCompleter;

fn main_noninteractive<T: BufRead>(mut f: T, show_prompt: bool) {
    use std::io::{stdout, Write};

    let mut ctx = match load() {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    let mut line = String::new();
    loop {
        if show_prompt {
            print!("> ");
        }
        stdout().flush().unwrap();
        if f.read_line(&mut line).is_err() {
            return;
        }
        // the underlying file object has hit an EOF if we try to read a
        // line but do not find the newline at the end, so let's break
        // out of the loop
        if line.find('\n').is_none() {
            return;
        }
        match one_line(&mut ctx, &*line) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("{}", e),
        };
        line.clear();
    }
}

fn main_interactive() {
    let rl = match Interface::new("rink") {
        Err(_) => {
            // If we can't initialize linefeed on this terminal for some reason,
            // e.g. it being a pipe instead of a tty, use the noninteractive version
            // with prompt instead.
            let stdin_handle = stdin();
            return main_noninteractive(stdin_handle.lock(), true);
        }
        Ok(rl) => rl,
    };
    rl.set_prompt("> ").unwrap();

    let ctx = match load() {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    let ctx = Arc::new(Mutex::new(ctx));
    let completer = RinkCompleter::new(ctx.clone());
    rl.set_completer(Arc::new(completer));

    let mut hpath = rink_core::config_dir();
    if let Ok(ref mut path) = hpath {
        path.push("history.txt");
        rl.load_history(path).unwrap_or_else(|e| {
            // ignore "not found" error
            if e.kind() != std::io::ErrorKind::NotFound {
                eprintln!("Loading history failed: {}", e);
            }
        });
    }
    loop {
        let readline = rl.read_line();
        match readline {
            Ok(ReadResult::Input(ref line)) if line == "quit" => {
                println!();
                break;
            }
            Ok(ReadResult::Input(ref line)) if line == "help" => {
                println!(
                    "For information on how to use Rink, see the manual: \
                     https://github.com/tiffany352/rink-rs/wiki/Rink-Manual"
                );
            }
            Ok(ReadResult::Input(line)) => {
                rl.add_history(line.clone());
                match one_line(&mut *ctx.lock().unwrap(), &*line) {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("{}", e),
                };
            }
            Ok(ReadResult::Eof) => {
                println!();
                if let Ok(ref path) = hpath {
                    // ignore error - if this fails, the next line will as well.
                    let _ = std::fs::create_dir_all(path.parent().unwrap());
                    rl.save_history(path).unwrap_or_else(|e| {
                        eprintln!("Saving history failed: {}", e);
                    });
                }
                break;
            }
            Ok(ReadResult::Signal(_)) => (),
            Err(err) => {
                println!("Readline: {:?}", err);
                break;
            }
        }
    }
}

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
                    main_noninteractive(stdin_handle.lock(), false);
                }
                _ => {
                    let file = File::open(&name).unwrap_or_else(|e| {
                        eprintln!("Could not open input file '{}': {}", name, e);
                        std::process::exit(1);
                    });
                    main_noninteractive(BufReader::new(file), false);
                }
            };
        }
        // else call the interactive version
        None => main_interactive(),
    };
}
