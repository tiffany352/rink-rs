// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use rink;

use std::fs::File;
use std::io::{stdin, BufRead, BufReader};

use rink::*;

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

#[cfg(feature = "linefeed")]
fn main_interactive() {
    use linefeed::{Completer, Completion, Interface, Prompter, ReadResult, Suffix, Terminal};
    use std::sync::{Arc, Mutex};

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

    struct RinkCompleter(Arc<Mutex<Context>>);

    impl<Term: Terminal> Completer<Term> for RinkCompleter {
        fn complete(
            &self,
            name: &str,
            _prompter: &Prompter<Term>,
            _start: usize,
            _end: usize,
        ) -> Option<Vec<Completion>> {
            fn inner(ctx: &Context, name: &str) -> Vec<Completion> {
                let mut out = vec![];
                for k in &ctx.dimensions {
                    if (**k.0).starts_with(name) {
                        out.push(Completion {
                            completion: (*k.0).clone(),
                            display: Some(format!("{} (base unit)", k.0)),
                            suffix: Suffix::Default,
                        });
                    }
                }
                for k in ctx.units.keys() {
                    if k.starts_with(name) {
                        let def = &ctx.definitions.get(&**k);
                        let def = def.map(|def| format!("{} = ", def)).unwrap_or_default();
                        let res = ctx.lookup(k).unwrap();
                        let parts = res.to_parts(ctx);
                        out.push(Completion {
                            completion: (*k).clone(),
                            display: Some(format!("{} ({}{})", k, def, parts.format("n u p"))),
                            suffix: Suffix::Default,
                        });
                    }
                }
                for (ref k, ref sub) in &ctx.substances {
                    if k.starts_with(name) {
                        out.push(Completion {
                            completion: (*k).clone(),
                            display: Some(format!(
                                "{} (substance{})",
                                k,
                                ctx.docs
                                    .get(&**k)
                                    .map(|x| format!(", {}", x))
                                    .unwrap_or_default()
                            )),
                            suffix: Suffix::Default,
                        });
                    }
                    for (pk, prop) in &sub.properties.properties {
                        if pk.starts_with(name) {
                            out.push(Completion {
                                completion: format!("{} of", pk),
                                display: Some(format!(
                                    "{} of (property of {}{}{})",
                                    pk,
                                    k,
                                    (&prop.input / &prop.output)
                                        .expect("Non-zero substance properties")
                                        .to_parts(ctx)
                                        .quantity
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                    prop.doc
                                        .as_ref()
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                )),
                                suffix: Suffix::Default,
                            });
                        }
                        if prop.input_name.starts_with(name) {
                            out.push(Completion {
                                completion: format!("{} of", prop.input_name),
                                display: Some(format!(
                                    "{} of (property of {} {}{}{})",
                                    prop.input_name,
                                    k,
                                    prop.output_name,
                                    prop.input
                                        .to_parts(ctx)
                                        .quantity
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                    prop.doc
                                        .as_ref()
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                )),
                                suffix: Suffix::Default,
                            });
                        }
                        if prop.output_name.starts_with(name) {
                            out.push(Completion {
                                completion: format!("{} of", prop.output_name),
                                display: Some(format!(
                                    "{} of (property of {} {}{}{})",
                                    prop.output_name,
                                    k,
                                    prop.input_name,
                                    prop.output
                                        .to_parts(ctx)
                                        .quantity
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                    prop.doc
                                        .as_ref()
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                )),
                                suffix: Suffix::Default,
                            });
                        }
                    }
                }
                for (ref unit, ref k) in &ctx.quantities {
                    if k.starts_with(name) {
                        out.push(Completion {
                            completion: (*k).clone(),
                            display: Some(format!(
                                "{} (quantity; {})",
                                k,
                                Number::unit_to_string(unit)
                            )),
                            suffix: Suffix::Default,
                        });
                    }
                }
                out
            }

            let mut out = vec![];
            let ctx = self.0.lock().unwrap();
            out.append(&mut inner(&ctx, name));
            for &(ref k, ref v) in &ctx.prefixes {
                if name.starts_with(&**k) {
                    out.append(
                        &mut inner(&ctx, &name[k.len()..])
                            .into_iter()
                            .map(|x| Completion {
                                completion: format!("{}{}", k, x.completion),
                                display: Some(format!("{} {}", k, x.display.unwrap())),
                                suffix: Suffix::Default,
                            })
                            .collect(),
                    );
                } else if k.starts_with(name) {
                    out.insert(
                        0,
                        Completion {
                            completion: k.clone(),
                            display: Some(format!("{} ({:?} prefix)", k, v.value)),
                            suffix: Suffix::Default,
                        },
                    );
                }
            }

            Some(out)
        }
    }

    let ctx = match load() {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    let ctx = Arc::new(Mutex::new(ctx));
    let completer = RinkCompleter(ctx.clone());
    rl.set_completer(Arc::new(completer));

    let mut hpath = rink::config_dir();
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

// If we aren't compiling with linefeed support we should just call the
// noninteractive version
#[cfg(not(feature = "linefeed"))]
fn main_interactive() {
    let stdin = stdin();
    main_noninteractive(stdin.lock(), true);
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
