// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;
#[cfg(feature = "linefeed")]
extern crate linefeed;

use std::fs::File;
use std::io::{BufRead, BufReader, stdin};

use rink::*;

fn main_noninteractive<T: BufRead>(mut f: T, show_prompt: bool) {
    use std::io::{stdout, Write};

    let mut ctx = match load() {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return
        }
    };
    let mut line = String::new();
    loop {
        if show_prompt {
            print!("> ");
        }
        stdout().flush().unwrap();
        match f.read_line(&mut line) {
            Ok(_) => (),
            Err(_) => return
        };
        // the underlying file object has hit an EOF if we try to read a
        // line but do not find the newline at the end, so let's break
        // out of the loop
        if line.find('\n').is_none() {
            return;
        }
        match one_line(&mut ctx, &*line) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("{}", e)
        };
        line.clear();
    }
}

#[cfg(feature = "linefeed")]
fn main_interactive() {
    use linefeed::{Reader, ReadResult, Suffix, Terminal, Completer, Completion};
    use std::rc::Rc;
    use std::cell::RefCell;

    let mut rl = match Reader::new("rink") {
        Err(_) => {
            // If we can't initialize linefeed on this terminal for some reason,
            // e.g. it being a pipe instead of a tty, use the noninteractive version
            // with prompt instead.
            let stdin_handle = stdin();
            return main_noninteractive(stdin_handle.lock(), true);
        },
        Ok(rl) => rl
    };
    rl.set_prompt("> ");

    struct RinkCompleter(Rc<RefCell<Context>>);

    impl<Term: Terminal> Completer<Term> for RinkCompleter {
        fn complete(&self, name: &str, _reader: &Reader<Term>, _start: usize, _end: usize)
                    -> Option<Vec<Completion>> {
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
                                ctx.docs.get(&**k)
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
                                    pk, k,
                                    (&prop.input / &prop.output)
                                        .expect("Non-zero substance properties")
                                        .to_parts(ctx)
                                        .quantity
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                    prop.doc.as_ref()
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
                                    prop.input_name, k,
                                    prop.output_name,
                                    prop.input
                                        .to_parts(ctx)
                                        .quantity
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                    prop.doc.as_ref()
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
                                    prop.output_name, k,
                                    prop.input_name,
                                    prop.output
                                        .to_parts(ctx)
                                        .quantity
                                        .map(|x| format!("; {}", x))
                                        .unwrap_or_default(),
                                    prop.doc.as_ref()
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
                            display: Some(format!("{} (quantity; {})",
                                                  k, Number::unit_to_string(unit))),
                            suffix: Suffix::Default,
                        });
                    }
                }
                out
            }

            let mut out = vec![];
            out.append(&mut inner(&*self.0.borrow(), name));
            for &(ref k, ref v) in &self.0.borrow().prefixes {
                if name.starts_with(&**k) {
                    out.append(&mut inner(&*self.0.borrow(), &name[k.len()..])
                               .into_iter().map(|x| Completion {
                                   completion: format!("{}{}", k, x.completion),
                                   display: Some(format!("{} {}", k, x.display.unwrap())),
                                   suffix: Suffix::Default,
                               }).collect());
                } else if k.starts_with(name) {
                    out.insert(0, Completion {
                        completion: k.clone(),
                        display: Some(format!("{} ({:?} prefix)", k, v.value)),
                        suffix: Suffix::Default,
                    });
                }
            }

            Some(out)
        }
    }

    let ctx = match load() {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return
        }
    };
    let ctx = Rc::new(RefCell::new(ctx));
    let completer = RinkCompleter(ctx.clone());
    rl.set_completer(Rc::new(completer));

    let mut hpath = rink::config_dir();
    if let Ok(ref mut path) = hpath {
        path.push("rink/history.txt");
    }
    let hfile = hpath.clone().and_then(|hpath| File::open(hpath).map_err(|x| x.to_string()));
    let hfile = hfile.map(BufReader::new);
    if let Ok(hfile) = hfile {
        for line in hfile.lines() {
            let line = match line {
                Ok(line) => line,
                Err(_e) => break
            };
            rl.add_history(line);
        }
    }
    loop {
        let readline = rl.read_line();
        match readline {
            Ok(ReadResult::Input(ref line)) if line == "quit" => {
                println!();
                break
            },
            Ok(ReadResult::Input(ref line)) if line == "help" => {
                println!("For information on how to use Rink, see the manual: \
                          https://github.com/tiffany352/rink-rs/wiki/Rink-Manual");
            },
            Ok(ReadResult::Input(line)) => {
                rl.add_history(line.clone());
                match one_line(&mut *ctx.borrow_mut(), &*line) {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("{}", e)
                };
            },
            Ok(ReadResult::Eof) => {
                println!();
                let hfile = hpath.and_then(|hpath| File::create(hpath).map_err(|x| x.to_string()));
                if let Ok(mut hfile) = hfile {
                    for line in rl.history() {
                        use std::io::Write;
                        let _ = writeln!(hfile, "{}", line);
                    }
                }
                break
            },
            Ok(ReadResult::Signal(_)) => (),
            Err(err) => {
                println!("Readline: {:?}", err);
                break
            },
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
                },
                _ => {
                    let file = File::open(&name).unwrap_or_else(|e| {
                        eprintln!("Could not open input file '{}': {}", name, e);
                        std::process::exit(1);
                    });
                    main_noninteractive(BufReader::new(file), false);
                }
            };
        },
        // else call the interactive version
        None => main_interactive()
    };
}
