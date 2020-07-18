use std::io::{stdin, BufRead};
use std::sync::{Arc, Mutex};

use linefeed::{Interface, ReadResult, Signal};

use rink_core::{load, one_line};

use crate::RinkCompleter;

pub fn noninteractive<T: BufRead>(mut f: T, show_prompt: bool) {
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

pub fn interactive() {
    let rl = match Interface::new("rink") {
        Err(_) => {
            // If we can't initialize linefeed on this terminal for some reason,
            // e.g. it being a pipe instead of a tty, use the noninteractive version
            // with prompt instead.
            let stdin_handle = stdin();
            return noninteractive(stdin_handle.lock(), true);
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

    let save_history = || {
        if let Ok(ref path) = hpath {
            // ignore error - if this fails, the next line will as well.
            let _ = std::fs::create_dir_all(path.parent().unwrap());
            rl.save_history(path).unwrap_or_else(|e| {
                eprintln!("Saving history failed: {}", e);
            });
        }
    };

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
                     https://github.com/tiffany352/rink-rs/wiki/Rink-Manual\n\
                     To quit, type `quit`."
                );
            }
            Ok(ReadResult::Input(ref line)) if line == "quit" || line == ":q" || line == "exit" => {
                save_history();
                break;
            }
            Ok(ReadResult::Input(line)) => {
                rl.add_history(line.clone());
                match one_line(&mut *ctx.lock().unwrap(), &*line) {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("{}", e),
                };
            }
            Ok(ReadResult::Eof)
            | Ok(ReadResult::Signal(Signal::Interrupt))
            | Ok(ReadResult::Signal(Signal::Quit)) => {
                save_history();
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
