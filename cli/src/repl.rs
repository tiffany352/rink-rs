// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::config::Config;
use crate::runner::Runner;
use crate::RinkHelper;
use eyre::Result;
use rink_core::one_line;
use rustyline::{config::Configurer, error::ReadlineError, CompletionType, Editor};
use std::io::{BufRead, ErrorKind};

pub fn noninteractive<T: BufRead>(mut f: T, config: &Config, show_prompt: bool) -> Result<()> {
    use std::io::{stdout, Write};

    let mut ctx = crate::config::load(config)?;
    let mut line = String::new();
    loop {
        if show_prompt {
            print!("> ");
        }
        stdout().flush().unwrap();
        if f.read_line(&mut line).is_err() {
            return Ok(());
        }
        // the underlying file object has hit an EOF if we try to read a
        // line but do not find the newline at the end, so let's break
        // out of the loop
        if line.find('\n').is_none() {
            return Ok(());
        }
        match one_line(&mut ctx, &*line) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("{}", e),
        };
        line.clear();
    }
}

pub const HELP_TEXT: &'static str = "The rink manual can be found with `man 7 rink`, or online:
https://rinkcalc.app/manual
To quit, type `quit` or press Ctrl+D.";

pub fn interactive(config: Config) -> Result<()> {
    let mut runner = Runner::new(config.clone())?;
    let mut rl = Editor::<RinkHelper>::new();
    let helper = RinkHelper::new(runner.local.clone(), config.clone());
    rl.set_helper(Some(helper));
    rl.set_completion_type(CompletionType::List);

    let mut hpath = dirs::data_local_dir().map(|mut path| {
        path.push("rink");
        path.push("history.txt");
        path
    });
    if let Some(ref mut path) = hpath {
        match rl.load_history(path) {
            // Ignore file not found errors.
            Err(ReadlineError::Io(ref err)) if err.kind() == ErrorKind::NotFound => (),
            Err(err) => eprintln!("Loading history failed: {}", err),
            _ => (),
        };
    }

    let save_history = |rl: &mut Editor<RinkHelper>| {
        if let Some(ref path) = hpath {
            // ignore error - if this fails, the next line will as well.
            let _ = std::fs::create_dir_all(path.parent().unwrap());
            rl.save_history(path).unwrap_or_else(|e| {
                eprintln!("Saving history failed: {}", e);
            });
        }
    };

    loop {
        let readline = rl.readline(&config.rink.prompt);
        match readline {
            Ok(ref line) if line == "help" => {
                println!("{}", HELP_TEXT);
            }
            Ok(ref line) if line == "quit" || line == ":q" || line == "exit" => {
                save_history(&mut rl);
                break;
            }
            Ok(line) => {
                rl.add_history_entry(&line);

                let (result, metrics) = runner.execute(line);
                match result {
                    Ok(line) => println!("{}", line),
                    Err(line) => println!("{}", line),
                }
                if let Some(metrics) = metrics {
                    println!("{}", metrics);
                }
            }
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => {
                save_history(&mut rl);
                break;
            }
            Err(err) => {
                println!("{:?}", eyre::eyre!(err).wrap_err("Readline"));
                break;
            }
        }
    }

    runner.terminate()?;

    Ok(())
}
