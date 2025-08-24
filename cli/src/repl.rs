// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::config::{Config, CurrencyBehavior};
use crate::currency::CurrencyStatus;
use crate::runner::Runner;
use crate::service::EvalResult;
use crate::RinkHelper;
use eyre::Result;
use jiff::{Timestamp, Unit};
use rink_core::one_line;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::{CompletionType, Editor};
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

fn prompt_load_currency(endpoint: &str, rl: &mut Editor<RinkHelper>) -> Result<bool> {
    let prompt = format!("Download currency data from <{}>? [y/n] ", endpoint);
    loop {
        let line = rl.readline(&prompt);
        match line {
            Ok(line) => match &line.trim().to_lowercase()[..] {
                "y" | "yes" => return Ok(true),
                "n" | "no" => return Ok(false),
                _ => println!("Unknown answer. Please type `y` or `n`."),
            },
            Err(ReadlineError::Interrupted) => return Ok(false),
            Err(ReadlineError::Eof) => return Ok(false),
            Err(err) => return Err(eyre::eyre!(err)),
        }
    }
}

fn prompt_fetch_currency(config: &Config, rl: &mut Editor<RinkHelper>) -> Result<bool> {
    let should_fetch = match config.currency.behavior {
        CurrencyBehavior::Always => {
            println!("Downloading {}...", config.currency.endpoint);
            true
        }
        CurrencyBehavior::Prompt => prompt_load_currency(&config.currency.endpoint, rl)?,
    };
    if !should_fetch {
        return Ok(false);
    }
    let start = Timestamp::now();
    let file = match crate::currency::load_live_currency(&config.currency) {
        Ok(file) => file,
        Err(err) => {
            println!("{err:#}");
            return Ok(false);
        }
    };
    let metadata = file.metadata()?;
    let stop = Timestamp::now();
    let delta = stop - start;
    println!(
        "Fetched {}kB in {:#}",
        metadata.len() / 1000,
        delta.round(Unit::Millisecond).unwrap()
    );
    Ok(true)
}

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
            Ok(()) => (),
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

                if config.rink.show_interpretation {
                    let query = rink_core::reformat(&mut runner.local.lock().unwrap(), &line);
                    println!("Input: {}", crate::fmt::to_ansi_string(&config, &query));
                }

                let (result, metrics) = runner.execute(line.clone());
                match result {
                    EvalResult::AnsiString(line) => {
                        println!("{}", line);
                        if let Some(metrics) = metrics {
                            println!("{}", metrics);
                        }
                    }
                    EvalResult::MissingDeps(_deps) => {
                        let did_fetch = prompt_fetch_currency(&config, &mut rl)?;
                        if !did_fetch {
                            continue;
                        }
                        let status =
                            crate::currency::load_cached_if_current(config.currency.expiration())?;
                        match status {
                            CurrencyStatus::Found(_file) => (),
                            CurrencyStatus::NotFound => {
                                println!("Couldn't find file again after downloading");
                                continue;
                            }
                            CurrencyStatus::Expired => {
                                println!("File was expired immediately after downloading");
                                continue;
                            }
                        }
                        runner.restart()?;
                        let (result, metrics) = runner.execute(line);
                        match result {
                            EvalResult::AnsiString(line) => println!("{}", line),
                            EvalResult::MissingDeps(deps) => println!(
                                "Still missing dependencies after fetch. Dependencies: {}",
                                deps
                            ),
                        }
                        if let Some(metrics) = metrics {
                            println!("{}", metrics);
                        }
                    }
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
