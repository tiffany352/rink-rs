use crate::config::Config;
use crate::fmt::print_fmt;
use crate::service::RinkService;
use crate::RinkHelper;
use async_std::task;
use eyre::Result;
use rink_core::one_line;
use rink_sandbox::Sandbox;
use rustyline::{config::Configurer, error::ReadlineError, CompletionType, Editor};
use std::io::{BufRead, ErrorKind};
use std::sync::{Arc, Mutex};

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

pub fn interactive(config: &Config) -> Result<()> {
    let mut rl = Editor::<RinkHelper>::new();

    let ctx = crate::config::load(config)?;
    let ctx = Arc::new(Mutex::new(ctx));
    let helper = RinkHelper::new(ctx.clone(), config.clone());
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
                println!(
                    "For information on how to use Rink, see the manual: \
                     https://github.com/tiffany352/rink-rs/wiki/Rink-Manual\n\
                     To quit, type `quit`."
                );
            }
            Ok(ref line) if line == "quit" || line == ":q" || line == "exit" => {
                save_history(&mut rl);
                break;
            }
            Ok(line) => {
                match rink_core::eval(&mut *ctx.lock().unwrap(), &*line) {
                    Ok(v) => {
                        rl.add_history_entry(line);
                        print_fmt(config, &v)
                    }
                    Err(e) => print_fmt(config, &e),
                };
                println!();
            }
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => {
                save_history(&mut rl);
                break;
            }
            Err(err) => {
                println!("Readline: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}

pub async fn interactive_sandboxed(config: Config) -> Result<()> {
    let ctx = crate::config::load(&config)?;
    let ctx = Arc::new(Mutex::new(ctx));
    let rl = Arc::new(Mutex::new({
        let mut rl = Editor::<RinkHelper>::new();
        let helper = RinkHelper::new(ctx.clone(), config.clone());
        rl.set_helper(Some(helper));
        rl.set_completion_type(CompletionType::List);
        rl
    }));

    let sandbox = Sandbox::<RinkService>::new(config.clone()).await?;
    let sandbox = Arc::new(sandbox);

    let mut hpath = dirs::data_local_dir().map(|mut path| {
        path.push("rink");
        path.push("history.txt");
        path
    });
    if let Some(ref mut path) = hpath {
        match rl.lock().unwrap().load_history(path) {
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
        let readline = {
            let rl = rl.clone();
            let config = config.clone();
            task::spawn_blocking(move || rl.lock().unwrap().readline(&config.rink.prompt)).await
        };
        match readline {
            Ok(ref line) if line == "help" => {
                println!(
                    "For information on how to use Rink, see the manual: \
                     https://github.com/tiffany352/rink-rs/wiki/Rink-Manual\n\
                     To quit, type `quit`."
                );
            }
            Ok(ref line) if line == "quit" || line == ":q" || line == "exit" => {
                save_history(&mut rl.lock().unwrap());
                break;
            }
            Ok(line) => {
                rl.lock().unwrap().add_history_entry(&line);
                let config = config.clone();

                let result = sandbox.execute(line).await;
                match result {
                    Ok(res) => {
                        match res.result {
                            Ok(line) => println!("{}", line),
                            Err(line) => println!("{}", line),
                        }
                        if config.limits.show_metrics {
                            println!(
                                "Finished in {:?} using {}K of memory",
                                res.time_taken,
                                res.memory_used / 1_000
                            );
                        }
                    }
                    Err(err) => println!("{:#}", eyre::eyre!(err)),
                };
            }
            Err(ReadlineError::Interrupted) => {}
            Err(ReadlineError::Eof) => {
                save_history(&mut rl.lock().unwrap());
                break;
            }
            Err(err) => {
                println!("{:?}", eyre::eyre!(err).wrap_err("Readline"));
                break;
            }
        }
    }

    sandbox.terminate().await?;

    Ok(())
}
