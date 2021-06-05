use crate::config::Config;
use crate::fmt::to_ansi_string;
use crate::sandbox::Sandbox;
use crate::sandbox::SandboxError;
use crate::sandbox::SandboxReply;
use crate::RinkHelper;
use crate::GLOBAL;
use async_ctrlc::CtrlC;
use async_std::prelude::{FutureExt, StreamExt};
use async_std::sync::Mutex as AsyncMutex;
use eyre::Result;
use rink_core::{eval, one_line};
use rustyline::{config::Configurer, error::ReadlineError, CompletionType, Editor};
use std::io::{stdin, BufRead, ErrorKind};
use std::sync::{Arc, Mutex};
use std::time::Instant;

pub fn service(config: &Config) -> Result<()> {
    use std::io::{stdout, Write};

    let stdin = stdin();
    let mut stdout = stdout();
    let mut input = stdin.lock();

    let mut ctx = crate::config::load(config)?;
    let mut buf = vec![];
    loop {
        stdout.flush().unwrap();
        if input.read_until(b'\0', &mut buf).is_err() {
            return Ok(());
        }
        // Was at eof
        if buf.last() != Some(&b'\0') {
            return Ok(());
        }
        let start = Instant::now();
        GLOBAL.reset_max();
        buf.pop();
        let string = String::from_utf8(buf.drain(..).collect())?;
        let result = eval(&mut ctx, &string);
        let result = result
            .map(|result| {
                let stop = Instant::now();
                SandboxReply {
                    result: format!("{}", to_ansi_string(config, &result)),
                    memory_used: GLOBAL.get_max(),
                    time_taken: stop - start,
                }
            })
            .map_err(|err| format!("{}", to_ansi_string(config, &err)));
        let mut out = stdout.lock();
        serde_json::to_writer(&mut out, &result)?;
        out.write(b"\0")?;
        buf.clear();
    }
}

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

pub async fn interactive(config: Config) -> Result<()> {
    let mut rl = Editor::<RinkHelper>::new();

    let ctx = crate::config::load(&config)?;
    let ctx = Arc::new(Mutex::new(ctx));
    let helper = RinkHelper::new(ctx.clone(), config.clone());
    rl.set_helper(Some(helper));
    rl.set_completion_type(CompletionType::List);
    let mut ctrlc = CtrlC::new()?;

    let sandbox = Arc::new(AsyncMutex::new(Sandbox::start()));

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
                rl.add_history_entry(&line);
                let config = config.clone();

                let interrupted = async {
                    ctrlc.next().await;
                    let mut sandbox = sandbox.lock().await;
                    if let Err(err) = sandbox.kill() {
                        println!(
                            "{:#}",
                            eyre::eyre!(err).wrap_err("Failed to restart child process")
                        );
                    }
                };

                let task = async {
                    let mut sandbox = sandbox.lock().await;
                    let pending = sandbox.eval(line, config.limits.timeout);
                    match pending.await {
                        Ok(v) => {
                            println!("{}", v.result);
                            if config.limits.show_metrics {
                                println!(
                                    "Finished in {:?} using {}K of memory",
                                    v.time_taken,
                                    v.memory_used / 1_000
                                );
                            }
                        }
                        Err(SandboxError::Query(err)) => println!("{}", err),
                        Err(err) => println!("{:#}", eyre::eyre!(err)),
                    };
                };

                interrupted.race(task).await;
                sandbox.lock().await.restart_if_dead()?;
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
