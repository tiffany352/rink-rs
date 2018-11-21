// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate irc;
extern crate glob;
extern crate rink;

fn main() {
    use irc::client::prelude::*;
    use rink::*;
    use glob::glob;
    use std::thread;

    #[cfg(feature = "sandbox")]
    fn eval(line: &str) -> String {
        one_line_sandbox(line)
    }

    #[cfg(not(feature = "sandbox"))]
    fn eval(line: &str) -> String {
        let mut ctx = load().unwrap();
        ctx.short_output = true;
        match one_line(&mut ctx, line) {
            Ok(v) => v,
            Err(e) => e
        }
    }

    fn run(config: &str) {
        let server = IrcServer::new(config).unwrap();
        server.identify().unwrap();
        let nick = server.config().nickname.clone().unwrap();
        let mut prefix = nick.clone();
        prefix.push(':');
        for message in server.iter() {
            if let Ok(Message { command: Command::PRIVMSG(ref chan, ref message_str), ..}) = message {
                let prefixed = message_str.starts_with(&*prefix);
                if prefixed || &*chan == &*nick {
                    let reply_to = if &*chan == &*nick {
                        message.as_ref().unwrap().source_nickname().unwrap()
                    } else {
                        &*chan
                    };
                    let line = if prefixed {
                        message_str[prefix.len()..].trim()
                    } else {
                        &message_str[..]
                    };
                    let mut i = 0;
                    let reply = eval(line);
                    for line in reply.lines() {
                        if !line.trim().is_empty() {
                            server.send(Command::NOTICE(reply_to.to_owned(), line.to_owned())).unwrap();
                            i += 1;
                        }
                        // cut off early
                        if i > 4 {
                            break;
                        }
                    }
                }
            } else if let Err(e) = message {
                println!("{}: {}", config, e);
            }
        }
        println!("Thread {} exiting", config);
    }

    let mut threads = vec![];
    for config in glob("servers/*.json").expect("Glob failed") {
        match config {
            Ok(config) => threads.push(thread::spawn(move || run(config.to_str().unwrap()))),
            Err(e) => println!("{:?}", e)
        }
    }
    for thread in threads {
        thread.join().unwrap()
    }
}
