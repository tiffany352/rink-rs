#[cfg(feature = "irc")]
extern crate irc;
#[cfg(feature = "irc")]
extern crate rink;

#[cfg(feature = "irc")]
fn main() {
    use irc::client::prelude::*;
    use rink::*;

    let mut ctx = load().unwrap();
    let server = IrcServer::new("config.json").unwrap();
    server.identify().unwrap();
    let mut prefix = server.config().nickname.clone().unwrap();
    prefix.push(':');
    for message in server.iter() {
        if let Ok(Message { command: Command::PRIVMSG(chan, message), ..}) = message {
            if message.starts_with(&*prefix) {
                let line = message[prefix.len()..].trim();
                let reply = match one_line(&mut ctx, line) {
                    Ok(v) => ctx.show(&v),
                    Err(e) => e
                };
                let mut i = 0;
                for line in reply.lines() {
                    if line.trim().len() > 0 {
                        server.send(Command::NOTICE(chan.clone(), line.to_owned())).unwrap();
                        i += 1;
                    }
                    // cut off early
                    if i > 4 {
                        break;
                    }
                }
            }
        } else if let Err(e) = message {
            println!("{}", e);
        }
    }
}

#[cfg(not(feature = "irc"))]
fn main() {
    println!("Rink was not compiled with IRC support.");
}
