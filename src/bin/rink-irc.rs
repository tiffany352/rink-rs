#[cfg(feature = "irc")]
extern crate irc;
#[cfg(feature = "irc")]
extern crate rink;

#[cfg(feature = "irc")]
fn main() {
    use irc::client::prelude::*;
    use rink::*;

    let mut ctx = load().unwrap();
    ctx.short_output = true;
    let server = IrcServer::new("config.json").unwrap();
    server.identify().unwrap();
    let nick = server.config().nickname.clone().unwrap();
    let mut prefix = nick.clone();
    prefix.push(':');
    for message in server.iter() {
        if let Ok(Message { command: Command::PRIVMSG(ref chan, ref message_str), ..}) = message {
            if message_str.starts_with(&*prefix) {
                let reply_to = if &*chan == &*nick {
                    message.as_ref().unwrap().source_nickname().unwrap()
                } else {
                    &*chan
                };
                let line = message_str[prefix.len()..].trim();
                let reply = match one_line(&mut ctx, line) {
                    Ok(v) => v,
                    Err(e) => e
                };
                let mut i = 0;
                for line in reply.lines() {
                    if line.trim().len() > 0 {
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
            println!("{}", e);
        }
    }
}

#[cfg(not(feature = "irc"))]
fn main() {
    println!("Rink was not compiled with IRC support.");
}
