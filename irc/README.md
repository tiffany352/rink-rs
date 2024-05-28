# Rink IRC

This is an IRC bot that runs rink queries when asked.

Note that this IRC bot does not yet support sandboxing. This means that
a bad query can cause the bot to OOM or to time out. It does not mean
that a user would be able to pwn your system though.

## How to interact with it

In an IRC channel, prefix a message with `rink: ` and then put your
query after. Direct messages are always interpreted as queries.

## How to operate it

You'll need to create a `config.toml` for the bot. It uses the following
format (the values show the defaults):

```toml
[limits]
# Not yet implemented
enabled = true
# Not yet implemented
show_metrics = true
# Not yet implemented
memory = "20MB"
# Not yet implemented
timeout = "10sec"

[currency]
# If you want rink to be able to do live currency conversions, setup a
# cron job to automatically download this file every hour or so:
# https://rinkcalc.app/data/currency.json
enabled = false
path = "./currency.json"

# This is a toml array, multiple can be specified.
[[servers]]
# The options here are the exact ones from the `irc` crate config file.
# https://github.com/aatxe/irc?tab=readme-ov-file#configuring-irc-clients
owners = ["Tiffany"]
nickname = "rink"
realname = "rink-irc-bot"
server = "irc.libera.chat"
port = 6697
use_tls = true
version = "Rink IRC bot v0.8"
source = "https://github.com/tiffany352/rink-rs"
channels = ["#rink"]
```
