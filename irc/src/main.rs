// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use futures::StreamExt;
use irc::client::prelude::*;
use rink_core::output::fmt::TokenFmt;
use rink_sandbox::Alloc;

mod config;
mod fmt;

#[global_allocator]
pub(crate) static GLOBAL: Alloc = Alloc::new(usize::MAX);

async fn server_task(config: config::Config, index: usize) {
    let server = &config.servers[index];
    let mut ctx = config::load(&config);

    let servername = server.server.as_ref().map(|s| &s[..]).unwrap_or("");

    let mut client = Client::from_config(server.clone()).await.unwrap();
    println!("[{servername}] Connecting...");
    client.identify().unwrap();
    println!(
        "[{servername}] Connected. nickname: {}",
        client.current_nickname()
    );

    let mut stream = client.stream().unwrap();
    while let Some(message) = stream.next().await.transpose().unwrap() {
        let source = message.source_nickname();
        if let Command::PRIVMSG(ref channel, ref text) = message.command {
            let nick = client.current_nickname();
            let prefix = format!("{}:", nick);
            let command = if let Some(text) = text.strip_prefix(&prefix) {
                text
            } else if channel == client.current_nickname() {
                &text
            } else {
                continue;
            };
            println!("[{servername}] <== {command}");
            let result = rink_core::eval(&mut ctx, command);
            let result = match &result {
                Ok(res) => res.to_spans(),
                Err(err) => err.to_spans(),
            };
            let result = fmt::to_irc_string(&config, &result);
            println!("[{servername}] ==> {result}");
            let where_to = if channel == client.current_nickname() {
                if let Some(source) = source {
                    source
                } else {
                    continue;
                }
            } else {
                &channel
            };
            client.send_notice(where_to, result).unwrap();
        } else if let Command::INVITE(_nickname, channel) = message.command {
            if config.behavior.follow_invites {
                client.send_join(channel).unwrap();
            }
        }
    }
}

#[tokio::main]
async fn main() {
    let config_file = std::fs::read_to_string("config.toml").unwrap();
    let config = toml::from_str::<config::Config>(&config_file).unwrap();

    println!("Loading rink...");
    let mut ctx = config::load(&config);

    println!("Making sure we can actually run queries...");
    let test_query = rink_core::eval(&mut ctx, "m");
    match test_query {
        Ok(_res) => (),
        bad => {
            println!("failed to run a basic query: {:#?}", bad);
            std::process::exit(1);
        }
    }

    if config.servers.is_empty() {
        println!("config.toml doesn't contain any servers");
        std::process::exit(1);
    }

    let handles = (0..config.servers.len())
        .map(|i| server_task(config.clone(), i))
        .collect::<Vec<_>>();

    for handle in handles {
        handle.await;
    }
}
