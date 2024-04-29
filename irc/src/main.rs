// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use futures::StreamExt;
use irc::client::prelude::*;
//use rink_core::*;

mod config;

#[tokio::main]
async fn main() {
    let config_file = std::fs::read_to_string("config.toml").unwrap();
    let config = toml::from_str::<config::Config>(&config_file).unwrap();

    // todo: multiple servers
    let server = &config.servers[0];
    let mut client = Client::from_config(server.clone()).await.unwrap();
    println!("Connecting...");
    client.identify().unwrap();
    println!("Connected. nickname: {}", client.current_nickname());

    let mut stream = client.stream().unwrap();
    while let Some(message) = stream.next().await.transpose().unwrap() {
        let source = message.source_nickname();
        if let Command::PRIVMSG(ref channel, ref text) = message.command {
            let nick = client.current_nickname();
            let prefix = format!("{}: ", nick);
            let command = if let Some(text) = text.strip_prefix(&prefix) {
                text
            } else if channel == client.current_nickname() {
                &text
            } else {
                continue;
            };
            println!("<== {command}");
            let reply = format!("echoing back {command}");
            println!("==> {reply}");
            let where_to = if channel == client.current_nickname() {
                if let Some(source) = source {
                    source
                } else {
                    continue;
                }
            } else {
                &channel
            };
            client.send_notice(where_to, reply).unwrap();
        }
    }
}
