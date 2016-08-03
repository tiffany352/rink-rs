extern crate units_rs;

use units_rs::*;

fn main() {
    use std::io::{stdin, stdout, Write};

    let mut ctx = load();
    let f = stdin();
    let mut line = String::new();
    loop {
        print!("> ");
        stdout().flush().unwrap();
        match f.read_line(&mut line) {
            Ok(_) => (),
            Err(_) => return
        };
        match one_line(&mut ctx, line.trim()) {
            Ok(v) => ctx.print(&v),
            Err(e) => println!("{}", e)
        };
        line.clear();
    }
}
