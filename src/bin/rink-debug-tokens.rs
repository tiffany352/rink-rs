// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;

use rink::gnu_units::*;

fn main() {
    use std::io::Read;
    use std::fs::File;

    let mut f = File::open("definitions.units").unwrap();
    let mut buf = vec![];
    f.read_to_end(&mut buf).unwrap();
    let string = String::from_utf8_lossy(&*buf);
    let mut iter = TokenIterator::new(&*string).peekable();
    let res = tokens(&mut iter);

    for tok in res {
        match tok {
            Token::Eof => panic!(),
            Token::Newline => print!("\n"),
            Token::Doc(doc) => print!("?? {}\n", doc),
            Token::Ident(name) => print!("`{}` ", name),
            Token::Number(i, f, e) => {
                print!("{}", i);
                if let Some(f) = f {
                    print!(".{}", f);
                }
                if let Some(e) = e {
                    print!("e{}", e);
                }
                print!(" ");
            },
            Token::LPar => print!("("),
            Token::RPar => print!(")"),
            Token::Bang => print!("!"),
            Token::Slash => print!("/"),
            Token::Pipe => print!("|"),
            Token::Caret => print!("^"),
            Token::Plus => print!("+"),
            Token::Dash => print!("-"),
            Token::Asterisk => print!("*"),
            Token::Question => print!("?"),
            Token::Error(e) => print!("<error: {}>", e),
        }
    }
}
