extern crate rink;

use rink::*;

fn main() {
    use std::io::Read;
    use std::fs::File;

    let mut f = File::open("units.txt").unwrap();
    let mut buf = vec![];
    f.read_to_end(&mut buf).unwrap();
    let string = String::from_utf8_lossy(&*buf);
    let mut iter = unit_defs::TokenIterator::new(&*string).peekable();
    let res = unit_defs::tokens(&mut iter);

    println!("{:#?}", res);
}
