pub mod unit_defs;

fn main() {
    use std::io::{stdin, Read};

    let mut f = stdin();//File::open("units.txt").unwrap();
    let mut buf = vec![];
    f.read_to_end(&mut buf).unwrap();
    let string = String::from_utf8_lossy(&*buf);
    //let res = unit_defs::tokens(&mut string.chars().peekable());
    let res = unit_defs::parse(&mut string.chars().peekable());
    println!("{:#?}", res);
}
