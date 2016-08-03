pub mod unit_defs;
pub mod eval;

fn main() {
    use std::io::{stdin, stdout, Read, Write};
    use std::fs::File;

    let mut f = File::open("units.txt").unwrap();
    let mut buf = vec![];
    f.read_to_end(&mut buf).unwrap();
    let string = String::from_utf8_lossy(&*buf);
    let mut iter = unit_defs::TokenIterator::new(&*string).peekable();
    //let res = unit_defs::tokens(&mut iter);
    let res = unit_defs::parse(&mut iter);
    let ctx = eval::Context::new(res);
    //println!("{:#?}", res);
    let f = stdin();
    let mut line = String::new();
    loop {
        print!("> ");
        stdout().flush().unwrap();
        match f.read_line(&mut line) {
            Ok(_) => (),
            Err(_) => return
        };
        {
            let mut iter = unit_defs::TokenIterator::new(&*line).peekable();
            let expr = unit_defs::parse_expr(&mut iter);
            match ctx.eval(&expr) {
                Ok(value) => ctx.print(&value),
                Err(e) => println!("{}", e)
            };
        }
        line.clear();
    }
}
