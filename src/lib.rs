pub mod unit_defs;
pub mod eval;

pub use eval::{Context, Value};

pub fn load() -> Context {
    use std::io::Read;
    use std::fs::File;

    let mut f = File::open("units.txt").unwrap();
    let mut buf = vec![];
    f.read_to_end(&mut buf).unwrap();
    let string = String::from_utf8_lossy(&*buf);
    let mut iter = unit_defs::TokenIterator::new(&*string).peekable();
    let res = unit_defs::parse(&mut iter);

    eval::Context::new(res)
}

pub fn one_line(ctx: &mut Context, line: &str) -> Result<Value, String> {
    let mut iter = unit_defs::TokenIterator::new(line.trim()).peekable();
    let expr = unit_defs::parse_expr(&mut iter);
    ctx.eval(&expr)
}
