extern crate rink;

use rink::*;

#[test]
fn canonicalizations() {
    let ctx = load().unwrap();
    for (name, value) in &ctx.units {
        let canon = match ctx.canonicalize(&*name) {
            Some(x) => x,
            None => continue
        };
        let cvalue = ctx.lookup(&*canon).expect(&*format!("Failed to lookup {}", canon));
        assert_eq!(cvalue, *value, "{} == {}", name, canon);
    }
}
