// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;
#[cfg(feature = "linefeed")]
extern crate linefeed;

use rink::*;

#[cfg(feature = "linefeed")]
fn main() {
    use linefeed::{Reader, Terminal, Completer, Completion};
    use std::rc::Rc;
    use std::cell::RefCell;

    struct RinkCompleter(Rc<RefCell<Context>>);

    impl<Term: Terminal> Completer<Term> for RinkCompleter {
        fn complete(&self, name: &str, _reader: &Reader<Term>, _start: usize, _end: usize)
                    -> Option<Vec<Completion>> {
            fn inner(ctx: &Context, name: &str) -> Vec<Completion> {
                let mut out = vec![];
                for k in &ctx.dimensions {
                    if (**k.0).starts_with(name) {
                        out.push(Completion {
                            completion: (*k.0).clone(),
                            display: Some(format!("{} (base unit)", k.0)),
                            suffix: None,
                        });
                    }
                }
                for (ref k, _) in &ctx.units {
                    if k.starts_with(name) {
                        let ref def = ctx.definitions.get(&**k);
                        let def = match def {
                            &Some(ref def) => format!("{} = ", def),
                            &None => format!("")
                        };
                        let res = ctx.lookup(k).unwrap();
                        let raw = res.show_number_part();
                        let unit = res.unit_name(ctx);
                        let base_units = Number::unit_to_string(&res.1);
                        let base_units = if unit == base_units { None } else { Some(base_units) };
                        let quantity = ctx.quantities.get(&res.1);
                        let unit = if unit.len() > 0 {
                            format!(" {}", unit)
                        } else {
                            unit
                        };
                        let parens = match (quantity, base_units) {
                            (Some(quantity), Some(base)) => format!(" ({}; {})", quantity, base),
                            (Some(quantity), None) => format!(" ({})", quantity),
                            (None, Some(base)) => format!(" ({})", base),
                            (None, None) => format!(""),
                        };
                        out.push(Completion {
                            completion: (*k).clone(),
                            display: Some(format!("{} ({}{}{}{})", k, def, raw, unit, parens)),
                            suffix: None,
                        });
                    }
                }
                for (ref unit, ref k) in &ctx.quantities {
                    if k.starts_with(name) {
                        out.push(Completion {
                            completion: (*k).clone(),
                            display: Some(format!("{} (quantity; {})",
                                                  k, Number::unit_to_string(unit))),
                            suffix: None,
                        });
                    }
                }
                out
            }

            let mut out = vec![];
            out.append(&mut inner(&*self.0.borrow(), name));
            for &(ref k, ref v) in &self.0.borrow().prefixes {
                if name.starts_with(&**k) {
                    out.append(&mut inner(&*self.0.borrow(), &name[k.len()..])
                               .into_iter().map(|x| Completion {
                                   completion: format!("{}{}", k, x.completion),
                                   display: Some(format!("{} {}", k, x.display.unwrap())),
                                   suffix: None,
                               }).collect());
                } else if k.starts_with(name) {
                    out.insert(0, Completion {
                        completion: k.clone(),
                        display: Some(format!("{} ({:?} prefix)", k, v.0)),
                        suffix: None,
                    });
                }
            }

            Some(out)
        }
    }

    let ctx = match load() {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return
        }
    };
    let ctx = Rc::new(RefCell::new(ctx));
    let completer = RinkCompleter(ctx.clone());
    let mut rl = Reader::new("rink").unwrap();
    rl.set_prompt("> ");
    rl.set_completer(Rc::new(completer));
    loop {
        let readline = rl.read_line();
        match readline {
            Ok(Some(ref line)) if line == "quit" => {
                println!("");
                break
            },
            Ok(Some(ref line)) if line == "help" => {
                println!("For information on how to use Rink, see the manual: \
                          https://github.com/tiffany352/rink-rs/wiki/Rink-Manual");
            },
            Ok(Some(line)) => {
                rl.add_history(line.clone());
                match one_line(&mut *ctx.borrow_mut(), &*line) {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("{}", e)
                };
            },
            Ok(None) => {
                println!("");
                break
            },
            Err(err) => {
                println!("Readline: {:?}", err);
                break
            },
        }
    }
}

#[cfg(not(feature = "linefeed"))]
fn main() {
    use std::io::{stdin, stdout, Write};

    let mut ctx = match load() {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return
        }
    };
    let f = stdin();
    let mut line = String::new();
    loop {
        print!("> ");
        stdout().flush().unwrap();
        match f.read_line(&mut line) {
            Ok(_) => (),
            Err(_) => return
        };
        match one_line(&mut ctx, &*line) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("{}", e)
        };
        line.clear();
    }
}
