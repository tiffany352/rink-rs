// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate rink;
#[cfg(feature = "rustyline")]
extern crate rustyline;

use rink::*;

#[cfg(feature = "rustyline")]
fn main() {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;
    use std::rc::Rc;
    use std::cell::RefCell;

    struct Completer(Rc<RefCell<Context>>);

    impl rustyline::completion::Completer for Completer {
        fn complete(&self, line: &str, pos: usize) -> rustyline::Result<(usize, Vec<String>)> {
            let name = line.rsplit(|x:char| !x.is_alphanumeric() && x != '_').next();
            let name = match name {
                Some(res) if res.len() > 0 => res,
                _ => return Ok((pos, vec![]))
            };
            fn inner(ctx: &Context, name: &str) -> Vec<String> {
                let mut out = vec![];
                for k in &ctx.dimensions {
                    if (**k.0).starts_with(name) {
                        out.push((*k.0).clone());
                    }
                }
                for (ref k, _) in &ctx.units {
                    if k.starts_with(name) {
                        out.push((*k).clone());
                    }
                }
                for (_, ref k) in &ctx.aliases {
                    if k.starts_with(name) {
                        out.push((*k).clone());
                    }
                }
                out
            }

            let mut out = vec![];
            out.append(&mut inner(&*self.0.borrow(), name));
            for &(ref k, _) in &self.0.borrow().prefixes {
                if name.starts_with(&**k) {
                    out.append(&mut inner(&*self.0.borrow(), &name[k.len()..])
                               .into_iter().map(|x| format!("{}{}", k, x)).collect());
                } else if k.starts_with(name) {
                    out.insert(0, k.clone());
                }
            }

            Ok((pos - name.len(), out))
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
    let completer = Completer(ctx.clone());
    let mut rl = Editor::new();
    rl.set_completer(Some(&completer));
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match one_line(&mut *ctx.borrow_mut(), &*line) {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("{}", e)
                };
            },
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Readline: {:?}", err);
                break
            },
        }
    }
}

#[cfg(not(feature = "rustyline"))]
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
