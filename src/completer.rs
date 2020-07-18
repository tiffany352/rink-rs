use std::sync::{Arc, Mutex};

use linefeed::{Completer, Completion, Prompter, Suffix, Terminal};

use rink_core::{Context, Number};

pub struct RinkCompleter(Arc<Mutex<Context>>);

impl RinkCompleter {
    pub fn new(context: Arc<Mutex<Context>>) -> RinkCompleter {
        RinkCompleter(context)
    }
}

impl<Term: Terminal> Completer<Term> for RinkCompleter {
    fn complete(
        &self,
        name: &str,
        _prompter: &Prompter<Term>,
        _start: usize,
        _end: usize,
    ) -> Option<Vec<Completion>> {
        fn inner(ctx: &Context, name: &str) -> Vec<Completion> {
            let mut out = vec![];
            for k in &ctx.dimensions {
                if (**k.id).starts_with(name) {
                    out.push(Completion {
                        completion: (*k.id).clone(),
                        display: Some(format!("{} (base unit)", k.id)),
                        suffix: Suffix::Default,
                    });
                }
            }
            for k in ctx.units.keys() {
                if k.starts_with(name) {
                    let def = &ctx.definitions.get(&**k);
                    let def = def.map(|def| format!("{} = ", def)).unwrap_or_default();
                    let res = ctx.lookup(k).unwrap();
                    let parts = res.to_parts(ctx);
                    out.push(Completion {
                        completion: (*k).clone(),
                        display: Some(format!("{} ({}{})", k, def, parts.format("n u p"))),
                        suffix: Suffix::Default,
                    });
                }
            }
            for (ref k, ref sub) in &ctx.substances {
                if k.starts_with(name) {
                    out.push(Completion {
                        completion: (*k).clone(),
                        display: Some(format!(
                            "{} (substance{})",
                            k,
                            ctx.docs
                                .get(&**k)
                                .map(|x| format!(", {}", x))
                                .unwrap_or_default()
                        )),
                        suffix: Suffix::Default,
                    });
                }
                for (pk, prop) in &sub.properties.properties {
                    if pk.starts_with(name) {
                        out.push(Completion {
                            completion: format!("{} of", pk),
                            display: Some(format!(
                                "{} of (property of {}{}{})",
                                pk,
                                k,
                                (&prop.input / &prop.output)
                                    .expect("Non-zero substance properties")
                                    .to_parts(ctx)
                                    .quantity
                                    .map(|x| format!("; {}", x))
                                    .unwrap_or_default(),
                                prop.doc
                                    .as_ref()
                                    .map(|x| format!("; {}", x))
                                    .unwrap_or_default(),
                            )),
                            suffix: Suffix::Default,
                        });
                    }
                    if prop.input_name.starts_with(name) {
                        out.push(Completion {
                            completion: format!("{} of", prop.input_name),
                            display: Some(format!(
                                "{} of (property of {} {}{}{})",
                                prop.input_name,
                                k,
                                prop.output_name,
                                prop.input
                                    .to_parts(ctx)
                                    .quantity
                                    .map(|x| format!("; {}", x))
                                    .unwrap_or_default(),
                                prop.doc
                                    .as_ref()
                                    .map(|x| format!("; {}", x))
                                    .unwrap_or_default(),
                            )),
                            suffix: Suffix::Default,
                        });
                    }
                    if prop.output_name.starts_with(name) {
                        out.push(Completion {
                            completion: format!("{} of", prop.output_name),
                            display: Some(format!(
                                "{} of (property of {} {}{}{})",
                                prop.output_name,
                                k,
                                prop.input_name,
                                prop.output
                                    .to_parts(ctx)
                                    .quantity
                                    .map(|x| format!("; {}", x))
                                    .unwrap_or_default(),
                                prop.doc
                                    .as_ref()
                                    .map(|x| format!("; {}", x))
                                    .unwrap_or_default(),
                            )),
                            suffix: Suffix::Default,
                        });
                    }
                }
            }
            for (ref unit, ref k) in &ctx.quantities {
                if k.starts_with(name) {
                    out.push(Completion {
                        completion: (*k).clone(),
                        display: Some(format!(
                            "{} (quantity; {})",
                            k,
                            Number::unit_to_string(unit)
                        )),
                        suffix: Suffix::Default,
                    });
                }
            }
            out
        }

        let mut out = vec![];
        let ctx = self.0.lock().unwrap();
        out.append(&mut inner(&ctx, name));
        for &(ref k, ref v) in &ctx.prefixes {
            if name.starts_with(&**k) {
                out.append(
                    &mut inner(&ctx, &name[k.len()..])
                        .into_iter()
                        .map(|x| Completion {
                            completion: format!("{}{}", k, x.completion),
                            display: Some(format!("{} {}", k, x.display.unwrap())),
                            suffix: Suffix::Default,
                        })
                        .collect(),
                );
            } else if k.starts_with(name) {
                out.insert(
                    0,
                    Completion {
                        completion: k.clone(),
                        display: Some(format!("{} ({:?} prefix)", k, v.value)),
                        suffix: Suffix::Default,
                    },
                );
            }
        }

        Some(out)
    }
}
