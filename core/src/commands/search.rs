// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::loader::Context;
use crate::output::{NumberParts, SearchReply};
use crate::types::{BaseUnit, Dimensionality};

pub(crate) fn search_internal<'a>(
    ctx: &'a Context,
    query: &str,
    num_results: usize,
) -> Vec<&'a str> {
    let base_units = ctx.registry.base_units.iter().map(|dim| &dim.id[..]);
    let units = ctx.registry.units.keys().map(|name| &name[..]);
    let substances = ctx.registry.substances.keys().map(|name| &name[..]);

    let iter = base_units.chain(units).chain(substances);
    crate::algorithms::search_impl(iter, query, num_results)
}

pub fn search(ctx: &Context, query: &str, num_results: usize) -> SearchReply {
    SearchReply {
        results: search_internal(ctx, query, num_results)
            .into_iter()
            .map(|name| {
                let parts = ctx
                    .lookup(name)
                    .and_then(|v| v.to_number())
                    .map(|x| x.to_parts(ctx))
                    .or_else(|| {
                        if ctx.registry.substances.get(name).is_some() {
                            Some(NumberParts {
                                quantity: Some("substance".to_owned()),
                                ..Default::default()
                            })
                        } else {
                            None
                        }
                    })
                    .expect("Search returned non-existent result");
                let raw = Dimensionality::base_unit(BaseUnit::new(name));
                NumberParts {
                    unit: Some(name.to_owned()),
                    raw_unit: Some(raw),
                    quantity: parts.quantity,
                    ..Default::default()
                }
            })
            .collect(),
    }
}
