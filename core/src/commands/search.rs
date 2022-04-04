// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::{
    context::Context,
    number::{Dimension, NumberParts},
    output::SearchReply,
};
use std::collections::BTreeMap;

pub(crate) fn search_internal<'a>(
    ctx: &'a Context,
    query: &str,
    num_results: usize,
) -> Vec<&'a str> {
    let dimensions = ctx.dimensions.iter().map(|dim| &dim.id[..]);
    let units = ctx.units.keys().map(|name| &name[..]);
    let quantities = ctx.quantities.values().map(|name| &name[..]);
    let substances = ctx.substances.keys().map(|name| &name[..]);

    let iter = dimensions.chain(units).chain(quantities).chain(substances);
    crate::algorithms::search_impl(iter, query, num_results)
}

pub fn search(ctx: &Context, query: &str, num_results: usize) -> SearchReply {
    SearchReply {
        results: search_internal(ctx, query, num_results)
            .into_iter()
            .map(|name| {
                let parts = ctx
                    .lookup(name)
                    .map(|x| x.to_parts(ctx))
                    .or_else(|| {
                        if ctx.substances.get(name).is_some() {
                            Some(NumberParts {
                                quantity: Some("substance".to_owned()),
                                ..Default::default()
                            })
                        } else {
                            None
                        }
                    })
                    .expect("Search returned non-existent result");
                let mut raw = BTreeMap::new();
                raw.insert(Dimension::new(name), 1);
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
