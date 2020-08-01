// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use rink_core::*;

#[test]
fn canonicalizations() {
    let ctx = simple_context().unwrap();
    for (name, value) in &ctx.units {
        let canon = match ctx.canonicalize(&*name) {
            Some(x) => x,
            None => continue,
        };
        let cvalue = ctx
            .lookup(&*canon)
            .expect(&*format!("Failed to lookup {}", canon));
        assert_eq!(cvalue, *value, "{} == {}", name, canon);
    }
}
