// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;

pub(crate) fn btree_merge<K: Ord + Clone, V: Clone, F: Fn(&V, &V) -> Option<V>>(
    left: &BTreeMap<K, V>,
    right: &BTreeMap<K, V>,
    merge_func: F,
) -> BTreeMap<K, V> {
    let mut res = BTreeMap::new();
    let mut a = left.iter().peekable();
    let mut b = right.iter().peekable();
    loop {
        match (a.peek().cloned(), b.peek().cloned()) {
            (Some((akey, _)), Some((bkey, bval))) if akey > bkey => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            }
            (Some((akey, aval)), Some((bkey, _))) if akey < bkey => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            }
            (Some((akey, aval)), Some((bkey, bval))) => {
                // If !(akey > bkey) && !(bkey < akey) then they must be
                // equal. Can't use assert_eq because of the Debug
                // bound.
                assert!(akey == bkey);
                if let Some(v) = merge_func(aval, bval) {
                    res.insert(akey.clone(), v);
                }
                a.next();
                b.next();
            }
            (None, Some((bkey, bval))) => {
                res.insert(bkey.clone(), bval.clone());
                b.next();
            }
            (Some((akey, aval)), None) => {
                res.insert(akey.clone(), aval.clone());
                a.next();
            }
            (None, None) => break,
        }
    }
    res
}
