// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use chrono::{DateTime, FixedOffset, TimeZone};
use chrono_tz::Tz;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenericDateTime {
    Fixed(DateTime<FixedOffset>),
    Timezone(DateTime<Tz>),
}

impl GenericDateTime {
    pub fn with_timezone<Tz: TimeZone>(&self, tz: &Tz) -> DateTime<Tz> {
        match *self {
            GenericDateTime::Fixed(ref d) => d.with_timezone(tz),
            GenericDateTime::Timezone(ref d) => d.with_timezone(tz),
        }
    }
}
