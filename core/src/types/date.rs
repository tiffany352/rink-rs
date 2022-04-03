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
