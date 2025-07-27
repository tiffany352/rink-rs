use std::{fmt, ops};

use jiff::{SignedDuration, Timestamp, Zoned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DateTime {
    pub(crate) dt: Zoned,
}

impl DateTime {
    pub fn year(&self) -> i32 {
        self.dt.year() as i32
    }

    pub fn month(&self) -> i32 {
        self.dt.month() as i32
    }

    pub fn day(&self) -> i32 {
        self.dt.day() as i32
    }

    pub fn hour(&self) -> i32 {
        self.dt.hour() as i32
    }

    pub fn minute(&self) -> i32 {
        self.dt.minute() as i32
    }

    pub fn second(&self) -> i32 {
        self.dt.second() as i32
    }

    pub fn nanosecond(&self) -> i32 {
        self.dt.nanosecond() as i32
    }

    pub fn humanize(&self, _now: &DateTime) -> Option<String> {
        None
    }

    pub fn to_rfc3339(&self) -> String {
        jiff::fmt::strtime::format("%F %T", &self.dt).unwrap()
    }

    pub fn convert_to_timezone(&self, timezone: TimeZone) -> DateTime {
        DateTime {
            dt: self.dt.with_time_zone(timezone),
        }
    }

    pub fn now() -> DateTime {
        DateTime { dt: Zoned::now() }
    }

    pub fn from_millis_local(millis: i64) -> DateTime {
        DateTime {
            dt: Timestamp::new(millis / 1000, ((millis % 1000) * 1000_000) as i32)
                .unwrap()
                .to_zoned(TimeZone::system()),
        }
    }

    pub fn checked_sub(&self, right: SignedDuration) -> Option<DateTime> {
        self.dt.checked_sub(right).map(Into::into).ok()
    }

    pub fn checked_add(&self, right: SignedDuration) -> Option<DateTime> {
        self.dt.checked_add(right).map(Into::into).ok()
    }
}

impl Default for DateTime {
    fn default() -> Self {
        DateTime {
            dt: Timestamp::UNIX_EPOCH.to_zoned(TimeZone::system()),
        }
    }
}

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dt.fmt(f)
    }
}

impl<'a> ops::Sub for &'a DateTime {
    type Output = SignedDuration;

    fn sub(self, rhs: Self) -> Self::Output {
        self.dt.duration_until(&rhs.dt)
    }
}

impl From<Zoned> for DateTime {
    fn from(value: Zoned) -> Self {
        DateTime { dt: value }
    }
}

pub use jiff::tz::TimeZone;
