use chrono::{Datelike, Timelike};
use std::{fmt, ops};

#[derive(Debug, Clone, Copy)]
pub(crate) enum GenericTimeZone {
    Local(chrono::Local),
    Fixed(chrono::FixedOffset),
    TimeZone(chrono_tz::Tz),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum GenericOffset {
    Local(<chrono::Local as chrono::TimeZone>::Offset),
    Fixed(<chrono::FixedOffset as chrono::TimeZone>::Offset),
    TimeZone(<chrono_tz::Tz as chrono::TimeZone>::Offset),
}

impl chrono::Offset for GenericOffset {
    fn fix(&self) -> chrono::FixedOffset {
        match self {
            GenericOffset::Local(offset) => offset.fix(),
            GenericOffset::Fixed(offset) => offset.fix(),
            GenericOffset::TimeZone(offset) => offset.fix(),
        }
    }
}

impl chrono::TimeZone for GenericTimeZone {
    type Offset = GenericOffset;

    fn from_offset(offset: &Self::Offset) -> Self {
        match offset {
            GenericOffset::Local(offset) => {
                GenericTimeZone::Local(chrono::Local::from_offset(offset))
            }
            GenericOffset::Fixed(offset) => {
                GenericTimeZone::Fixed(chrono::FixedOffset::from_offset(offset))
            }
            GenericOffset::TimeZone(offset) => {
                GenericTimeZone::TimeZone(chrono::TimeZone::from_offset(offset))
            }
        }
    }

    fn offset_from_local_date(
        &self,
        naive: &chrono::NaiveDate,
    ) -> chrono::MappedLocalTime<Self::Offset> {
        match self {
            GenericTimeZone::Local(local) => local
                .offset_from_local_date(naive)
                .map(GenericOffset::Local),
            GenericTimeZone::Fixed(fixed_offset) => fixed_offset
                .offset_from_local_date(naive)
                .map(GenericOffset::Fixed),
            GenericTimeZone::TimeZone(tz) => tz
                .offset_from_local_date(naive)
                .map(GenericOffset::TimeZone),
        }
    }

    fn offset_from_local_datetime(
        &self,
        naive: &chrono::NaiveDateTime,
    ) -> chrono::MappedLocalTime<Self::Offset> {
        match self {
            GenericTimeZone::Local(local) => local
                .offset_from_local_datetime(naive)
                .map(GenericOffset::Local),
            GenericTimeZone::Fixed(fixed_offset) => fixed_offset
                .offset_from_local_datetime(naive)
                .map(GenericOffset::Fixed),
            GenericTimeZone::TimeZone(tz) => tz
                .offset_from_local_datetime(naive)
                .map(GenericOffset::TimeZone),
        }
    }

    fn offset_from_utc_date(&self, utc: &chrono::NaiveDate) -> Self::Offset {
        match self {
            GenericTimeZone::Local(local) => GenericOffset::Local(local.offset_from_utc_date(utc)),
            GenericTimeZone::Fixed(fixed_offset) => {
                GenericOffset::Fixed(fixed_offset.offset_from_utc_date(utc))
            }
            GenericTimeZone::TimeZone(tz) => GenericOffset::TimeZone(tz.offset_from_utc_date(utc)),
        }
    }

    fn offset_from_utc_datetime(&self, utc: &chrono::NaiveDateTime) -> Self::Offset {
        match self {
            GenericTimeZone::Local(local) => {
                GenericOffset::Local(local.offset_from_utc_datetime(utc))
            }
            GenericTimeZone::Fixed(fixed_offset) => {
                GenericOffset::Fixed(fixed_offset.offset_from_utc_datetime(utc))
            }
            GenericTimeZone::TimeZone(tz) => {
                GenericOffset::TimeZone(tz.offset_from_utc_datetime(utc))
            }
        }
    }
}

impl fmt::Display for GenericTimeZone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericTimeZone::Local(_local) => write!(f, "Local"),
            GenericTimeZone::Fixed(fixed_offset) => fixed_offset.fmt(f),
            GenericTimeZone::TimeZone(tz) => tz.fmt(f),
        }
    }
}

impl fmt::Display for GenericOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericOffset::Local(local) => local.fmt(f),
            GenericOffset::Fixed(fixed_offset) => fixed_offset.fmt(f),
            GenericOffset::TimeZone(tz) => tz.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DateTime {
    dt: chrono::DateTime<GenericTimeZone>,
}

impl DateTime {
    pub(crate) fn to_chrono(&self) -> chrono::DateTime<GenericTimeZone> {
        self.dt
    }

    pub fn year(&self) -> i32 {
        self.dt.year()
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

    pub fn checked_add_signed(&self, right: chrono::TimeDelta) -> Option<DateTime> {
        Some(DateTime {
            dt: self.dt.checked_add_signed(right)?,
        })
    }

    pub fn checked_sub_signed(&self, right: chrono::TimeDelta) -> Option<DateTime> {
        Some(DateTime {
            dt: self.dt.checked_sub_signed(right)?,
        })
    }

    pub fn humanize(&self, now: DateTime) -> Option<String> {
        if cfg!(feature = "chrono-humanize") {
            use chrono_humanize::HumanTime;
            let duration = *self - now;
            Some(HumanTime::from(duration).to_string())
        } else {
            None
        }
    }

    pub fn to_rfc3339(&self) -> String {
        self.dt.to_rfc3339()
    }

    pub fn convert_to_offset(&self, offset: i64) -> DateTime {
        DateTime {
            dt: self.dt.with_timezone(&GenericTimeZone::Fixed(
                chrono::FixedOffset::east_opt(offset as i32).unwrap(),
            )),
        }
    }

    pub fn convert_to_timezone(&self, timezone: TimeZone) -> DateTime {
        DateTime {
            dt: self.dt.with_timezone(&GenericTimeZone::TimeZone(timezone)),
        }
    }

    pub fn now() -> DateTime {
        DateTime {
            dt: chrono::Local::now().with_timezone(&GenericTimeZone::Local(chrono::Local)),
        }
    }
}

impl Default for DateTime {
    fn default() -> Self {
        <chrono::Local as chrono::TimeZone>::timestamp_opt(&chrono::Local, 0, 0)
            .unwrap()
            .into()
    }
}

impl fmt::Display for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.dt.fmt(f)
    }
}

impl ops::Sub for DateTime {
    type Output = Duration;

    fn sub(self, rhs: Self) -> Self::Output {
        self.dt - rhs.dt
    }
}

impl From<chrono::DateTime<chrono::Local>> for DateTime {
    fn from(value: chrono::DateTime<chrono::Local>) -> DateTime {
        DateTime {
            dt: value.with_timezone(&GenericTimeZone::Local(value.timezone())),
        }
    }
}

impl From<chrono::DateTime<chrono::FixedOffset>> for DateTime {
    fn from(value: chrono::DateTime<chrono::FixedOffset>) -> DateTime {
        DateTime {
            dt: value.with_timezone(&GenericTimeZone::Fixed(value.timezone())),
        }
    }
}

impl From<chrono::DateTime<chrono_tz::Tz>> for DateTime {
    fn from(value: chrono::DateTime<chrono_tz::Tz>) -> DateTime {
        DateTime {
            dt: value.with_timezone(&GenericTimeZone::TimeZone(value.timezone())),
        }
    }
}

// TODO: replace with custom types
pub type TimeZone = chrono_tz::Tz;
pub type Duration = chrono::Duration;
