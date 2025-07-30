// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::fmt;

use jiff::{RoundMode, Span, Unit, ZonedDifference};

use crate::types::DateTime;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub(crate) struct ApproxDuration {
    unit: Unit,
    value: i32,
}

impl ApproxDuration {
    fn new(unit: Unit, value: i32) -> ApproxDuration {
        ApproxDuration { unit, value }
    }

    pub(crate) fn between(date: &DateTime, now: &DateTime) -> ApproxDuration {
        let difference = ZonedDifference::new(&now.dt)
            .smallest(Unit::Nanosecond)
            .largest(Unit::Year)
            .mode(RoundMode::Trunc);

        let date = date.dt.with_time_zone(now.dt.time_zone().clone());
        let span = date.since(difference).unwrap();
        ApproxDuration::from_span(&span, now)
    }

    fn from_span(span: &Span, now: &DateTime) -> ApproxDuration {
        let years = get_unit(span, now, Unit::Year);
        if years != 0 {
            return ApproxDuration::new(Unit::Year, years as i32);
        }

        let months = get_unit(span, now, Unit::Month);
        if months != 0 {
            return ApproxDuration::new(Unit::Month, months as i32);
        }

        let weeks = get_unit(span, now, Unit::Week);
        if weeks != 0 {
            return ApproxDuration::new(Unit::Week, weeks as i32);
        }

        let days = get_unit(span, now, Unit::Day);
        if days != 0 {
            if days % 7 == 0 {
                return ApproxDuration::new(Unit::Week, days as i32 / 7);
            }
            return ApproxDuration::new(Unit::Day, days as i32);
        }

        let hours = get_unit(span, now, Unit::Hour);
        if hours != 0 {
            return ApproxDuration::new(Unit::Hour, hours as i32);
        }

        let minutes = get_unit(span, now, Unit::Minute);
        if minutes != 0 {
            return ApproxDuration::new(Unit::Minute, minutes as i32);
        }

        let seconds = get_unit(span, now, Unit::Second);
        if seconds != 0 {
            return ApproxDuration::new(Unit::Second, seconds as i32);
        }

        let millis = get_unit(span, now, Unit::Millisecond);
        if millis != 0 {
            return ApproxDuration::new(Unit::Millisecond, millis as i32);
        }

        let micros = get_unit(span, now, Unit::Microsecond);
        if micros != 0 {
            return ApproxDuration::new(Unit::Microsecond, micros as i32);
        }

        let nanos = get_unit(span, now, Unit::Nanosecond);
        return ApproxDuration::new(Unit::Nanosecond, nanos as i32);
    }
}

fn get_unit(span: &Span, _now: &DateTime, unit: Unit) -> i32 {
    match unit {
        Unit::Year => span.get_years() as i32,
        Unit::Month => span.get_months(),
        Unit::Week => span.get_weeks(),
        Unit::Day => span.get_days(),
        Unit::Hour => span.get_hours(),
        Unit::Minute => span.get_minutes() as i32,
        Unit::Second => span.get_seconds() as i32,
        Unit::Millisecond => span.get_milliseconds() as i32,
        Unit::Microsecond => span.get_microseconds() as i32,
        Unit::Nanosecond => span.get_nanoseconds() as i32,
    }
}

fn singular(unit: Unit) -> &'static str {
    match unit {
        Unit::Year => "year",
        Unit::Month => "month",
        Unit::Week => "week",
        Unit::Day => "day",
        Unit::Hour => "hour",
        Unit::Minute => "minute",
        Unit::Second => "second",
        Unit::Millisecond => "millisecond",
        Unit::Microsecond => "microsecond",
        Unit::Nanosecond => "nanosecond",
    }
}

fn plural(unit: Unit) -> &'static str {
    match unit {
        Unit::Year => "years",
        Unit::Month => "months",
        Unit::Week => "weeks",
        Unit::Day => "days",
        Unit::Hour => "hours",
        Unit::Minute => "minutes",
        Unit::Second => "seconds",
        Unit::Millisecond => "milliseconds",
        Unit::Microsecond => "microseconds",
        Unit::Nanosecond => "nanoseconds",
    }
}

impl fmt::Display for ApproxDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value {
            0 => write!(f, "now"),
            1 => write!(f, "in 1 {}", singular(self.unit)),
            -1 => write!(f, "1 {} ago", singular(self.unit)),
            x if x > 0 => write!(f, "in {} {}", self.value, plural(self.unit)),
            _ => write!(f, "{} {} ago", -self.value, plural(self.unit)),
        }
    }
}

#[cfg(test)]
mod tests {
    use jiff::{Span, Unit, Zoned};

    use crate::types::{humanize::ApproxDuration, DateTime};

    fn dt(input: &str) -> DateTime {
        input.parse::<Zoned>().unwrap().into()
    }

    #[test]
    fn test_with_dates() {
        let now = dt("2025-07-29 19:41:54[US/Pacific]");

        fn s() -> Span {
            Span::new()
        }

        fn expected(unit: Unit, value: i32) -> ApproxDuration {
            ApproxDuration::new(unit, value)
        }

        let actual = |span: Span| -> ApproxDuration {
            let date = (&now.dt + span).into();
            ApproxDuration::between(&date, &now)
        };

        assert_eq!(actual(s().nanoseconds(1)), expected(Unit::Nanosecond, 1));
        assert_eq!(
            actual(s().nanoseconds(999)),
            expected(Unit::Nanosecond, 999)
        );
        assert_eq!(actual(s().microseconds(1)), expected(Unit::Microsecond, 1));
        assert_eq!(
            actual(s().microseconds(999)),
            expected(Unit::Microsecond, 999)
        );
        assert_eq!(actual(s().milliseconds(1)), expected(Unit::Millisecond, 1));
        assert_eq!(
            actual(s().milliseconds(999)),
            expected(Unit::Millisecond, 999)
        );
        assert_eq!(actual(s().seconds(1)), expected(Unit::Second, 1));
        assert_eq!(actual(s().seconds(59)), expected(Unit::Second, 59));
        assert_eq!(actual(s().minutes(1)), expected(Unit::Minute, 1));
        assert_eq!(actual(s().minutes(59)), expected(Unit::Minute, 59));
        assert_eq!(actual(s().hours(1)), expected(Unit::Hour, 1));
        assert_eq!(actual(s().hours(23)), expected(Unit::Hour, 23));
        assert_eq!(actual(s().days(1)), expected(Unit::Day, 1));
        assert_eq!(actual(s().days(6)), expected(Unit::Day, 6));
        assert_eq!(actual(s().days(8)), expected(Unit::Day, 8));
        assert_eq!(actual(s().weeks(1)), expected(Unit::Week, 1));
        assert_eq!(actual(s().months(1)), expected(Unit::Month, 1));
        assert_eq!(actual(s().years(1)), expected(Unit::Year, 1));

        // subtract instead of add
        let actual = |span: Span| -> ApproxDuration {
            let date = (&now.dt - span).into();
            ApproxDuration::between(&date, &now)
        };

        assert_eq!(actual(s().nanoseconds(1)), expected(Unit::Nanosecond, -1));
        assert_eq!(
            actual(s().nanoseconds(999)),
            expected(Unit::Nanosecond, -999)
        );
        assert_eq!(actual(s().microseconds(1)), expected(Unit::Microsecond, -1));
        assert_eq!(
            actual(s().microseconds(999)),
            expected(Unit::Microsecond, -999)
        );
        assert_eq!(actual(s().milliseconds(1)), expected(Unit::Millisecond, -1));
        assert_eq!(
            actual(s().milliseconds(999)),
            expected(Unit::Millisecond, -999)
        );
        assert_eq!(actual(s().seconds(1)), expected(Unit::Second, -1));
        assert_eq!(actual(s().seconds(59)), expected(Unit::Second, -59));
        assert_eq!(actual(s().minutes(1)), expected(Unit::Minute, -1));
        assert_eq!(actual(s().minutes(59)), expected(Unit::Minute, -59));
        assert_eq!(actual(s().hours(1)), expected(Unit::Hour, -1));
        assert_eq!(actual(s().hours(23)), expected(Unit::Hour, -23));
        assert_eq!(actual(s().days(1)), expected(Unit::Day, -1));
        assert_eq!(actual(s().days(6)), expected(Unit::Day, -6));
        assert_eq!(actual(s().days(8)), expected(Unit::Day, -8));
        assert_eq!(actual(s().weeks(1)), expected(Unit::Week, -1));
        assert_eq!(actual(s().months(1)), expected(Unit::Month, -1));
        assert_eq!(actual(s().years(1)), expected(Unit::Year, -1));
    }

    #[test]
    fn test_format() {
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Nanosecond, 0)),
            "now"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Nanosecond, 1)),
            "in 1 nanosecond"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Nanosecond, 2)),
            "in 2 nanoseconds"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Nanosecond, -1)),
            "1 nanosecond ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Nanosecond, -2)),
            "2 nanoseconds ago"
        );

        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Microsecond, 0)),
            "now"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Microsecond, 1)),
            "in 1 microsecond"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Microsecond, 2)),
            "in 2 microseconds"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Microsecond, -1)),
            "1 microsecond ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Microsecond, -2)),
            "2 microseconds ago"
        );

        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Millisecond, 0)),
            "now"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Millisecond, 1)),
            "in 1 millisecond"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Millisecond, 2)),
            "in 2 milliseconds"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Millisecond, -1)),
            "1 millisecond ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Millisecond, -2)),
            "2 milliseconds ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Second, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Second, 1)),
            "in 1 second"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Second, 2)),
            "in 2 seconds"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Second, -1)),
            "1 second ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Second, -2)),
            "2 seconds ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Minute, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Minute, 1)),
            "in 1 minute"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Minute, 2)),
            "in 2 minutes"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Minute, -1)),
            "1 minute ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Minute, -2)),
            "2 minutes ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Hour, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Hour, 1)),
            "in 1 hour"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Hour, 2)),
            "in 2 hours"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Hour, -1)),
            "1 hour ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Hour, -2)),
            "2 hours ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Day, 0)), "now");
        assert_eq!(format!("{}", ApproxDuration::new(Unit::Day, 1)), "in 1 day");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Day, 2)),
            "in 2 days"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Day, -1)),
            "1 day ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Day, -2)),
            "2 days ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Week, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, 1)),
            "in 1 week"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, 2)),
            "in 2 weeks"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, -1)),
            "1 week ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, -2)),
            "2 weeks ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Month, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, 1)),
            "in 1 month"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, 2)),
            "in 2 months"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, -1)),
            "1 month ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, -2)),
            "2 months ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Year, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, 1)),
            "in 1 year"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, 2)),
            "in 2 years"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, -1)),
            "1 year ago"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, -2)),
            "2 years ago"
        );
    }

    #[test]
    fn test_spans() {
        let now = DateTime::default();
        assert_eq!(
            ApproxDuration::from_span(&Span::new().years(1), &now),
            ApproxDuration::new(Unit::Year, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().months(1), &now),
            ApproxDuration::new(Unit::Month, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().weeks(1), &now),
            ApproxDuration::new(Unit::Week, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().days(1), &now),
            ApproxDuration::new(Unit::Day, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().hours(1), &now),
            ApproxDuration::new(Unit::Hour, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().minutes(1), &now),
            ApproxDuration::new(Unit::Minute, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().seconds(1), &now),
            ApproxDuration::new(Unit::Second, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().milliseconds(1), &now),
            ApproxDuration::new(Unit::Millisecond, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().microseconds(1), &now),
            ApproxDuration::new(Unit::Microsecond, 1)
        );
        assert_eq!(
            ApproxDuration::from_span(&Span::new().nanoseconds(1), &now),
            ApproxDuration::new(Unit::Nanosecond, 1)
        );
    }
}
