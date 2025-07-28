// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::fmt;

use jiff::{RoundMode, Span, SpanRound, Unit};

use crate::types::DateTime;

pub(crate) struct ApproxDuration {
    unit: Unit,
    value: i32,
}

impl ApproxDuration {
    fn new(unit: Unit, value: i32) -> ApproxDuration {
        ApproxDuration { unit, value }
    }

    pub(crate) fn from_span(span: &Span, now: &DateTime) -> ApproxDuration {
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

fn get_unit(span: &Span, now: &DateTime, unit: Unit) -> i32 {
    let rounding = SpanRound::from(unit)
        .relative(&now.dt)
        .mode(RoundMode::Trunc);
    let span = span.round(rounding);
    let span = if let Ok(span) = span { span } else { return 0 };
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

fn one_ago(unit: Unit) -> &'static str {
    match unit {
        Unit::Year => "last year",
        Unit::Month => "last month",
        Unit::Week => "last week",
        Unit::Day => "yesterday",
        Unit::Hour => "1 hour ago",
        Unit::Minute => "1 minute ago",
        Unit::Second => "1 second ago",
        Unit::Millisecond => "1 millisecond ago",
        Unit::Microsecond => "1 microsecond ago",
        Unit::Nanosecond => "1 nanosecond ago",
    }
}

fn one_from_now(unit: Unit) -> &'static str {
    match unit {
        Unit::Year => "next year",
        Unit::Month => "next month",
        Unit::Week => "next week",
        Unit::Day => "tomorrow",
        Unit::Hour => "in 1 hour",
        Unit::Minute => "in 1 minute",
        Unit::Second => "in 1 second",
        Unit::Millisecond => "in 1 millisecond",
        Unit::Microsecond => "in 1 microsecond",
        Unit::Nanosecond => "in 1 nanosecond",
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
            1 => write!(f, "{}", one_from_now(self.unit)),
            -1 => write!(f, "{}", one_ago(self.unit)),
            x if x > 0 => write!(f, "in {} {}", self.value, plural(self.unit)),
            _ => write!(f, "{} {} ago", -self.value, plural(self.unit)),
        }
    }
}

#[cfg(test)]
mod tests {
    use jiff::Unit;

    use crate::types::humanize::ApproxDuration;

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
        assert_eq!(format!("{}", ApproxDuration::new(Unit::Day, 1)), "tomorrow");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Day, 2)),
            "in 2 days"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Day, -1)),
            "yesterday"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Day, -2)),
            "2 days ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Week, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, 1)),
            "next week"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, 2)),
            "in 2 weeks"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, -1)),
            "last week"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Week, -2)),
            "2 weeks ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Month, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, 1)),
            "next month"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, 2)),
            "in 2 months"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, -1)),
            "last month"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Month, -2)),
            "2 months ago"
        );

        assert_eq!(format!("{}", ApproxDuration::new(Unit::Year, 0)), "now");
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, 1)),
            "next year"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, 2)),
            "in 2 years"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, -1)),
            "last year"
        );
        assert_eq!(
            format!("{}", ApproxDuration::new(Unit::Year, -2)),
            "2 years ago"
        );
    }
}
