// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use jiff::civil::{Date, Era, ISOWeekDate, Time, Weekday};
use jiff::fmt::strtime::Meridiem;
use jiff::tz::Offset;
use jiff::SignedDuration;

use crate::ast::{DateMatch, DatePattern, DateToken};
use crate::loader::Context;
use crate::types::{BaseUnit, BigInt, BigRat, DateTime, Dimensionality, Number, Numeric, TimeZone};
use std::iter::Peekable;

fn parse_fixed(value: &str, digits: usize) -> Option<i32> {
    if digits != 0 && value.len() != digits {
        return None;
    }
    i32::from_str_radix(value, 10).ok()
}

fn parse_range(value: &str, digits: usize, range: std::ops::RangeInclusive<i32>) -> Option<i32> {
    parse_fixed(value, digits).filter(|v| range.contains(v))
}

fn numeric_match(
    tok: Option<&DateToken>,
    name: &str,
    digits: usize,
    range: std::ops::RangeInclusive<i32>,
) -> Result<i32, String> {
    let tok = tok.ok_or_else(|| format!("Expected {}-digit {}, got eof", digits, name))?;

    if let DateToken::Number(ref s, None) = tok {
        parse_range(s, digits, range.clone())
            .ok_or(format!("Expected {} in range {:?}, got {}", name, range, s))
    } else {
        Err(format!("Expected {}-digit {}, got {}", digits, name, tok))
    }
}

pub(crate) struct Fields {
    era: Option<Era>,
    year: Option<i32>,
    iso_year: Option<i32>,
    month: Option<i32>,
    week: Option<i32>,
    weekday: Option<Weekday>,
    day: Option<i32>,
    ordinal: Option<i32>,

    hour: Option<i32>,
    hour12: Option<i32>,
    meridiem: Option<Meridiem>,
    minute: Option<i32>,
    second: Option<i32>,
    nanosecond: Option<i32>,

    unix_timestamp: Option<i64>,
    time_zone: Option<TimeZone>,
}

impl Fields {
    fn get_year(&self) -> Option<i32> {
        if let Some(iso_year) = self.iso_year {
            return Some(iso_year);
        }
        let era = self.era.unwrap_or(Era::CE);
        if let Some(year) = self.year {
            match era {
                Era::CE => return Some(year),
                Era::BCE => return Some(-year - 1),
            }
        }
        None
    }

    fn to_ymd_date(&self, year: i32) -> Option<Date> {
        let month = self.month?;
        let day = self.day?;
        Date::new(year as i16, month as i8, day as i8).ok()
    }

    fn to_week_date(&self, year: i32) -> Option<Date> {
        let week = self.week?;
        let weekday = self.weekday?;
        ISOWeekDate::new(year as i16, week as i8, weekday)
            .ok()
            .map(Into::into)
    }

    fn to_ordinal_date(&self, year: i32) -> Option<Date> {
        let ordinal = self.ordinal?;
        let date = Date::new(year as i16, 1, 1).ok()?;
        date.with().day_of_year(ordinal as i16).build().ok()
    }

    fn to_date(&self, current_year: i32) -> Option<Date> {
        let year = self.get_year().unwrap_or(current_year);
        self.to_ymd_date(year)
            .or_else(|| self.to_week_date(year))
            .or_else(|| self.to_ordinal_date(year))
    }

    fn get_hour12(&self) -> Option<i32> {
        let hour12 = self.hour12?;
        let meridiem = self.meridiem?;
        Some(match (hour12, meridiem) {
            (12, Meridiem::AM) => 0,
            (12, Meridiem::PM) => 12,
            (x, Meridiem::AM) => x,
            (x, Meridiem::PM) => 12 + x,
        })
    }

    fn get_hour(&self) -> Option<i32> {
        self.hour.or_else(|| self.get_hour12())
    }

    fn to_time(&self) -> Option<Time> {
        let hour = self.get_hour()?;
        let minute = self.minute.unwrap_or(0);
        let second = self.second.unwrap_or(0);
        let nanos = self.nanosecond.unwrap_or(0);
        Time::new(hour as i8, minute as i8, second as i8, nanos).ok()
    }
}

impl Default for Fields {
    fn default() -> Fields {
        Fields {
            era: None,
            year: None,
            iso_year: None,
            month: None,
            week: None,
            weekday: None,
            day: None,
            ordinal: None,
            hour: None,
            hour12: None,
            meridiem: None,
            minute: None,
            second: None,
            nanosecond: None,
            unix_timestamp: None,
            time_zone: None,
        }
    }
}

pub(crate) fn parse_date<I>(
    out: &mut Fields,
    date: &mut Peekable<I>,
    pat: &[DatePattern],
) -> Result<(), String>
where
    I: Iterator<Item = DateToken> + Clone,
{
    use std::borrow::Borrow;

    let tok = date.peek().cloned();

    fn ts<T>(x: Option<T>) -> String
    where
        T: Borrow<DateToken>,
    {
        match x {
            Some(ref x) => format!("`{}`", x.borrow()),
            None => "eof".to_owned(),
        }
    }

    let mut advance = true;

    #[allow(unused_assignments)]
    macro_rules! take {
        ($($pat: pat)|+) => {
            match date.peek().cloned() {
                $(Some($pat))|+ => {date.next().unwrap()},
                x => return Err(format!("Expected {}, got {}", stringify!($($pat)|+), ts(x)))
            }
        };
        ($pat:pat, $var:ident) => {
            match date.peek().cloned() {
                Some($pat) => {date.next(); $var},
                x => return Err(format!("Expected {}, got {}", stringify!($pat), ts(x)))
            }
        }
    }

    let res = match pat.first() {
        None => return Ok(()),
        Some(&DatePattern::Literal(ref l)) => match tok {
            Some(DateToken::Literal(ref s)) if s == l => Ok(()),
            x => Err(format!("Expected `{}`, got {}", l, ts(x))),
        },
        Some(&DatePattern::Match(what)) => match what {
            DateMatch::FullYear => {
                numeric_match(tok.as_ref(), "fullyear", 4, 0..=9999).and_then(|v| {
                    out.year = Some(v);
                    Ok(())
                })
            }
            DateMatch::MonthNum => {
                numeric_match(tok.as_ref(), "monthnum", 2, 1..=12).and_then(|v| {
                    out.month = Some(v);
                    Ok(())
                })
            }
            DateMatch::Day => numeric_match(tok.as_ref(), "day", 0, 1..=31).and_then(|v| {
                out.day = Some(v);
                Ok(())
            }),
            DateMatch::FullDay => numeric_match(tok.as_ref(), "fullday", 2, 1..=31).and_then(|v| {
                out.day = Some(v);
                Ok(())
            }),
            DateMatch::Min => numeric_match(tok.as_ref(), "min", 2, 0..=60).and_then(|v| {
                out.minute = Some(v);
                Ok(())
            }),
            DateMatch::Ordinal => {
                numeric_match(tok.as_ref(), "ordinal", 3, 1..=366).and_then(|v| {
                    out.ordinal = Some(v);
                    Ok(())
                })
            }
            DateMatch::Year => numeric_match(tok.as_ref(), "year", 0, 0..=9999).and_then(|v| {
                out.year = Some(v);
                Ok(())
            }),
            DateMatch::IsoWeek => numeric_match(tok.as_ref(), "isoweek", 2, 1..=53).and_then(|v| {
                out.week = Some(v);
                Ok(())
            }),
            DateMatch::Unix => numeric_match(tok.as_ref(), "unix", 0, 0..=i32::MAX).and_then(|v| {
                out.unix_timestamp = Some(v as i64);
                Ok(())
            }),
            DateMatch::IsoYear => {
                advance = false;
                let x = take!(DateToken::Dash | DateToken::Plus | DateToken::Number(_, None));
                let (sign, num) = match x {
                    DateToken::Dash => (-1, None),
                    DateToken::Plus => (1, None),
                    DateToken::Number(i, None) => (1, Some(i)),
                    _ => unreachable!(),
                };
                let num = match num {
                    Some(x) => x,
                    None => take!(DateToken::Number(x, None), x),
                };
                let value = i32::from_str_radix(&*num, 10);
                if let Ok(value) = value {
                    out.iso_year = Some(value * sign);
                    Ok(())
                } else {
                    Err(format!("Expected isoyear, got out of range value"))
                }
            }
            DateMatch::Era => match tok {
                Some(DateToken::Literal(ref s))
                    if { s.to_lowercase() == "ad" || s.to_lowercase() == "ce" } =>
                {
                    out.era = Some(Era::CE);
                    Ok(())
                }
                Some(DateToken::Literal(ref s))
                    if { s.to_lowercase() == "bc" || s.to_lowercase() == "bce" } =>
                {
                    out.era = Some(Era::BCE);
                    Ok(())
                }
                x => Err(format!("Expected AD/BC or CE/BCE, got {}", ts(x))),
            },
            DateMatch::Hour12 => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 0, 1..=12) {
                        out.hour12 = Some(value);
                        Ok(())
                    } else {
                        Err(format!("Expected hour12, got out of range value {}", s))
                    }
                }
                x => Err(format!("Expected hour12, got {}", ts(x))),
            },
            DateMatch::Hour24 => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 0, 0..=23) {
                        out.hour = Some(value);
                        Ok(())
                    } else {
                        Err(format!("Expected hour24, got out of range value {}", s))
                    }
                }
                x => Err(format!("Expected hour24, got {}", ts(x))),
            },
            DateMatch::FullHour12 => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 2, 1..=12) {
                        out.hour12 = Some(value);
                        Ok(())
                    } else {
                        Err(format!(
                            "Expected 2-digit hour12, got out of range value {}",
                            s
                        ))
                    }
                }
                x => Err(format!("Expected 2-digit hour12, got {}", ts(x))),
            },
            DateMatch::FullHour24 => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 2, 0..=23) {
                        out.hour = Some(value);
                        Ok(())
                    } else {
                        Err(format!(
                            "Expected 2-digit hour24, got out of range value {}",
                            s
                        ))
                    }
                }
                x => Err(format!("Expected 2-digit hour24, got {}", ts(x))),
            },
            DateMatch::Meridiem => match tok {
                Some(DateToken::Literal(ref s)) if s.to_lowercase() == "am" => {
                    out.meridiem = Some(Meridiem::AM);
                    Ok(())
                }
                Some(DateToken::Literal(ref s)) if s.to_lowercase() == "pm" => {
                    out.meridiem = Some(Meridiem::PM);
                    Ok(())
                }
                x => Err(format!("Expected AM/PM, got {}", ts(x))),
            },
            DateMatch::Sec => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 2, 0..=60) {
                        out.second = Some(value);
                        Ok(())
                    } else {
                        Err(format!("Expected 2-digit sec in range 0..=60, got {}", s))
                    }
                }
                Some(DateToken::Number(ref s, Some(ref f))) if s.len() == 2 => {
                    let secs = u32::from_str_radix(&**s, 10);
                    let nsecs = u32::from_str_radix(&**f, 10);
                    if let (Ok(secs), Ok(nsecs)) = (secs, nsecs) {
                        let nsecs = nsecs * 10u32.pow(9 - f.len() as u32);
                        out.second = Some(secs as i32);
                        out.nanosecond = Some(nsecs as i32);
                        Ok(())
                    } else {
                        Err(format!("Expected 2-digit sec, got {}.{}", s, f))
                    }
                }
                x => Err(format!("Expected 2-digit sec, got {}", ts(x))),
            },
            DateMatch::Offset => {
                advance = false;
                if let Some(DateToken::Literal(ref s)) = date.peek().cloned() {
                    date.next();
                    let s = s
                        .strip_prefix('[')
                        .and_then(|s| s.strip_suffix(']'))
                        .unwrap_or(s);
                    if let Ok(tz) = TimeZone::get(s) {
                        out.time_zone = Some(tz);
                        Ok(())
                    } else {
                        Err(format!("Invalid timezone {}", s))
                    }
                } else {
                    let s = match take!(DateToken::Plus | DateToken::Dash) {
                        DateToken::Plus => 1,
                        DateToken::Dash => -1,
                        _ => unreachable!(),
                    };
                    let h = take!(DateToken::Number(s, None), s);
                    if let Some(hm) = parse_fixed(&h, 4) {
                        let m = hm % 100;
                        let h = hm / 100;
                        let offset = Offset::from_seconds(s * (h * 3600 + m * 60)).unwrap();
                        out.time_zone = Some(TimeZone::fixed(offset));
                        Ok(())
                    } else if let Ok(h) = i32::from_str_radix(&h, 10) {
                        take!(DateToken::Colon);
                        let m = take!(DateToken::Number(s, None), s);
                        if let Some(m) = parse_range(&m, 2, 0..=59) {
                            let offset = Offset::from_seconds(s * (h * 3600 + m * 60)).unwrap();
                            out.time_zone = Some(TimeZone::fixed(offset));
                            Ok(())
                        } else {
                            Err(format!("Expected 2 digits after : in offset, got {}", m))
                        }
                    } else {
                        Err(format!("Expected offset, got {}", h))
                    }
                }
            }
            DateMatch::MonthName => match tok {
                Some(DateToken::Literal(ref s)) => {
                    let res = match &*s.to_lowercase() {
                        "jan" | "january" => 1,
                        "feb" | "february" => 2,
                        "mar" | "march" => 3,
                        "apr" | "april" => 4,
                        "may" => 5,
                        "jun" | "june" => 6,
                        "jul" | "july" => 7,
                        "aug" | "august" => 8,
                        "sep" | "september" => 9,
                        "oct" | "october" => 10,
                        "nov" | "november" => 11,
                        "dec" | "december" => 12,
                        x => return Err(format!("Unknown month name: {}", x)),
                    };
                    out.month = Some(res);
                    Ok(())
                }
                x => Err(format!("Expected month name, got {}", ts(x))),
            },
            DateMatch::WeekDay => match tok {
                Some(DateToken::Literal(ref s)) => {
                    let res = match &*s.to_lowercase() {
                        "mon" | "monday" => Weekday::Monday,
                        "tue" | "tuesday" => Weekday::Tuesday,
                        "wed" | "wednesday" => Weekday::Wednesday,
                        "thu" | "thursday" => Weekday::Thursday,
                        "fri" | "friday" => Weekday::Friday,
                        "sat" | "saturday" => Weekday::Saturday,
                        "sun" | "sunday" => Weekday::Sunday,
                        x => return Err(format!("Unknown weekday: {}", x)),
                    };
                    out.weekday = Some(res);
                    Ok(())
                }
                x => Err(format!("Expected weekday, got {}", ts(x))),
            },
        },
        Some(&DatePattern::Optional(ref pats)) => {
            advance = false;
            let mut iter = date.clone();
            if let Ok(()) = parse_date(out, &mut iter, &pats[..]) {
                *date = iter
            }
            Ok(())
        }
        Some(&DatePattern::Dash) => match tok {
            Some(DateToken::Dash) => Ok(()),
            x => Err(format!("Expected `-`, got {}", ts(x))),
        },
        Some(&DatePattern::Colon) => match tok {
            Some(DateToken::Colon) => Ok(()),
            x => Err(format!("Expected `:`, got {}", ts(x))),
        },
        Some(&DatePattern::Space) => match tok {
            Some(DateToken::Space) => Ok(()),
            x => Err(format!("Expected ` `, got {}", ts(x))),
        },
    };
    if advance {
        date.next();
    }
    res.and_then(|_| parse_date(out, date, &pat[1..]))
}

fn attempt(
    now: &DateTime,
    date: &[DateToken],
    pat: &[DatePattern],
) -> Result<DateTime, (String, usize)> {
    let mut parsed = Fields::default();
    let mut iter = date.iter().cloned().peekable();
    let res = parse_date(&mut parsed, &mut iter, pat);
    let count = iter.count();
    let res = if count > 0 && res.is_ok() {
        Err(format!(
            "Expected eof, got {}",
            date[date.len() - count..]
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("")
        ))
    } else {
        res
    };
    res.map_err(|e| (e, count))?;

    let date = parsed.to_date(now.year());
    let time = parsed.to_time();
    let tz = parsed.time_zone.unwrap_or(now.dt.time_zone().clone());

    let dt = match (date, time) {
        (Some(date), Some(time)) => jiff::civil::DateTime::from_parts(date, time),
        (Some(date), None) => date.into(),
        (None, Some(time)) => jiff::civil::DateTime::from_parts(now.dt.date(), time),
        (None, None) => return Err(("Can't make a valid datetime".to_owned(), count)),
    };

    match dt.to_zoned(tz) {
        Ok(zoned) => Ok(zoned.into()),
        Err(err) => Err((err.to_string(), count)),
    }
}

pub fn try_decode(date: &[DateToken], context: &Context) -> Result<DateTime, String> {
    let mut best = None;
    for pat in &context.registry.datepatterns {
        match attempt(&context.now, date, pat) {
            Ok(datetime) => return Ok(datetime),
            Err((e, c)) => {
                //println!("{}", e);
                let better = if let Some((count, _, _)) = best {
                    c < count
                } else {
                    true
                };
                if better {
                    best = Some((c, pat, e.clone()));
                }
            }
        }
    }
    if let Some((_, pat, err)) = best {
        Err(format!(
            "Most likely pattern `{}` failed: {}",
            DatePattern::show(pat),
            err
        ))
    } else {
        Err("Invalid date literal".to_string())
    }
}

pub fn to_duration(num: &Number) -> Result<SignedDuration, String> {
    if num.unit != Dimensionality::base_unit(BaseUnit::new("s")) {
        return Err("Expected seconds".to_string());
    }
    let max = Numeric::from(i64::max_value() / 1000);
    if num.value.abs() > max {
        return Err(format!(
            "Implementation error: Number is out of range ({:?})",
            max
        ));
    }
    let (seconds, rem) = num.value.div_rem(&Numeric::from(1));
    let nanos = &rem * &Numeric::from(1_000_000_000);
    let seconds = seconds.to_int().unwrap();
    let nanos = nanos.to_int().unwrap();
    Ok(SignedDuration::new(seconds, nanos as i32))
}

pub fn from_duration(duration: &SignedDuration) -> Result<Number, String> {
    let seconds = duration.as_secs();
    let nanos = duration.subsec_nanos();
    let seconds_div = BigInt::one();
    let nanos_div = BigInt::from(1_000_000_000u64);
    let seconds = BigRat::ratio(&BigInt::from(seconds), &seconds_div);
    let nanos = BigRat::ratio(&BigInt::from(nanos), &nanos_div);
    Ok(Number::new_unit(
        Numeric::Rational(&seconds + &nanos),
        BaseUnit::new("s"),
    ))
}

pub fn parse_datepattern<I>(iter: &mut Peekable<I>) -> Result<Vec<DatePattern>, String>
where
    I: Iterator<Item = char>,
{
    let mut out = vec![];
    while iter.peek().is_some() {
        let res = match iter.peek().cloned().unwrap() {
            '-' => DatePattern::Dash,
            ':' => DatePattern::Colon,
            '[' => {
                iter.next();
                let res = DatePattern::Optional(parse_datepattern(iter)?);
                if iter.peek().cloned() != Some(']') {
                    return Err("Expected ]".to_string());
                } else {
                    res
                }
            }
            ']' => break,
            '\'' => {
                iter.next();
                let mut buf = String::new();
                while let Some(c) = iter.peek().cloned() {
                    if c == '\'' {
                        break;
                    } else {
                        iter.next();
                        buf.push(c);
                    }
                }
                DatePattern::Literal(buf)
            }
            x if x.is_whitespace() => {
                while iter.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                    iter.next();
                }
                out.push(DatePattern::Space);
                continue;
            }
            x if x.is_alphabetic() => {
                let mut buf = String::new();
                while let Some(c) = iter.peek().cloned() {
                    if c.is_alphanumeric() {
                        iter.next();
                        buf.push(c);
                    } else {
                        break;
                    }
                }
                match DateMatch::from_str(&buf) {
                    Some(dm) => out.push(DatePattern::Match(dm)),
                    None => return Err(format!("Unknown date pattern `{}`", buf)),
                }
                continue;
            }
            x => return Err(format!("Unrecognized character {}", x)),
        };
        out.push(res);
        iter.next();
    }
    Ok(out)
}

pub fn parse_datefile(file: &str) -> Vec<Vec<DatePattern>> {
    let mut defs = vec![];
    for (num, line) in file.lines().enumerate() {
        let line = line.split('#').next().unwrap();
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let res = parse_datepattern(&mut line.chars().peekable());
        match res {
            Ok(res) => defs.push(res),
            Err(e) => println!("Line {}: {}: {}", num, e, line),
        }
    }
    defs
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pattern(s: &str) -> Vec<DatePattern> {
        parse_datepattern(&mut s.chars().peekable()).unwrap()
    }

    fn parse_with_tz(date: Vec<DateToken>, pat: &str) -> (Result<(), String>, Fields) {
        let mut parsed = Fields::default();
        let pat = pattern(pat);
        let res = parse_date(&mut parsed, &mut date.into_iter().peekable(), &pat);

        (res, parsed)
    }

    fn parse(date: Vec<DateToken>, pat: &str) -> (Result<(), String>, Fields) {
        let (res, parsed) = parse_with_tz(date, pat);
        (res, parsed)
    }

    #[test]
    fn test_literal() {
        let date = vec![DateToken::Literal("abc".into())];
        let (res, _parsed) = parse(date.clone(), "'abc'");
        assert!(res.is_ok());

        let (res, _parsed) = parse(date, "'def'");
        assert_eq!(res, Err("Expected `def`, got `abc`".into()));
    }

    #[test]
    fn test_year_plus() {
        let date = vec![DateToken::Plus, DateToken::Number("123".into(), None)];
        let (res, parsed) = parse(date.clone(), "isoyear");
        assert!(res.is_ok());
        assert_eq!(parsed.iso_year, Some(123));

        let date = vec![DateToken::Number("123".into(), None)];
        let (res, parsed2) = parse(date.clone(), "year");
        assert!(res.is_ok());
        assert_eq!(parsed2.year, Some(123));
    }

    #[test]
    fn test_complicated_date_input() {
        let date = vec![
            DateToken::Number("2".to_owned(), None),
            DateToken::Space,
            DateToken::Literal("Pm".into()),
            DateToken::Dash,
            DateToken::Number("05".to_owned(), None),
            DateToken::Colon,
            DateToken::Number("123".to_owned(), None),
            DateToken::Space,
            DateToken::Number("01".to_owned(), None),
            DateToken::Dash,
            DateToken::Number("57".to_owned(), None),
            DateToken::Space,
            DateToken::Literal("May".into()),
        ];

        let (res, parsed) = parse(date, "day meridiem-monthnum:year hour12-min monthname");
        assert_eq!(res, Ok(()));
        assert_eq!(parsed.year, Some(123));
        assert_eq!(parsed.month, Some(5));
        assert_eq!(parsed.day, Some(2));
        assert_eq!(parsed.hour12, Some(1));
        assert_eq!(parsed.meridiem, Some(Meridiem::PM));
        assert_eq!(parsed.get_hour(), Some(13));
        assert_eq!(parsed.minute, Some(57));
    }

    #[test]
    fn ad_bc() {
        let date = vec![
            DateToken::Number("100".to_owned(), None),
            DateToken::Space,
            DateToken::Literal("bce".into()),
            DateToken::Space,
            DateToken::Number("07".to_owned(), None),
            DateToken::Space,
            DateToken::Literal("am".into()),
        ];

        let (res, parsed) = parse(date, "year adbc hour12 meridiem");
        assert!(res.is_ok(), "{}", res.unwrap_err());
        assert_eq!(parsed.year, Some(100));
        assert_eq!(parsed.era, Some(Era::BCE));
        assert_eq!(parsed.get_year(), Some(-101));
        assert_eq!(parsed.hour12, Some(7));
        assert_eq!(parsed.meridiem, Some(Meridiem::AM));
    }

    #[test]
    fn ad_bc_wrong() {
        for date in vec![
            vec![DateToken::Literal("foo".into())],
            vec![DateToken::Plus],
        ] {
            let (res, _) = parse(date, "adbc");
            assert!(res.is_err());
        }
    }

    #[test]
    fn short_hour() {
        let date = vec![DateToken::Number("7".into(), None)];
        let (res, _) = parse(date, "hour24");
        assert_eq!(res, Ok(()));
    }

    #[test]
    fn wrong_length_24h() {
        let date = vec![DateToken::Number("7".into(), None)];
        let (res, _) = parse(date, "fullhour24");
        assert_eq!(
            res,
            Err(format!("Expected 2-digit hour24, got out of range value 7"))
        );
    }

    #[test]
    fn test_24h() {
        let (res, parsed) = parse(vec![DateToken::Number("23".into(), None)], "hour24");
        assert!(res.is_ok());
        assert_eq!(parsed.hour, Some(23));
    }

    #[test]
    fn seconds() {
        let date = vec![DateToken::Number("27".into(), Some("000012345".into()))];
        let (res, parsed) = parse(date, "sec");
        assert!(res.is_ok());
        assert_eq!(parsed.second, Some(27));
        assert_eq!(parsed.nanosecond, Some(12345));

        let date = vec![DateToken::Number("27".into(), None)];
        let (res, parsed) = parse(date, "sec");
        assert!(res.is_ok());
        assert_eq!(parsed.second, Some(27));
        assert_eq!(parsed.nanosecond, None);
    }

    #[test]
    fn test_offset() {
        let date = vec![DateToken::Plus, DateToken::Number("0200".into(), None)];
        let (res, parsed) = parse(date, "offset");
        assert!(res.is_ok());
        assert_eq!(
            parsed.time_zone,
            Some(TimeZone::fixed(Offset::from_seconds(2 * 3600).unwrap()))
        );

        let date = vec![
            DateToken::Dash,
            DateToken::Number("01".into(), None),
            DateToken::Colon,
            DateToken::Number("23".into(), None),
        ];
        let (res, parsed) = parse(date, "offset");
        assert!(res.is_ok());
        assert_eq!(
            parsed.time_zone,
            Some(TimeZone::fixed(
                Offset::from_seconds(-(1 * 60 + 23) * 60).unwrap()
            ))
        );

        let date = vec![DateToken::Literal("Europe/London".into())];
        let (res, parsed) = parse_with_tz(date, "offset");
        assert!(res.is_ok(), "{}", res.unwrap_err());
        assert_eq!(
            parsed.time_zone,
            Some(TimeZone::get("Europe/London").unwrap())
        );
    }

    #[test]
    fn test_weekday() {
        let date = vec![DateToken::Literal("saturday".into())];
        let (res, parsed) = parse(date, "weekday");
        assert!(res.is_ok());
        assert_eq!(parsed.weekday, Some(Weekday::Saturday));

        let date = vec![DateToken::Literal("sun".into())];
        assert!(parse(date, "weekday").0.is_ok());

        let date = vec![DateToken::Literal("snu".into())];
        assert_eq!(parse(date, "weekday").0, Err("Unknown weekday: snu".into()));
    }

    #[test]
    fn test_monthname() {
        for (i, &s) in [
            "jan", "feb", "mar", "apr", "may", "june", "jul", "AUGUST", "SEp", "Oct", "novemBer",
            "dec",
        ]
        .iter()
        .enumerate()
        {
            let date = vec![DateToken::Literal(s.into())];
            let (res, parsed) = parse(date, "monthname");
            assert!(res.is_ok());
            assert_eq!(parsed.month, Some(i as i32 + 1));
        }

        let date = vec![DateToken::Literal("foobar".into())];
        let (res, parsed) = parse(date, "monthname");
        assert_eq!(res, Err("Unknown month name: foobar".into()));
        assert_eq!(parsed.month, None);
    }

    #[test]
    fn test_parse_datepattern() {
        use self::DatePattern::*;

        fn parse(s: &str) -> Result<Vec<DatePattern>, String> {
            parse_datepattern(&mut s.chars().peekable())
        }

        assert_eq!(
            parse("-:['abc']"),
            Ok(vec![Dash, Colon, Optional(vec![Literal("abc".into())])])
        );
        assert!(parse("-:['abc'").is_err());
        assert!(parse("*").is_err());
    }

    #[test]
    fn test_attempt() {
        use self::DateToken::*;
        fn n(x: &str) -> DateToken {
            Number(x.into(), None)
        }

        let now = DateTime::default();

        macro_rules! check_attempt {
            ($date:expr, $pat:expr) => {{
                let pat = parse_datepattern(&mut $pat.chars().peekable()).unwrap();
                attempt(&now, $date, pat.as_ref())
            }};
        }

        let tz = Literal("Europe/London".into());

        let date = &[n("23"), Space, n("05"), Space, tz.clone()];
        let res = check_attempt!(date, "hour24 min offset");
        assert!(res.is_ok(), "{:?}", res);

        let date = &[n("23"), Space, tz.clone()];
        let res = check_attempt!(date, "hour24 offset");
        let res = res.expect("should have parsed");
        assert_eq!(res.hour(), 23);
        assert_eq!(res.dt.time_zone(), &TimeZone::get("Europe/London").unwrap());

        let date = &[n("2018"), Space, n("01"), Space, n("01"), Space, tz.clone()];
        let res = check_attempt!(date, "year monthnum day offset");
        assert!(res.is_ok(), "{:?}", res);
    }
}
