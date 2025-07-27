// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use jiff::civil::Weekday;
use jiff::fmt::strtime::BrokenDownTime;
use jiff::tz::Offset;
use jiff::SignedDuration;

use crate::ast::{DatePattern, DateToken};
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

pub fn parse_date<I>(
    out: &mut BrokenDownTime,
    out_tz: &mut Option<TimeZone>,
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
        Some(&DatePattern::Match(ref what)) => match &**what {
            "fullyear" => numeric_match(tok.as_ref(), "fullyear", 4, 0..=9999).and_then(|v| {
                out.set_year(Some(v as i16)).unwrap();
                Ok(())
            }),
            "shortyear" => numeric_match(tok.as_ref(), "shortyear", 2, 0..=99).and_then(|v| {
                let year = out.year().unwrap_or(1900);
                out.set_year(Some(year / 100 * 100 + v as i16)).unwrap();
                Ok(())
            }),
            "century" => numeric_match(tok.as_ref(), "century", 2, 0..=99).and_then(|v| {
                let short_year = out.year().unwrap_or(0) % 100;
                out.set_year(Some(v as i16 * 100 + short_year)).unwrap();
                Ok(())
            }),
            "monthnum" => numeric_match(tok.as_ref(), "monthnum", 2, 1..=12).and_then(|v| {
                out.set_month(Some(v as i8)).unwrap();
                Ok(())
            }),
            "day" => numeric_match(tok.as_ref(), "day", 0, 1..=31).and_then(|v| {
                out.set_day(Some(v as i8)).unwrap();
                Ok(())
            }),
            "fullday" => numeric_match(tok.as_ref(), "fullday", 2, 1..=31).and_then(|v| {
                out.set_day(Some(v as i8)).unwrap();
                Ok(())
            }),
            "min" => numeric_match(tok.as_ref(), "min", 2, 0..=60).and_then(|v| {
                out.set_minute(Some(v as i8)).unwrap();
                Ok(())
            }),
            "ordinal" => numeric_match(tok.as_ref(), "ordinal", 3, 1..=366).and_then(|v| {
                out.set_day_of_year(Some(v as i16)).unwrap();
                Ok(())
            }),
            "isoyear" => numeric_match(tok.as_ref(), "isoyear", 4, 0..=9999).and_then(|v| {
                out.set_year(Some(v as i16)).unwrap();
                Ok(())
            }),
            "isoweek" => numeric_match(tok.as_ref(), "isoweek", 2, 1..=53).and_then(|v| {
                out.set_iso_week(Some(v as i8)).unwrap();
                Ok(())
            }),
            "unix" => numeric_match(tok.as_ref(), "unix", 0, 0..=i32::MAX).and_then(|_v| {
                todo!()
                //out.timestamp = Some(v as i64);
                //Ok(())
            }),
            "year" => {
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
                    out.set_year(Some((value * sign) as i16)).unwrap();
                    Ok(())
                } else {
                    Err(format!("Expected year, got out of range value"))
                }
            }
            "adbc" => match tok {
                Some(DateToken::Literal(ref s))
                    if { s.to_lowercase() == "ad" || s.to_lowercase() == "ce" } =>
                {
                    Ok(())
                }
                Some(DateToken::Literal(ref s))
                    if { s.to_lowercase() == "bc" || s.to_lowercase() == "bce" } =>
                {
                    out.set_year(out.year().map(|x| -x + 1)).unwrap();
                    Ok(())
                }
                x => Err(format!("Expected AD/BC or CE/BCE, got {}", ts(x))),
            },
            "hour12" => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 2, 1..=12) {
                        let hour = out.hour().unwrap_or(0) / 12 * 12;
                        out.set_hour(Some(hour + value as i8)).unwrap();
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
            "hour24" => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 2, 0..=23) {
                        out.set_hour(Some(value as i8)).unwrap();
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
            "meridiem" => match tok {
                Some(DateToken::Literal(ref s)) if s.to_lowercase() == "am" => Ok(()),
                Some(DateToken::Literal(ref s)) if s.to_lowercase() == "pm" => {
                    out.set_hour(out.hour().map(|x| x + 12)).unwrap();
                    Ok(())
                }
                x => Err(format!("Expected AM/PM, got {}", ts(x))),
            },
            "sec" => match tok {
                Some(DateToken::Number(ref s, None)) => {
                    if let Some(value) = parse_range(s, 2, 0..=60) {
                        out.set_second(Some(value as i8)).unwrap();
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
                        out.set_second(Some(secs as i8)).unwrap();
                        out.set_subsec_nanosecond(Some(nsecs as i32)).unwrap();
                        Ok(())
                    } else {
                        Err(format!("Expected 2-digit sec, got {}.{}", s, f))
                    }
                }
                x => Err(format!("Expected 2-digit sec, got {}", ts(x))),
            },
            "offset" => {
                advance = false;
                if let Some(DateToken::Literal(ref s)) = date.peek().cloned() {
                    date.next();
                    if let Ok(tz) = TimeZone::get(s) {
                        *out_tz = Some(tz);
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
                        out.set_offset(Some(offset));
                        Ok(())
                    } else if let Ok(h) = i32::from_str_radix(&h, 10) {
                        take!(DateToken::Colon);
                        let m = take!(DateToken::Number(s, None), s);
                        if let Some(m) = parse_range(&m, 2, 0..=59) {
                            let offset = Offset::from_seconds(s * (h * 3600 + m * 60)).unwrap();
                            out.set_offset(Some(offset));
                            Ok(())
                        } else {
                            Err(format!("Expected 2 digits after : in offset, got {}", m))
                        }
                    } else {
                        Err(format!("Expected offset, got {}", h))
                    }
                }
            }
            "monthname" => match tok {
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
                    out.set_month(Some(res)).unwrap();
                    Ok(())
                }
                x => Err(format!("Expected month name, got {}", ts(x))),
            },
            "weekday" => match tok {
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
                    out.set_weekday(Some(res));
                    Ok(())
                }
                x => Err(format!("Expected weekday, got {}", ts(x))),
            },
            x => Err(format!("Unknown match pattern `{}`", x)),
        },
        Some(&DatePattern::Optional(ref pats)) => {
            advance = false;
            let mut iter = date.clone();
            if let Ok(()) = parse_date(out, out_tz, &mut iter, &pats[..]) {
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
    res.and_then(|_| parse_date(out, out_tz, date, &pat[1..]))
}

fn attempt(
    now: &DateTime,
    date: &[DateToken],
    pat: &[DatePattern],
) -> Result<DateTime, (String, usize)> {
    let mut parsed = BrokenDownTime::default();
    let mut tz = None;
    let mut iter = date.iter().cloned().peekable();
    let res = parse_date(&mut parsed, &mut tz, &mut iter, pat);
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

    let year = parsed.year().unwrap_or(now.year() as i16);
    parsed.set_year(Some(year)).unwrap();

    if parsed.offset().is_none() && parsed.iana_time_zone().is_none() {
        let name = now
            .dt
            .time_zone()
            .iana_name()
            .expect("system timezone has no iana tzdb name");
        parsed.set_iana_time_zone(Some(name.to_owned()));
    }

    if parsed.to_date().is_err() {
        let month = parsed.month().unwrap_or(now.month() as i8);
        parsed.set_month(Some(month)).unwrap();
        let day = parsed.day().unwrap_or(now.day() as i8);
        parsed.set_day(Some(day)).unwrap();
    }

    if parsed.to_time().is_err() {
        parsed.set_hour(Some(parsed.hour().unwrap_or(0))).unwrap();
        parsed
            .set_minute(Some(parsed.minute().unwrap_or(0)))
            .unwrap();
        parsed
            .set_second(Some(parsed.second().unwrap_or(0)))
            .unwrap();
    }

    if let Ok(value) = parsed.to_zoned() {
        return Ok(value.into());
    }

    match parsed.to_zoned() {
        Ok(value) => Ok(value.into()),
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
                out.push(DatePattern::Match(buf));
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

    fn parse_with_tz(
        date: Vec<DateToken>,
        pat: &str,
    ) -> (Result<(), String>, BrokenDownTime, Option<TimeZone>) {
        let mut parsed = BrokenDownTime::default();
        let mut tz = None;
        let pat = pattern(pat);
        let res = parse_date(&mut parsed, &mut tz, &mut date.into_iter().peekable(), &pat);

        (res, parsed, tz)
    }

    fn parse(date: Vec<DateToken>, pat: &str) -> (Result<(), String>, BrokenDownTime) {
        let (res, parsed, _) = parse_with_tz(date, pat);
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
        let mut expected = BrokenDownTime::default();
        expected.set_year(Some(123)).unwrap();

        let date = vec![
            DateToken::Plus,
            DateToken::Number(expected.year().unwrap().to_string(), None),
        ];
        let (res, parsed) = parse(date.clone(), "year");
        assert!(res.is_ok());
        assert_eq!(parsed.year(), Some(123));

        let date = vec![DateToken::Number(
            expected.year().unwrap().to_string(),
            None,
        )];
        let (res, parsed2) = parse(date.clone(), "year");
        assert!(res.is_ok());
        assert_eq!(parsed2.year(), Some(123));
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
        assert_eq!(parsed.year(), Some(123));
        assert_eq!(parsed.month(), Some(5));
        assert_eq!(parsed.day(), Some(2));
        assert_eq!(parsed.hour(), Some(13));
        assert_eq!(parsed.minute(), Some(57));
    }

    #[test]
    fn ad_bc() {
        let year = -100i16;
        let date = vec![
            DateToken::Number("101".to_owned(), None),
            DateToken::Space,
            DateToken::Literal("bce".into()),
            DateToken::Space,
            DateToken::Number("07".to_owned(), None),
            DateToken::Space,
            DateToken::Literal("am".into()),
        ];

        let (res, parsed) = parse(date, "year adbc hour12 meridiem");
        assert!(res.is_ok(), "{}", res.unwrap_err());
        assert_eq!(parsed.year(), Some(year));
        assert_eq!(parsed.hour(), Some(7));
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
    fn wrong_length_24h() {
        let date = vec![DateToken::Number("7".into(), None)];
        let (res, _) = parse(date, "hour24");
        assert_eq!(
            res,
            Err(format!("Expected 2-digit hour24, got out of range value 7"))
        );
    }

    #[test]
    fn test_24h() {
        let (res, parsed) = parse(vec![DateToken::Number("23".into(), None)], "hour24");
        assert!(res.is_ok());
        assert_eq!(parsed.hour(), Some(23));
    }

    #[test]
    fn seconds() {
        let date = vec![DateToken::Number("27".into(), Some("000012345".into()))];
        let (res, parsed) = parse(date, "sec");
        assert!(res.is_ok());
        assert_eq!(parsed.second(), Some(27));
        assert_eq!(parsed.subsec_nanosecond(), Some(12345));

        let date = vec![DateToken::Number("27".into(), None)];
        let (res, parsed) = parse(date, "sec");
        assert!(res.is_ok());
        assert_eq!(parsed.second(), Some(27));
        assert_eq!(parsed.subsec_nanosecond(), None);
    }

    #[test]
    fn test_offset() {
        let date = vec![DateToken::Plus, DateToken::Number("0200".into(), None)];
        let (res, parsed) = parse(date, "offset");
        assert!(res.is_ok());
        assert_eq!(
            parsed.offset(),
            Some(Offset::from_seconds(2 * 3600).unwrap())
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
            parsed.offset(),
            Some(Offset::from_seconds(-(1 * 60 + 23) * 60).unwrap())
        );

        let date = vec![DateToken::Literal("Europe/London".into())];
        let (res, parsed, tz) = parse_with_tz(date, "offset");
        assert!(res.is_ok(), "{}", res.unwrap_err());
        assert_eq!(tz, TimeZone::get("Europe/London").ok());
        assert_eq!(parsed.offset(), None);
    }

    #[test]
    fn test_weekday() {
        let date = vec![DateToken::Literal("saturday".into())];
        let (res, parsed) = parse(date, "weekday");
        assert!(res.is_ok());
        assert_eq!(parsed.weekday(), Some(Weekday::Saturday));

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
            assert_eq!(parsed.month(), Some(i as i8 + 1));
        }

        let date = vec![DateToken::Literal("foobar".into())];
        let (res, parsed) = parse(date, "monthname");
        assert_eq!(res, Err("Unknown month name: foobar".into()));
        assert_eq!(parsed.month(), None);
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
        assert_eq!(
            res,
            Err(("Failed to construct a useful datetime".into(), 0))
        );

        let date = &[n("2018"), Space, n("01"), Space, n("01"), Space, tz.clone()];
        let res = check_attempt!(date, "year monthnum day offset");
        assert!(res.is_ok(), "{:?}", res);
    }
}
