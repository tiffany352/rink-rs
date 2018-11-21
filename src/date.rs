// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ast::{DatePattern, DateToken, show_datepattern};
use chrono::format::Parsed;
use chrono::{Weekday, DateTime, UTC, FixedOffset, Duration, Date, TimeZone};
use context::Context;
use number::{Number, Dim};
use num::Num;
use std::iter::Peekable;
use chrono_tz::Tz;
use std::str::FromStr;

pub fn parse_date<I>(
    out: &mut Parsed,
    out_tz: &mut Option<Tz>,
    date: &mut Peekable<I>,
    pat: &[DatePattern]
) -> Result<(), String>
    where I: Iterator<Item=DateToken>+Clone {
    use std::borrow::Borrow;

    let tok = date.peek().cloned();

    fn ts<T>(x: Option<T>) -> String where T:Borrow<DateToken> {
        match x {
            Some(ref x) => format!("`{}`", x.borrow()),
            None => "eof".to_owned()
        }
    }

    macro_rules! numeric_match {
        ($name:expr, $digits:expr, $field:ident) => {
            match tok {
                Some(DateToken::Number(ref s, None)) if $digits == 0 || s.len() == $digits => {
                    let value = i32::from_str_radix(&**s, 10).unwrap();
                    out.$field = Some(value as _);
                    Ok(())
                },
                Some(DateToken::Number(ref s, None)) => Err(format!(
                    "Expected {}-digit {}, got {} digits", $digits, $name, s.len())),
                x => Err(format!(
                    "Expected {}-digit {}, got {}",
                    $digits, $name, ts(x)))
            }
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
            "fullyear"  => numeric_match!("fullyear",  4, year),
            "shortyear" => numeric_match!("shortyear", 2, year_mod_100),
            "century"   => numeric_match!("century",   2, year_div_100),
            "monthnum"  => numeric_match!("monthnum",  2, month),
            "day"       => numeric_match!("day",       0, day),
            "fullday"   => numeric_match!("fullday",   2, day),
            "min"       => numeric_match!("min",       2, minute),
            "ordinal"   => numeric_match!("ordinal",   3, ordinal),
            "isoyear"   => numeric_match!("isoyear",   4, isoyear),
            "isoweek"   => numeric_match!("isoweek",   2, isoweek),
            "unix"      => numeric_match!("unix",      0, timestamp),
            "year" => {
                advance = false;
                let x = take!(DateToken::Dash | DateToken::Plus | DateToken::Number(_, None));
                let (sign, num) = match x {
                    DateToken::Dash => (-1, None),
                    DateToken::Plus => (1, None),
                    DateToken::Number(i, None) => (1, Some(i)),
                    _ => panic!()
                };
                let num = match num {
                    Some(x) => x,
                    None => take!(DateToken::Number(x, None), x)
                };
                let value = i32::from_str_radix(&*num, 10).unwrap();
                out.year = Some(value * sign);
                Ok(())
            },
            "adbc" => match tok {
                Some(DateToken::Literal(ref s)) if {
                    s.to_lowercase() == "ad" || s.to_lowercase() == "ce"
                } => {
                    Ok(())
                },
                Some(DateToken::Literal(ref s)) if {
                    s.to_lowercase() == "bc" || s.to_lowercase() == "bce"
                } => {
                    out.year = out.year.map(|x| -x + 1);
                    Ok(())
                },
                x => Err(format!("Expected AD/BC or CE/BCE, got {}", ts(x)))
            },
            "hour12" => match tok {
                Some(DateToken::Number(ref s, None)) if s.len() == 2 => {
                    let value = u32::from_str_radix(&**s, 10).unwrap();
                    out.hour_mod_12 = Some(value % 12);
                    Ok(())
                }
                x => Err(format!("Expected 2-digit hour12, got {}", ts(x))),
            },
            "hour24" => match tok {
                Some(DateToken::Number(ref s, None)) if s.len() == 2 => {
                    let value = u32::from_str_radix(&**s, 10).unwrap();
                    out.hour_div_12 = Some(value / 12);
                    out.hour_mod_12 = Some(value % 12);
                    Ok(())
                },
                x => Err(format!("Expected 2-digit hour24, got {}", ts(x)))
            },
            "meridiem" => match tok {
                Some(DateToken::Literal(ref s)) if s.to_lowercase() == "am" => {
                    out.hour_div_12 = Some(0);
                    Ok(())
                },
                Some(DateToken::Literal(ref s)) if s.to_lowercase() == "pm" => {
                    out.hour_div_12 = Some(1);
                    Ok(())
                },
                x => Err(format!("Expected AM/PM, got {}", ts(x)))
            },
            "sec" => match tok {
                Some(DateToken::Number(ref s, None)) if s.len() == 2 => {
                    let value = u32::from_str_radix(&**s, 10).unwrap();
                    out.second = Some(value);
                    Ok(())
                },
                Some(DateToken::Number(ref s, Some(ref f))) if s.len() == 2 => {
                    let secs = u32::from_str_radix(&**s, 10).unwrap();
                    let nsecs = u32::from_str_radix(&**f, 10).unwrap() * 10u32.pow(9 - f.len() as u32);
                    out.second = Some(secs);
                    out.nanosecond = Some(nsecs);
                    Ok(())
                },
                x => Err(format!("Expected 2-digit sec, got {}", ts(x)))
            },
            "offset" => {
                advance = false;
                if let Some(DateToken::Literal(ref s)) = date.peek().cloned() {
                    date.next();
                    if let Ok(tz) = Tz::from_str(s) {
                        *out_tz = Some(tz);
                        Ok(())
                    } else {
                        Err(format!("Invalid timezone {}", s))
                    }
                } else {
                    let s = match take!(DateToken::Plus | DateToken::Dash) {
                        DateToken::Plus => 1, DateToken::Dash => -1, _ => panic!()
                    };
                    let h = take!(DateToken::Number(s, None), s);
                    if h.len() == 4 {
                        let h = i32::from_str_radix(&*h, 10).unwrap();
                        let m = h % 100;
                        let h = h / 100;
                        out.offset = Some(s * (h*3600 + m*60));
                    } else {
                        let h = i32::from_str_radix(&*h, 10).unwrap();
                        take!(DateToken::Colon);
                        let m = take!(DateToken::Number(s, None), s);
                        let m = i32::from_str_radix(&*m, 10).unwrap();
                        out.offset = Some(s * (h*3600 + m*60));
                    }
                    Ok(())
                }
            },
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
                        x => return Err(format!("Unknown month name: {}", x))
                    };
                    out.month = Some(res);
                    Ok(())
                },
                x => Err(format!("Expected month name, got {}", ts(x)))
            },
            "weekday" => match tok {
                Some(DateToken::Literal(ref s)) => {
                    let res = match &*s.to_lowercase() {
                        "mon" | "monday" => Weekday::Mon,
                        "tue" | "tuesday" => Weekday::Tue,
                        "wed" | "wednesday" => Weekday::Wed,
                        "thu" | "thursday" => Weekday::Thu,
                        "fri" | "friday" => Weekday::Fri,
                        "sat" | "saturday" => Weekday::Sat,
                        "sun" | "sunday" => Weekday::Sun,
                        x => return Err(format!("Unknown weekday: {}", x))
                    };
                    out.weekday = Some(res);
                    Ok(())
                },
                x => Err(format!("Expected weekday, got {}", ts(x)))
            },
            x => Err(format!("Unknown match pattern `{}`", x))
        },
        Some(&DatePattern::Optional(ref pats)) => {
            advance = false;
            let mut iter = date.clone();
            match parse_date(out, out_tz, &mut iter, &pats[..]) {
                Ok(()) => *date = iter,
                Err(_) => ()
            };
            Ok(())
        }
        Some(&DatePattern::Dash) => match tok {
            Some(DateToken::Dash) => Ok(()),
            x => Err(format!("Expected `-`, got {}", ts(x)))
        },
        Some(&DatePattern::Colon) => match tok {
            Some(DateToken::Colon) => Ok(()),
            x => Err(format!("Expected `:`, got {}", ts(x)))
        },
        Some(&DatePattern::Space) => match tok {
            Some(DateToken::Space) => Ok(()),
            x => Err(format!("Expected ` `, got {}", ts(x)))
        },
    };
    if advance {
        date.next();
    }
    match res {
        Ok(()) => parse_date(out, out_tz, date, &pat[1..]),
        Err(e) => Err(e)
    }
}

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

fn attempt(date: &[DateToken], pat: &[DatePattern]) -> Result<GenericDateTime, (String, usize)> {
    let mut parsed = Parsed::new();
    let mut tz = None;
    let mut iter = date.iter().cloned().peekable();
    let res = parse_date(&mut parsed, &mut tz, &mut iter, pat);
    let count = iter.count();
    let res = if count > 0 && res.is_ok() {
        Err(format!("Expected eof, got {}",
                    date[date.len()-count..].iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>().join("")))
    } else {
        res
    };
    try!(res.map_err(|e| (e, count)));
    let time = parsed.to_naive_time();
    let date = parsed.to_naive_date();
    if let Some(tz) = tz {
        match (time, date) {
            (Ok(time), Ok(date)) =>
                tz.from_local_datetime(&date.and_time(time)).earliest().ok_or_else(|| (
                    "Datetime does not represent a valid moment in time".to_string(),
                    count,
                )).map(GenericDateTime::Timezone),
            (Ok(time), Err(_)) =>
                Ok(UTC::now().with_timezone(&tz).date().and_time(time).unwrap()).map(
                    GenericDateTime::Timezone),
            (Err(_), Ok(date)) =>
                tz.from_local_date(&date).earliest().map(|x| x.and_hms(0, 0, 0)).ok_or_else(|| (
                    "Datetime does not represent a valid moment in time".to_string(),
                    count,
                )).map(GenericDateTime::Timezone),
            _ => Err(("Failed to construct a useful datetime".to_string(), count))
        }
    } else {
        let offset = parsed.to_fixed_offset().unwrap_or(FixedOffset::east(0));
        match (time, date) {
            (Ok(time), Ok(date)) =>
                Ok(GenericDateTime::Fixed(DateTime::<FixedOffset>::from_utc(
                    date.and_time(time), offset
                ))),
            (Ok(time), Err(_)) =>
                Ok(GenericDateTime::Fixed(UTC::now().with_timezone(
                    &offset
                ).date().and_time(time).unwrap())),
            (Err(_), Ok(date)) =>
                Ok(GenericDateTime::Fixed(Date::<FixedOffset>::from_utc(
                    date, offset
                ).and_hms(0, 0, 0))),
            _ => Err(("Failed to construct a useful datetime".to_string(), count))
        }
    }
}

pub fn try_decode(date: &[DateToken], context: &Context) -> Result<GenericDateTime, String> {
    let mut best = None;
    for pat in &context.datepatterns {
        //println!("Tring {:?} against {}", date, show_datepattern(pat));
        match attempt(date, pat) {
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
            },
        }
    }
    if let Some((_, pat, err)) = best {
        Err(format!("Most likely pattern `{}` failed: {}", show_datepattern(pat), err))
    } else {
        Err("Invalid date literal".to_string())
    }
}

pub fn to_duration(num: &Number) -> Result<Duration, String> {
    if num.unit.len() != 1 || num.unit.get("s") != Some(&1) {
        return Err("Expected seconds".to_string())
    }
    let max = Num::from(i64::max_value() / 1000);
    if num.value.abs() > max {
        return Err(format!("Implementation error: Number is out of range ({:?})", max))
    }
    let ms = &num.value * &Num::from(1000);
    let (ms, rem) = ms.div_rem(&Num::from(1));
    let ns = &rem * &Num::from(1_000_000_000);
    Ok(Duration::milliseconds(ms.to_int().unwrap()) +
       Duration::nanoseconds(ns.to_int().unwrap()))
}

pub fn from_duration(duration: &Duration) -> Result<Number, String> {
    use gmp::mpq::Mpq;
    use gmp::mpz::Mpz;

    let ms = duration.num_milliseconds();
    let ns = (*duration - Duration::milliseconds(ms)).num_nanoseconds().unwrap();
    let ms_div = Mpz::from(1_000);
    let ns_div = Mpz::from(1_000_000_000);
    let ms = Mpq::ratio(&Mpz::from(ms), &ms_div);
    let ns = Mpq::ratio(&Mpz::from(ns), &ns_div);
    Ok(Number::new_unit(Num::Mpq(&ms + &ns), Dim::new("s")))
}

pub fn now() -> DateTime<FixedOffset> {
    UTC::now().with_timezone(&FixedOffset::east(0))
}

pub fn parse_datepattern<I>(iter: &mut Peekable<I>)
                            -> Result<Vec<DatePattern>, String> where I: Iterator<Item=char> {
    let mut out = vec![];
    while iter.peek().is_some() {
        let res = match iter.peek().cloned().unwrap() {
            '-' => DatePattern::Dash,
            ':' => DatePattern::Colon,
            '[' => {
                iter.next();
                let res = DatePattern::Optional(try!(parse_datepattern(iter)));
                if iter.peek().cloned() != Some(']') {
                    return Err("Expected ]".to_string())
                } else {
                    res
                }
            },
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
            },
            x if x.is_whitespace() => {
                while iter.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                    iter.next();
                }
                out.push(DatePattern::Space);
                continue
            },
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
                continue
            },
            x => return Err(format!("Unrecognized character {}", x))
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
            continue
        }
        let res = parse_datepattern(&mut line.chars().peekable());
        match res {
            Ok(res) => defs.push(res),
            Err(e) => println!("Line {}: {}: {}", num, e, line),
        }
    }
    defs
}

impl Context {
    #[cfg(feature = "chrono-humanize")]
    pub fn humanize<Tz: TimeZone>(&self, date: DateTime<Tz>) -> Option<String> {
        if self.use_humanize {
            use chrono_humanize::HumanTime;
            Some(HumanTime::from(date).to_string())
        } else {
            None
        }
    }

    #[cfg(not(feature = "chrono-humanize"))]
    pub fn humanize<Tz: TimeZone>(&self, _date: DateTime<Tz>) -> Option<String> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pattern(s: &str) -> Vec<DatePattern> {
        parse_datepattern(&mut s.chars().peekable()).unwrap()
    }

    fn parse_with_tz(date: Vec<DateToken>, pat: &str) -> (Result<(), String>, Parsed, Option<Tz>) {
        let mut parsed = Parsed::new();
        let mut tz = None;
        let pat = pattern(pat);
        let res = parse_date(&mut parsed, &mut tz, &mut date.into_iter().peekable(), &pat);

        (res, parsed, tz)
    }

    fn parse(date: Vec<DateToken>, pat: &str) -> (Result<(), String>, Parsed) {
        let (res, parsed, _) = parse_with_tz(date, pat);
        (res, parsed)
    }

    #[test]
    fn test_literal() {
        let date = vec![DateToken::Literal("abc".into())];
        let (res, parsed) = parse(date.clone(), "'abc'");
        assert_eq!(parsed, Parsed::new());
        assert!(res.is_ok());

        let (res, parsed) = parse(date, "'def'");
        assert_eq!(parsed, Parsed::new());
        assert_eq!(res, Err("Expected `def`, got `abc`".into()));
    }

    #[test]
    fn test_year_plus() {
        let mut expected = Parsed::new();
        expected.set_year(123).unwrap();

        let date = vec![
            DateToken::Plus,
            DateToken::Number(expected.year.unwrap().to_string(), None),
        ];
        let (res, parsed) = parse(date.clone(), "year");
        assert!(res.is_ok());
        assert_eq!(parsed, expected);

        let date = vec![DateToken::Number(
            expected.year.unwrap().to_string(),
            None,
        )];
        let (res, parsed2) = parse(date.clone(), "year");
        assert!(res.is_ok());
        assert_eq!(parsed2, parsed);
    }

    #[test]
    fn test_complicated_date_input() {
        let mut expected = Parsed::new();
        expected.set_year(123).unwrap();
        expected.set_month(5).unwrap();
        expected.set_day(2).unwrap();
        expected.set_ampm(true).unwrap();
        expected.set_hour(13).unwrap();
        expected.set_minute(57).unwrap();

        let date = vec![
            DateToken::Number(expected.day.unwrap().to_string(), None),
            DateToken::Space,
            DateToken::Literal("Pm".into()),
            DateToken::Dash,
            DateToken::Number(format!("{:02}", expected.month.unwrap()), None),
            DateToken::Colon,
            DateToken::Number(expected.year.unwrap().to_string(), None),
            DateToken::Space,
            DateToken::Number(format!("{:02}", expected.hour_mod_12.unwrap()), None),
            DateToken::Dash,
            DateToken::Number(format!("{:02}", expected.minute.unwrap()), None),
            DateToken::Space,
            DateToken::Literal("May".into()),
        ];

        let (res, parsed) = parse(date, "day meridiem-monthnum:year hour12-min monthname");
        assert!(res.is_ok());
        assert_eq!(parsed, expected);
    }

    #[test]
    fn ad_bc() {
        let year = -100;
        let mut expected = Parsed::new();
        expected.set_year(year + 1).unwrap();
        expected.set_hour(7).unwrap();

        let date = vec![
            DateToken::Number(year.abs().to_string(), None),
            DateToken::Space,
            DateToken::Literal("bce".into()),
            DateToken::Space,
            DateToken::Number(format!("{:02}", expected.hour_mod_12.unwrap()), None),
            DateToken::Space,
            DateToken::Literal("am".into()),
        ];

        let (res, parsed) = parse(date, "year adbc hour12 meridiem");
        assert!(res.is_ok(), res.unwrap_err());
        assert_eq!(parsed, expected);
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
        assert_eq!(res, Err(format!("Expected 2-digit hour24, got `{}`", 7)));
    }

    #[test]
    fn test_24h() {
        let (res, parsed) = parse(vec![DateToken::Number("23".into(), None)], "hour24");
        assert!(res.is_ok());
        assert_eq!(parsed.hour_div_12, Some(1));
        assert_eq!(parsed.hour_mod_12, Some(11));
    }

    #[test]
    fn seconds() {
        let mut expected = Parsed::new();
        expected.set_second(27).unwrap();
        expected.set_nanosecond(12345).unwrap();

        let date = vec![DateToken::Number("27".into(), Some("000012345".into()))];
        let (res, parsed) = parse(date, "sec");
        assert!(res.is_ok());
        assert_eq!(parsed, expected);

        let date = vec![DateToken::Number("27".into(), None)];
        let (res, parsed) = parse(date, "sec");
        assert!(res.is_ok());
        expected.nanosecond = None;
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_offset() {
        let date = vec![DateToken::Plus, DateToken::Number("0200".into(), None)];
        let (res, parsed) = parse(date, "offset");
        assert!(res.is_ok());
        assert_eq!(parsed.offset, Some(2 * 3600));

        let date = vec![
            DateToken::Dash,
            DateToken::Number("01".into(), None),
            DateToken::Colon,
            DateToken::Number("23".into(), None),
        ];
        let (res, parsed) = parse(date, "offset");
        assert!(res.is_ok());
        assert_eq!(parsed.offset, Some(-(1 * 60 + 23) * 60));

        let date = vec![DateToken::Literal("Europe/London".into())];
        let (res, parsed, tz) = parse_with_tz(date, "offset");
        assert!(res.is_ok(), res.unwrap_err());
        assert_eq!(tz.unwrap(), Tz::Europe__London);
        assert_eq!(parsed.offset, None);
    }

    #[test]
    fn test_weekday() {
        let date = vec![DateToken::Literal("saturday".into())];
        let (res, parsed) = parse(date, "weekday");
        assert!(res.is_ok());
        assert_eq!(parsed.weekday, Some(Weekday::Sat));

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
            .into_iter()
            .enumerate()
        {
            let date = vec![DateToken::Literal(s.into())];
            let (res, parsed) = parse(date, "monthname");
            assert!(res.is_ok());
            assert_eq!(parsed.month, Some(i as u32 + 1));
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

        macro_rules! check_attempt {
            ($date:expr, $pat:expr) => {{
                let pat = parse_datepattern(&mut $pat.chars().peekable()).unwrap();
                attempt($date, pat.as_ref())
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
