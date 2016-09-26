// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ast::{DatePattern, DateToken, show_datepattern};
use chrono::format::Parsed;
use chrono::{Weekday, DateTime, UTC, FixedOffset, Duration, Date};
use context::Context;
use number::{Number, Dim};
use std::iter::Peekable;

pub fn parse_date<I>(
    out: &mut Parsed,
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
                },
                x => Err(format!("Expected 2-digit hour24, got {}", ts(x)))
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
                let s = match take!(DateToken::Plus | DateToken::Dash) {
                    DateToken::Plus => 1, DateToken::Dash => -1, _ => panic!()
                };
                let h = take!(DateToken::Number(s, None), s);
                let h = i32::from_str_radix(&*h, 10).unwrap();
                take!(DateToken::Colon);
                let m = take!(DateToken::Number(s, None), s);
                let m = i32::from_str_radix(&*m, 10).unwrap();
                out.offset = Some(s * (h*3600 + m*60));
                Ok(())
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
            match parse_date(out, &mut iter, &pats[..]) {
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
        Ok(()) => parse_date(out, date, &pat[1..]),
        Err(e) => Err(e)
    }
}

pub fn try_decode(date: &[DateToken], context: &Context) -> Result<DateTime<FixedOffset>, String> {
    let mut best = None;
    for pat in &context.datepatterns {
        //println!("Tring {:?} against {}", date, show_datepattern(pat));
        let attempt = || {
            let mut parsed = Parsed::new();
            let mut iter = date.iter().cloned().peekable();
            let res = parse_date(&mut parsed, &mut iter, &pat[..]);
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
            let offset = parsed.to_fixed_offset().unwrap_or(FixedOffset::east(0));
            let time = parsed.to_naive_time();
            let date = parsed.to_naive_date();
            match (time, date) {
                (Ok(time), Ok(date)) =>
                    Ok(DateTime::<FixedOffset>::from_utc(date.and_time(time), offset)),
                (Ok(time), Err(_)) =>
                    Ok(UTC::now().with_timezone(&offset).date().and_time(time).unwrap()),
                (Err(_), Ok(date)) =>
                    Ok(Date::<FixedOffset>::from_utc(date, offset).and_hms(0, 0, 0)),
                _ => Err((format!("Failed to construct a useful datetime"), count))
            }
        };
        match attempt() {
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
        Err(format!("Invalid date literal"))
    }
}

pub fn to_duration(num: &Number) -> Result<Duration, String> {
    use gmp::mpq::Mpq;
    use gmp::mpz::Mpz;

    if num.1.len() != 1 || num.1.get("s") != Some(&1) {
        return Err(format!("Expected seconds"))
    }
    let max = Mpq::ratio(&Mpz::from(i64::max_value() / 1000), &Mpz::one());
    if num.0.abs() > max {
        return Err(format!("Implementation error: Number is out of range ({:?})", max))
    }
    let ms_div = Mpz::from(1_000);
    let ms = &(&num.0.get_num() * &ms_div) / &num.0.get_den();
    let ns_div = Mpz::from(1_000_000_000);
    let ns = &num.0 - &Mpq::ratio(&ms, &ms_div);
    let ns = &(&ns.get_num() * &ns_div) / &ns.get_den();
    let ms: Option<i64> = (&ms).into();
    let ns: Option<i64> = (&ns).into();
    Ok(Duration::milliseconds(ms.unwrap()) + Duration::nanoseconds(ns.unwrap()))
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
    Ok(Number::new_unit(&ms + &ns, Dim::new("s")))
}

pub fn now() -> DateTime<FixedOffset> {
    UTC::now().with_timezone(&FixedOffset::east(0))
}

pub fn parse_datepattern<I>(mut iter: &mut Peekable<I>)
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
                    return Err(format!("Expected ]"))
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
        if line.len() == 0 {
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
