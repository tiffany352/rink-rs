// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{borrow::Cow, fmt};

use nu_ansi_term::{Color, Style};
use serde::{
    de::{Error, Unexpected, Visitor},
    Deserializer, Serializer,
};

struct StringVisitor;

impl<'de> Visitor<'de> for StringVisitor {
    type Value = String;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(v.to_owned())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(v)
    }
}

fn parse_color(input: &str) -> Option<Color> {
    match input {
        "black" => Some(Color::Black),
        "red" => Some(Color::Red),
        "green" => Some(Color::Green),
        "yellow" => Some(Color::Yellow),
        "blue" => Some(Color::Blue),
        "purple" => Some(Color::Purple),
        "cyan" => Some(Color::Cyan),
        "white" => Some(Color::White),
        "dark_gray" => Some(Color::DarkGray),
        "light_red" => Some(Color::LightRed),
        "light_green" => Some(Color::LightGreen),
        "light_yellow" => Some(Color::LightYellow),
        "light_blue" => Some(Color::LightBlue),
        "light_purple" => Some(Color::LightPurple),
        "magenta" => Some(Color::Magenta),
        "light_magenta" => Some(Color::LightMagenta),
        "light_cyan" => Some(Color::LightCyan),
        "light_gray" => Some(Color::LightGray),
        _ => {
            let value = input.parse::<u8>();
            if let Ok(value) = value {
                Some(Color::Fixed(value))
            } else if input.starts_with("rgb(") && input.ends_with(')') {
                let input = &input[4..input.len() - 1];
                let mut colors = input.split(',').filter_map(|num| num.parse::<u8>().ok());
                let r = colors.next();
                let g = colors.next();
                let b = colors.next();
                match (r, g, b) {
                    (Some(r), Some(g), Some(b)) => Some(Color::Rgb(r, g, b)),
                    _ => None,
                }
            } else if input.starts_with('#') {
                let input = input.trim_start_matches('#');
                let value = u32::from_str_radix(input, 16);
                if let Ok(value) = value {
                    let r = (value >> 16) as u8;
                    let g = (value >> 8) as u8;
                    let b = value as u8;
                    Some(Color::Rgb(r, g, b))
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

pub fn deserialize<'de, D>(des: D) -> Result<Style, D::Error>
where
    D: Deserializer<'de>,
{
    let string = des.deserialize_str(StringVisitor)?;
    let mut style = Style::new();
    let mut next_color_is_bg = false;
    for word in string.split(' ') {
        match word {
            "bold" => style = style.bold(),
            "italic" => style = style.italic(),
            "dimmed" | "dim" => style = style.dimmed(),
            "underline" | "under" => style = style.underline(),
            "blink" => style = style.blink(),
            "strikethrough" | "strike" => style = style.strikethrough(),
            "hidden" | "none" => style = style.hidden(),
            "on" => next_color_is_bg = true,
            "plain" | "default" => (),
            _ => {
                if let Some(color) = parse_color(word) {
                    if next_color_is_bg {
                        style = style.on(color);
                    } else {
                        style = style.fg(color);
                    }
                } else {
                    return Err(D::Error::invalid_value(
                        Unexpected::Str(word),
                        &"valid color token",
                    ));
                }
            }
        }
    }
    Ok(style)
}

fn color_to_string(color: Color) -> Cow<'static, str> {
    match color {
        Color::Black => "black".into(),
        Color::Red => "red".into(),
        Color::Green => "green".into(),
        Color::Yellow => "yellow".into(),
        Color::Blue => "blue".into(),
        Color::Purple => "purple".into(),
        Color::Cyan => "cyan".into(),
        Color::White => "white".into(),
        Color::DarkGray => "dark_gray".into(),
        Color::LightRed => "light_red".into(),
        Color::LightGreen => "light_green".into(),
        Color::LightYellow => "light_yellow".into(),
        Color::LightBlue => "light_blue".into(),
        Color::LightPurple => "light_purple".into(),
        Color::Magenta => "magenta".into(),
        Color::LightMagenta => "light_magenta".into(),
        Color::LightCyan => "light_cyan".into(),
        Color::LightGray => "light_gray".into(),
        Color::Default => "default".into(),
        Color::Fixed(i) => format!("{}", i).into(),
        Color::Rgb(r, g, b) => format!("rgb({},{},{})", r, g, b).into(),
    }
}

pub fn serialize<S>(style: &Style, ser: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut result: Vec<Cow<'static, str>> = vec![];
    if style.is_bold {
        result.push("bold".into());
    }
    if style.is_italic {
        result.push("italic".into());
    }
    if style.is_dimmed {
        result.push("dimmed".into());
    }
    if style.is_hidden {
        result.push("hidden".into());
    }
    if style.is_blink {
        result.push("blink".into());
    }
    if style.is_reverse {
        result.push("reverse".into());
    }
    if style.is_strikethrough {
        result.push("strikethrough".into());
    }
    if style.is_underline {
        result.push("underline".into());
    }
    if let Some(fg) = style.foreground {
        result.push(color_to_string(fg));
    }
    if let Some(bg) = style.background {
        result.push("on".into());
        result.push(color_to_string(bg));
    }
    if result.is_empty() {
        result.push("plain".into());
    }
    ser.serialize_str(&result.join(" "))
}
