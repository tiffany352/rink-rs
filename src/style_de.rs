// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::fmt;

use ansi_term::{Color, Style};
use serde::{
    de::{Error, Unexpected, Visitor},
    Deserializer,
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
        _ => {
            let value = input.parse::<u8>();
            if let Ok(value) = value {
                Some(Color::Fixed(value))
            } else if input.starts_with("rgb(") && input.ends_with(')') {
                let input = &input[4..input.len() - 1];
                let mut colors = input.split(',').map(|num| num.parse::<u8>().ok()).flatten();
                let r = colors.next();
                let g = colors.next();
                let b = colors.next();
                match (r, g, b) {
                    (Some(r), Some(g), Some(b)) => Some(Color::RGB(r, g, b)),
                    _ => None,
                }
            } else if input.starts_with('#') {
                let input = input.trim_start_matches('#');
                let value = u32::from_str_radix(input, 16);
                if let Ok(value) = value {
                    let r = (value >> 16) as u8;
                    let g = (value >> 8) as u8;
                    let b = value as u8;
                    Some(Color::RGB(r, g, b))
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

pub fn deserialize_style<'de, D>(des: D) -> Result<Style, D::Error>
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
