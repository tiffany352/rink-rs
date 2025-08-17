use nu_ansi_term::Color::*;
use nu_ansi_term::Style;
use serde_derive::{Deserialize, Serialize};

use rink::style_ser::parse_color;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Wrap {
    #[serde(with = "rink::style_ser")]
    style: Style,
}

#[test]
fn roundtrip() {
    fn check(style: Style) {
        let style = Wrap { style };
        let toml_str = toml::to_string(&style).unwrap();
        let des = toml::from_str::<Wrap>(&toml_str).unwrap();
        assert_eq!(style, des);
    }

    let tests = [
        Black,
        Red,
        Green,
        Yellow,
        Blue,
        Purple,
        Cyan,
        White,
        DarkGray,
        LightRed,
        LightGreen,
        LightYellow,
        LightBlue,
        LightPurple,
        Magenta,
        LightMagenta,
        LightCyan,
        LightGray,
        // doesn't roundtrip
        //Default,
        Fixed(0),
        Fixed(255),
        Rgb(0, 0, 0),
        Rgb(255, 255, 255),
        Rgb(1, 2, 3),
        Rgb(3, 2, 1),
    ];
    for test in tests {
        check(Style::new().fg(test));
        check(Style::new().on(test));
        check(Style::new().fg(test).on(test));
    }
    check(Style::new().fg(Red).on(Black).dimmed().underline());
}

#[test]
fn parse_colors() {
    assert_eq!(parse_color("black"), Some(Black));
    assert_eq!(parse_color("red"), Some(Red));
    assert_eq!(parse_color("#123456"), Some(Rgb(0x12, 0x34, 0x56)));
    assert_eq!(parse_color(""), None);
    assert_eq!(parse_color("asdf"), None);
    assert_eq!(parse_color("#123"), None);
    assert_eq!(parse_color("#1234"), None);
    assert_eq!(parse_color("#123456789"), None);
    assert_eq!(parse_color("#abc"), None);
}
