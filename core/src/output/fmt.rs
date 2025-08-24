// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::{borrow::Cow, fmt, iter::Peekable};

use serde_derive::Serialize;

/// Represents a node in a token tree. Each token is tagged with a hint
/// for how it should be displayed.
///
/// To process a list of these objects, you should iterate through it,
/// building up your final format string as you go. This allows the
/// addition of color, bold, and other formatting to replies, when
/// outputting to a format where those are available.
///
/// # Example
///
/// ```rs
/// fn write_string<'a>(string: &mut String, obj: &'a dyn TokenFmt<'a>) {
///     let spans = obj.to_spans();
///     for span in spans {
///         match span {
///             // In most cases you would apply varying formatting based on
///             // the FmtToken, as otherwise simply using Display would
///             // suffice.
///             Span::Content { text, token } => string.push_str(&text),
///             Span::Child(obj) => write_string(string, obj),
///         }
///     }
/// }
/// ```
#[derive(Clone)]
pub enum Span<'a> {
    Content { text: Cow<'a, str>, token: FmtToken },
    Child(&'a dyn TokenFmt<'a>),
}

impl<'a> Span<'a> {
    /// Creates a new span given the text and token it should represent.
    /// This is a leaf in the tree.
    pub fn new(text: impl Into<Cow<'a, str>>, token: FmtToken) -> Span<'a> {
        Span::Content {
            text: text.into(),
            token,
        }
    }

    /// Creates a new span with FmtToken::Plain
    pub fn plain(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Plain)
    }

    /// Creates a new span with FmtToken::Error
    pub fn error(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Error)
    }

    /// Creates a new span with FmtToken::Unit
    pub fn unit(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Unit)
    }

    /// Creates a new span with FmtToken::Quantity
    pub fn quantity(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Quantity)
    }

    /// Creates a new span with FmtToken::Number
    pub fn number(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Number)
    }

    /// Creates a new span with FmtToken::PropName
    pub fn prop_name(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::PropName)
    }

    /// Creates a new span with FmtToken::UserInput
    pub fn user_input(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::UserInput)
    }

    /// Creates a new span with FmtToken::ListBegin
    pub fn list_begin(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::ListBegin)
    }

    /// Creates a new span with FmtToken::ListSep
    pub fn list_sep(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::ListSep)
    }

    /// Creates a new span with FmtToken::DocString
    pub fn doc_string(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::DocString)
    }

    /// Creates a new span with FmtToken::Pow
    pub fn pow(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Pow)
    }

    /// Creates a new span with FmtToken::DateTime
    pub fn date_time(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::DateTime)
    }

    /// Creates a node with children in the token tree, given any object
    /// that implements TokenFmt.
    pub fn child(obj: &'a dyn TokenFmt<'a>) -> Span<'a> {
        Span::Child(obj)
    }

    /// Creates a new span with FmtToken::Link
    pub fn link(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Link)
    }

    // Creates a new span with FmtToken::Keyword
    pub fn keyword(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::Keyword)
    }

    // Creates a new span with FmtToken::TimeZone
    pub fn timezone(text: impl Into<Cow<'a, str>>) -> Span<'a> {
        Span::new(text, FmtToken::TimeZone)
    }

    pub fn is_ws(&self) -> bool {
        if let Span::Content { text, .. } = self {
            text.ends_with(" ")
        } else {
            false
        }
    }
}

impl<'a> fmt::Debug for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Span::Content { text, token } => write!(f, "({:?}, {:?})", text, token),
            Span::Child(obj) => {
                let spans = obj.to_spans();
                spans.fmt(f)
            }
        }
    }
}

/// Provides a hint for how a string should be displayed, based on its
/// contents.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum FmtToken {
    /// Indicator text that isn't based on user input.
    /// Generally displayed without any formatting.
    Plain,
    /// Similar to plain, but indicates text that is used in an error context.
    /// Generally displayed in red.
    Error,
    /// The name of a unit, like `kilogram`.
    Unit,
    /// A quantity like length or time.
    Quantity,
    /// A number in any context.
    Number,
    /// A string that's derived in some way from user input, but doesn't
    /// have a more specific usage. This is used for unit not found
    /// errors.
    UserInput,
    /// Text indicating the start of a list, usually a string followed
    /// by a colon.
    ListBegin,
    /// A separator between items in a list, usually a comma or
    /// semicolon. When a lot of vertical space is available, these can
    /// be turned into a bulleted list.
    ListSep,
    /// A documentation string.
    DocString,
    /// A number raised to a power, like `^32`.
    /// When possible, replace with superscript.
    Pow,
    /// The name of a property in a substance.
    PropName,
    /// A date time, either being printed, or from user input.
    /// Sometimes found inside doc strings.
    /// Suggested parsing:
    /// 1. ISO 8601 timestamp (yyyy-mm-ddThh:mm:ss +oo:oo)
    /// 2. ISO 8601 date (yyyy-mm-dd)
    /// 3. Fallback to displaying original text
    DateTime,
    /// A URL, typically found inside of a doc string.
    /// Intended to be made clickable.
    /// Generally formatted like `http://example.com`.
    Link,
    /// Reserved words in Rink's query language
    Keyword,
    /// Timezone conversions in syntax highlighted queries
    TimeZone,
}

pub(crate) fn write_spans_string(out: &mut String, spans: &[Span]) {
    for span in spans {
        match span {
            Span::Content { text, .. } => out.push_str(text),
            Span::Child(child) => write_spans_string(out, &child.to_spans()),
        }
    }
}

/// Allows an object to be converted into a token tree.
pub trait TokenFmt<'a> {
    fn to_spans(&'a self) -> Vec<Span<'a>>;

    fn spans_to_string(&'a self) -> String {
        let mut string = String::new();
        write_spans_string(&mut string, &self.to_spans());
        string
    }

    fn display(&'a self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        for span in self.to_spans() {
            match span {
                Span::Content { text, .. } => write!(fmt, "{}", text)?,
                Span::Child(child) => child.display(fmt)?,
            }
        }
        Ok(())
    }
}

pub(crate) struct JoinIter<'a, I>
where
    I: Iterator,
{
    iter: Peekable<I>,
    sep: Span<'a>,
    last_was_sep: bool,
}

pub(crate) fn join<'a, I>(iter: I, sep: Span<'a>) -> impl Iterator<Item = Span<'a>>
where
    I: Iterator<Item = Span<'a>>,
{
    JoinIter {
        iter: iter.peekable(),
        sep,
        last_was_sep: true,
    }
}

impl<'a, I> Iterator for JoinIter<'a, I>
where
    I: Iterator<Item = Span<'a>>,
{
    type Item = Span<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.peek().is_some() {
            if self.last_was_sep {
                self.last_was_sep = false;
                self.iter.next()
            } else {
                self.last_was_sep = true;
                Some(self.sep.clone())
            }
        } else {
            None
        }
    }
}

pub(crate) struct FlatJoinIter<'a, I, I2>
where
    I: Iterator<Item = I2>,
    I2: IntoIterator<Item = Span<'a>>,
{
    iter: Peekable<I>,
    sep: Span<'a>,
    last_was_sep: bool,
    current: Option<I2::IntoIter>,
}

pub(crate) fn flat_join<'a, I, I2>(iter: I, sep: Span<'a>) -> impl Iterator<Item = Span<'a>>
where
    I: Iterator<Item = I2>,
    I2: IntoIterator<Item = Span<'a>>,
{
    FlatJoinIter {
        iter: iter.peekable(),
        sep,
        last_was_sep: true,
        current: None,
    }
}

impl<'a, I, I2> Iterator for FlatJoinIter<'a, I, I2>
where
    I: Iterator<Item = I2>,
    I2: IntoIterator<Item = Span<'a>>,
{
    type Item = Span<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ref mut current) = self.current {
            if let Some(next) = current.next() {
                return Some(next);
            }
        }
        if self.iter.peek().is_some() {
            if self.last_was_sep {
                self.last_was_sep = false;
                let mut new_current = self.iter.next().unwrap().into_iter();
                let next = new_current.next();
                self.current = Some(new_current);
                next
            } else {
                self.last_was_sep = true;
                Some(self.sep.clone())
            }
        } else {
            None
        }
    }
}
