use std::fmt;

use serde_derive::Serialize;

use super::fmt::{Span, TokenFmt};

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(transparent)]
pub struct DocString {
    pub text: String,
}

impl DocString {
    pub fn new(text: impl Into<String>) -> DocString {
        DocString {
            text: text.into()
        }
    }
}

impl fmt::Display for DocString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.text.fmt(f)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum DocState {
    Plain,
    Link,
    Date,
}

struct DocParser<'a> {
    tokens: Vec<Span<'a>>,
    current: String,
    state: DocState,
}

impl<'a> DocParser<'a> {
    fn finish(&mut self) {
        if !self.current.is_empty() {
            let current = std::mem::replace(&mut self.current, String::new());
            match self.state {
                DocState::Plain => self.tokens.push(Span::doc_string(current)),
                DocState::Date => self.tokens.push(Span::date_time(current)),
                DocState::Link => self.tokens.push(Span::link(current)),
            }
            self.state = DocState::Plain;
        }
    }
}

impl<'a> TokenFmt<'a> for DocString {
    fn to_spans(&'a self) -> Vec<Span<'a>> {
        let mut parser = DocParser {
            tokens: vec![],
            current: String::new(),
            state: DocState::Plain,
        };

        for ch in self.text.chars() {
            match ch {
                '>' if parser.state == DocState::Link => parser.finish(),
                _ if parser.state == DocState::Link => parser.current.push(ch),
                '#' if parser.state == DocState::Date => parser.finish(),
                _ if parser.state == DocState::Date => parser.current.push(ch),
                '<' => {
                    parser.finish();
                    parser.state = DocState::Link;
                }
                '#' => {
                    parser.finish();
                    parser.state = DocState::Date;
                }
                _ => parser.current.push(ch),
            }
        }
        parser.finish();

        parser.tokens
    }
}
