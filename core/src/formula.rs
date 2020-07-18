// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::number::{Dimension, Number};
use crate::numeric::Numeric;
use crate::substance::{Properties, Property, Substance};
use std::collections::BTreeMap;
use std::iter::Peekable;
use std::str::{Chars, FromStr};
use std::sync::Arc;

enum Token {
    Symbol(String),
    Count(u32),
    Error,
}

#[derive(Clone)]
struct TokenIterator<'a>(Peekable<Chars<'a>>);

impl<'a> TokenIterator<'a> {
    pub fn new(input: &'a str) -> TokenIterator<'a> {
        TokenIterator(input.chars().peekable())
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.0.peek()?;
        let res = match self.0.next().unwrap() {
            letter @ 'A'..='Z' => {
                let mut symbol = String::new();
                symbol.push(letter);
                if let Some('a'..='z') = self.0.peek().cloned() {
                    symbol.push(self.0.next().unwrap())
                }
                Token::Symbol(symbol)
            }
            digit @ '0'..='9' => {
                let mut integer = String::new();
                integer.push(digit);
                while let Some('0'..='9') = self.0.peek().cloned() {
                    integer.push(self.0.next().unwrap())
                }
                Token::Count(u32::from_str(&integer).unwrap())
            }
            _ => Token::Error,
        };
        Some(res)
    }
}

/**
 * Compute the molar mass of a compound given its chemical formula.
 */
pub fn substance_from_formula(
    formula: &str,
    symbols: &BTreeMap<String, String>,
    substances: &BTreeMap<String, Substance>,
) -> Option<Substance> {
    let mut molar_mass_unit = BTreeMap::new();
    molar_mass_unit.insert(Dimension::new("kg"), 1);
    molar_mass_unit.insert(Dimension::new("mol"), -1);
    let mut total_molar_mass = Number {
        value: Numeric::from(0),
        unit: molar_mass_unit,
    };

    let mut iter = TokenIterator::new(formula).peekable();
    while let Some(token) = iter.next() {
        match token {
            Token::Symbol(ref sym) if symbols.contains_key(sym) => {
                let count = match iter.peek() {
                    Some(&Token::Count(n)) => {
                        iter.next().unwrap();
                        Number::new(Numeric::from(n as i64))
                    }
                    _ => Number::one(),
                };

                let subst = substances.get(symbols.get(sym).unwrap()).unwrap();
                match subst.get("molar_mass") {
                    Ok(subst_molar_mass) => {
                        let subst_molar_mass = (&subst_molar_mass * &count).unwrap();
                        total_molar_mass = (&total_molar_mass + &subst_molar_mass).unwrap();
                    }
                    Err(_) => return None,
                }
            }
            _ => return None,
        }
    }

    let mut props = BTreeMap::new();
    props.insert(
        "molar_mass".to_owned(),
        Property {
            output: total_molar_mass,
            output_name: "mass".to_owned(),
            input: Number::one(),
            input_name: "amount".to_owned(),
            doc: None,
        },
    );
    Some(Substance {
        amount: Number::one(),
        properties: Arc::new(Properties {
            name: formula.to_owned(),
            properties: props,
        }),
    })
}
