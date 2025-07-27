// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::Substance;
use crate::loader::Context;
use crate::parsing::datetime;
use crate::types::{DateTime, Number};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Clone, Debug)]
pub enum Value {
    Number(Number),
    DateTime(DateTime),
    Substance(Substance),
}

pub trait Show {
    /// Provides a string representation of something, using information contained in a Context.
    fn show(&self, context: &Context) -> String;
}

impl Show for DateTime {
    fn show(&self, context: &Context) -> String {
        if let Some(h) = context.humanize(*self) {
            format!("{} ({})", self, h)
        } else {
            self.to_string()
        }
    }
}

impl Show for Value {
    fn show(&self, context: &Context) -> String {
        match *self {
            Value::Number(ref num) => num.show(context),
            Value::DateTime(ref dt) => dt.show(context),
            Value::Substance(ref v) => v.show(context),
        }
    }
}

impl Value {
    pub fn pow(&self, exp: &Value) -> Result<Value, String> {
        match (self, exp) {
            (&Value::Number(ref left), &Value::Number(ref right)) => {
                left.pow(right).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }
}

impl<'a, 'b> Add<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn add(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => (left + right)
                .ok_or_else(|| {
                    "Addition of units with mismatched units is not meaningful".to_string()
                })
                .map(Value::Number),
            (&Value::DateTime(ref left), &Value::Number(ref right))
            | (&Value::Number(ref right), &Value::DateTime(ref left)) => left
                .checked_add_signed(datetime::to_duration(right)?)
                .ok_or_else(|| {
                    "Implementation error: value is out of range representable by datetime"
                        .to_string()
                })
                .map(Value::DateTime),
            (&Value::Substance(ref left), &Value::Substance(ref right)) => {
                left.add(right).map(Value::Substance)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }
}

impl<'a, 'b> Sub<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn sub(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => (left - right)
                .ok_or_else(|| {
                    "Subtraction of units with mismatched units is not meaningful".to_string()
                })
                .map(Value::Number),
            (&Value::DateTime(ref left), &Value::Number(ref right))
            | (&Value::Number(ref right), &Value::DateTime(ref left)) => left
                .checked_sub_signed(datetime::to_duration(right)?)
                .ok_or_else(|| {
                    "Implementation error: value is out of range representable by datetime"
                        .to_string()
                })
                .map(Value::DateTime),
            (&Value::DateTime(ref left), &Value::DateTime(ref right)) => {
                datetime::from_duration(&(*left - *right)).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }
}

impl<'a> Neg for &'a Value {
    type Output = Result<Value, String>;

    fn neg(self) -> Self::Output {
        match *self {
            Value::Number(ref num) => (-num)
                .ok_or_else(|| "Bug: Negation should not fail".to_string())
                .map(Value::Number),
            _ => Err("Operation is not defined".to_string()),
        }
    }
}

impl<'a, 'b> Mul<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn mul(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => (left * right)
                .ok_or_else(|| "Bug: Mul should not fail".to_string())
                .map(Value::Number),
            (&Value::Number(ref co), &Value::Substance(ref sub))
            | (&Value::Substance(ref sub), &Value::Number(ref co)) => {
                (sub * co).map(Value::Substance)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }
}

impl<'a, 'b> Div<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn div(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => (left / right)
                .ok_or_else(|| "Division by zero".to_string())
                .map(Value::Number),
            (&Value::Substance(ref sub), &Value::Number(ref co)) => {
                (sub / co).map(Value::Substance)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }
}

impl Value {
    pub fn shl(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => {
                left.shl(right).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }

    pub fn shr(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => {
                left.shr(right).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }

    pub fn rem(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => {
                left.rem(right).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }

    pub fn and(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => {
                left.and(right).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }

    pub fn or(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => {
                left.or(right).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }

    pub fn xor(&self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) => {
                left.xor(right).map(Value::Number)
            }
            (_, _) => Err("Operation is not defined".to_string()),
        }
    }
}
