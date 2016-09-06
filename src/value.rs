// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use number::Number;
use chrono::{DateTime, FixedOffset};
use eval::Context;
use std::ops::{Add, Div, Mul, Neg, Sub};
use date;

#[derive(Clone)]
pub enum Value {
    Number(Number),
    DateTime(DateTime<FixedOffset>),
}

pub trait Show {
    /// Provides a string representation of something, using information contained in a Context.
    fn show(&self, context: &Context) -> String;
}

#[cfg(feature = "chrono-humanize")]
impl Show for DateTime<FixedOffset> {
    fn show(&self, _context: &Context) -> String {
        use chrono_humanize::HumanTime;
        format!("{} ({})", self, HumanTime::from(*self))
    }
}

#[cfg(not(feature = "chrono-humanize"))]
impl Show for DateTime<FixedOffset> {
    fn show(&self, _context: &Context) -> String {
        format!("{}", self)
    }
}

impl Show for Value {
    fn show(&self, context: &Context) -> String {
        match *self {
            Value::Number(ref num) => num.show(context),
            Value::DateTime(ref dt) => dt.show(context),
        }
    }
}

impl Value {
    pub fn pow(&self, exp: &Value) -> Result<Value, String> {
        match (self, exp) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                left.pow(right).map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Add<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn add(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left + right)
                .ok_or(format!("Addition of units with mismatched units is not meaningful"))
                .map(Value::Number),
            (&Value::DateTime(ref left), &Value::Number(ref right)) |
            (&Value::Number(ref right), &Value::DateTime(ref left)) =>
                left.checked_add(try!(date::to_duration(right)))
                .ok_or(format!("Implementation error: value is out of range representable by datetime"))
                .map(Value::DateTime),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Sub<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn sub(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left - right)
                .ok_or(format!("Subtraction of units with mismatched units is not meaningful"))
                .map(Value::Number),
            (&Value::DateTime(ref left), &Value::Number(ref right)) |
            (&Value::Number(ref right), &Value::DateTime(ref left)) =>
                left.checked_sub(try!(date::to_duration(right)))
                .ok_or(format!("Implementation error: value is out of range representable by datetime"))
                .map(Value::DateTime),
            (&Value::DateTime(ref left), &Value::DateTime(ref right)) =>
                date::from_duration(&(*left - *right))
                .map(Value::Number),
            //(_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a> Neg for &'a Value {
    type Output = Result<Value, String>;

    fn neg(self) -> Self::Output {
        match *self {
            Value::Number(ref num) =>
                (-num).ok_or(format!("Bug: Negation should not fail")).map(Value::Number),
            _ => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Mul<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn mul(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left * right)
                .ok_or(format!("Bug: Mul should not fail"))
                .map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}

impl<'a,'b> Div<&'b Value> for &'a Value {
    type Output = Result<Value, String>;

    fn div(self, other: &Value) -> Result<Value, String> {
        match (self, other) {
            (&Value::Number(ref left), &Value::Number(ref right)) =>
                (left / right)
                .ok_or(format!("Division by zero"))
                .map(Value::Number),
            (_, _) => Err(format!("Operation is not defined"))
        }
    }
}
