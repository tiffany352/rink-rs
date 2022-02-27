use core::ops;

use crate::BaseUnitId;
use serde_derive::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(transparent)]
pub struct Dimensionality(Vec<(BaseUnitId, i64)>);

impl Dimensionality {
    pub fn dimensionless() -> Dimensionality {
        Dimensionality(vec![])
    }

    pub fn base_unit(id: BaseUnitId) -> Dimensionality {
        Dimensionality(vec![(id, 1)])
    }

    pub fn recip(self) -> Dimensionality {
        Dimensionality(self.0.into_iter().map(|(u, v)| (u, -v)).collect())
    }

    pub fn pow(self, exponent: i64) -> Dimensionality {
        Dimensionality(self.0.into_iter().map(|(u, v)| (u, v * exponent)).collect())
    }
}

impl ops::Deref for Dimensionality {
    type Target = [(BaseUnitId, i64)];

    fn deref(&self) -> &Self::Target {
        &self.0[..]
    }
}

impl ops::MulAssign for Dimensionality {
    fn mul_assign(&mut self, rhs: Self) {
        let mut left = self.0.drain(..).collect::<Vec<_>>().into_iter().peekable();
        let mut right = rhs.0.into_iter().peekable();

        loop {
            let left_val = left.peek();
            let right_val = right.peek();

            match (left_val, right_val) {
                (None, None) => break,
                (None, Some(_)) => self.0.push(right.next().unwrap()),
                (Some(_), None) => self.0.push(left.next().unwrap()),
                (Some(a), Some(b)) if a.0 < b.0 => self.0.push(left.next().unwrap()),
                (Some(a), Some(b)) if a.0 > b.0 => self.0.push(right.next().unwrap()),
                (Some(a), Some(b)) => {
                    self.0.push((a.0, a.1 + b.1));
                    let _ = (left.next(), right.next());
                }
            }
        }
    }
}

impl ops::Mul for Dimensionality {
    type Output = Dimensionality;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self *= rhs;
        self
    }
}

impl ops::DivAssign for Dimensionality {
    fn div_assign(&mut self, rhs: Self) {
        *self *= rhs.recip();
    }
}

impl ops::Div for Dimensionality {
    type Output = Dimensionality;

    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.recip()
    }
}
