use super::Approx;
use std::fmt::Debug;

pub trait Term: Debug + Send + Sync + 'static {
    fn eval(&self, precision: u64) -> Approx;
}
