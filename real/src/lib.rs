mod float;
mod real;
mod rreal;
pub(crate) mod util;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

pub(crate) use float::BigFloat;
pub use real::Real;
pub(crate) use rreal::RReal;
