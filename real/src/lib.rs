mod real;
mod rreal;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

pub use real::Real;
pub(crate) use rreal::RReal;
