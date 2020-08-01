use super::Expr::{self, *};
use super::Function;

fn check<T: ::std::fmt::Display>(e: T, expected: &str) {
    assert_eq!(e.to_string(), expected);
}

impl From<i64> for Expr {
    fn from(x: i64) -> Self {
        Const(x.into())
    }
}

#[test]
fn test_display_call() {
    check(
        Call {
            func: Function::Sin,
            args: vec![],
        },
        "sin()",
    );
    check(
        Call {
            func: Function::Sin,
            args: vec![1.into()],
        },
        "sin(1)",
    );
    check(
        Call {
            func: Function::Sin,
            args: vec![1.into(), 2.into()],
        },
        "sin(1, 2)",
    );
    check(
        Call {
            func: Function::Sin,
            args: vec![1.into(), 2.into(), 3.into()],
        },
        "sin(1, 2, 3)",
    );
}
