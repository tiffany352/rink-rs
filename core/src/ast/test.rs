use super::Expr::*;
use super::Function;

fn check<T: ::std::fmt::Display>(e: T, expected: &str) {
    assert_eq!(e.to_string(), expected);
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
