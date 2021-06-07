use rink_real::Real;

//#[test]
fn pi_works_at_all() {
    let pi = Real::pi();
    let value = format!("{}", pi.to_string(5, 10));
    assert_eq!(value, "3.14159");
}

//#[test]
fn test_pi_digits() {
    let pi = Real::pi();

    let results = vec![0, 5, 10, 20, 50]
        .into_iter()
        .map(|num_digits| format!("{}", pi.to_string(num_digits, 10)))
        .collect::<Vec<_>>();

    assert_eq!(
        results,
        vec![
            "3",
            "3.14159",
            "3.1415926535",
            "3.14159265358979323846",
            "3.14159265358979323846264338327950288419716939937510"
        ]
    );
}
