use rink_real::Real;

#[test]
fn test_add() {
    let value = Real::from(10) + Real::from(2);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "12");

    let value = Real::fraction(1100, 100) + Real::fraction(23, 100);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "11.23");
}

#[test]
fn test_sub() {
    let value = Real::from(1) - Real::from(1);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "0");

    let value = Real::fraction(2, 3) - Real::fraction(1, 3);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "0.33333");
}

#[test]
fn test_mul() {
    let value = Real::pi() * Real::from(0);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "0");

    let value = Real::fraction(2, 3) * Real::from(6);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "4");
}

#[test]
fn test_div() {
    let value = Real::from(10) / Real::from(5);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "2");

    let value = Real::from(1) / Real::from(8);
    let value = format!("{}", value.to_string(5, 10));
    assert_eq!(value, "0.125");
}
