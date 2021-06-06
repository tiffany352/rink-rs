use rink_real::Real;

#[test]
fn test_works_at_all() {
    let value = Real::from(2).sqrt();
    let true_digits = LONG_SQRT_2
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .collect::<String>();

    println!("{:#?}", value);
    let digits = value.to_string(6, 10);
    let digits = format!("{}", digits);

    assert_eq!(digits, true_digits[0..8]);
}

#[test]
fn test_accuracy() {
    let value = Real::from(2).sqrt();
    let true_digits = LONG_SQRT_2
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .collect::<String>();

    let digits = value.to_string(100, 10);
    let digits = format!("{}", digits);

    let count = true_digits
        .chars()
        .zip(digits.chars())
        .take_while(|(l, r)| l == r)
        .count();
    assert_eq!(count, 102);
}

// 100 digits of sqrt(2) from https://nerdparadise.com/math/reference/2sqrt10000
const LONG_SQRT_2: &'static str = r#"1.
4142135623 7309504880 1688724209 6980785696 7187537694 8073176679 7379907324 7846210703 8850387534 3276415727
"#;
