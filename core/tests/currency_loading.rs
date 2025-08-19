#[test]
#[cfg(feature = "serde_json")]
fn load_currency_with_live_data() {
    let mut ctx = rink_core::simple_context().unwrap();
    let live_data = include_str!("../tests/currency.snapshot.json");
    let base_defs = rink_core::CURRENCY_FILE.unwrap();
    ctx.load_currency(Some(&live_data), base_defs).unwrap();

    let result = rink_core::one_line(&mut ctx, "USD");
    assert_eq!(
        result,
        Ok("Definition: USD = (1 / 1.0852) EUR = \
            approx. 921.4891 millieuro (money; EUR). \
            Sourced from European Central Bank. \
            Current as of 2024-05-31."
            .to_owned())
    );
}

#[test]
#[cfg(feature = "serde_json")]
fn load_currency_without_live_data() {
    let mut ctx = rink_core::simple_context().unwrap();
    let base_defs = rink_core::CURRENCY_FILE.unwrap();
    ctx.load_currency(None, base_defs).unwrap();

    // Direct
    let result = rink_core::one_line(&mut ctx, "USD");
    assert_eq!(result, Err("Missing dependencies: USD".to_owned()));

    // Alias
    let result = rink_core::one_line(&mut ctx, "$");
    assert_eq!(result, Err("Missing dependencies: USD".to_owned()));

    // Derived unit
    let result = rink_core::one_line(&mut ctx, "cent");
    assert_eq!(result, Err("Missing dependencies: USD".to_owned()));

    // Direct
    let result = rink_core::one_line(&mut ctx, "bitcoin");
    assert_eq!(result, Err("Missing dependencies: bitcoin".to_owned()));

    // Alias
    let result = rink_core::one_line(&mut ctx, "BTC");
    assert_eq!(result, Err("Missing dependencies: BTC".to_owned()));

    // Prefix
    let result = rink_core::one_line(&mut ctx, "mBTC");
    assert_eq!(result, Err("Missing dependencies: BTC".to_owned()));
}
