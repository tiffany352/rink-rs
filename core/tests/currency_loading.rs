#[test]
fn load_currency() {
    let mut ctx = rink_core::simple_context().unwrap();
    let live_data = include_str!("../tests/currency.snapshot.json");
    let base_defs = rink_core::CURRENCY_FILE.unwrap();
    ctx.load_currency(&live_data, base_defs).unwrap();

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
