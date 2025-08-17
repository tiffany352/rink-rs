use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_run_interactive() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .write_stdin("3 feet to meters\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(predicate::eq("0.9144 meter (length)\n"));
}
