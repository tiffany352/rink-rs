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

#[test]
fn test_invalid() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .write_stdin("doesnt_exist\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(predicate::eq("No such unit doesnt_exist\n"));
}

#[test]
fn test_help_text() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .write_stdin("help\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(predicate::eq(rink::repl::HELP_TEXT).trim());
}

#[test]
fn test_sandboxed() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_sandboxed.toml")
        .write_stdin("3 feet to meters\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(predicate::str::starts_with(
            "0.9144 meter (length)\nFinished in ",
        ));

    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_sandboxed.toml")
        .write_stdin("doesnt_exist\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(predicate::str::starts_with(
            "No such unit doesnt_exist\nFinished in ",
        ));
}
