use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_with_config() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .arg("3 feet to meters")
        .assert()
        .success()
        .stdout(predicate::eq("> 3 feet to meters\n0.9144 meter (length)\n"));
}

#[test]
fn test_invalid_expr() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .arg("doesnt_exist")
        .assert()
        .failure()
        .stdout(predicate::eq("> doesnt_exist\nNo such unit doesnt_exist\n"));
}

#[test]
fn test_invalid_config() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("config_that_doesnt_exist.toml")
        .arg("3 feet to meters")
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Failed to read provided config file `config_that_doesnt_exist.toml`",
        ));
}

#[test]
fn test_run_file() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .arg("-f")
        .arg("tests/example.units")
        .assert()
        .success()
        .stdout(predicate::eq(
            "approx. 9.365338 meter / second^2 (acceleration)\n",
        ));
}

#[test]
fn test_run_stdin() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .arg("-f")
        .arg("-")
        .write_stdin("3 feet to meters\n")
        .assert()
        .success()
        .stdout(predicate::eq("0.9144 meter (length)\n"));

    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_for_tests.toml")
        .arg("-f")
        .arg("-")
        .write_stdin("doesnt_exist\n")
        .assert()
        .success()
        .stdout(predicate::eq("No such unit doesnt_exist\n"));
}
