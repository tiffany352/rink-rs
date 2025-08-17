use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_help() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.assert().success();
}

#[test]
fn test_invalid() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("--asdf").assert().failure();
}

#[test]
fn test_version() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    let output = format!("Rink {}\n", env!("CARGO_PKG_VERSION"));
    cmd.arg("--version")
        .assert()
        .success()
        .stdout(predicate::eq(output));

    let mut cmd = Command::cargo_bin("rink").unwrap();
    let output = format!("Rink {}\n", env!("CARGO_PKG_VERSION"));
    cmd.arg("-V")
        .assert()
        .success()
        .stdout(predicate::eq(output));
}

#[test]
fn test_config_path() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("--config-path")
        .assert()
        .success()
        .stdout(predicate::str::ends_with("/config.toml\n"));
}
