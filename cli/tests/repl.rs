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

// Causes a 100% repro for a bug that causes code coverage to fail.
// "invalid instrumentation profile data (file header is corrupt)"
#[test]
#[ignore]
fn test_sandboxed_panic_handler() {
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_sandboxed.toml")
        .write_stdin("__super_secret_plz_crash\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(
            predicate::eq(
                "The subprocess panicked (crashed).
Message: Thank you for playing Wing Commander
Location: core/src/runtime/eval.rs:76

Backtrace omitted.
Run with RUST_BACKTRACE=1 environment variable to display it.
Run with RUST_BACKTRACE=full to include source snippets.",
            )
            .trim(),
        );
}
