use assert_cmd::Command;
use once_cell::sync::Lazy;
use predicates::prelude::*;
use std::io::Read;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tiny_http::{Response, Server, StatusCode};

static SERVER: Lazy<Mutex<Arc<Server>>> = Lazy::new(|| {
    Mutex::new(Arc::new(
        Server::http("127.0.0.1:3090").expect("port 3090 is needed to do http tests"),
    ))
});

#[test]
fn test_download_timeout() {
    let server = SERVER.lock().unwrap();
    let server2 = server.clone();

    let thread_handle = std::thread::spawn(move || {
        let request = server2.recv().expect("the request should not fail");
        assert_eq!(request.url(), "/data/currency.json");
        std::thread::sleep(Duration::from_millis(100));
    });
    let result = rink::config::download_to_file(
        &PathBuf::from("currency.json"),
        "http://127.0.0.1:3090/data/currency.json",
        Duration::from_millis(5),
    );
    let result = result.expect_err("this should always fail");
    let result = result.to_string();
    assert!(result.starts_with("[28] Timeout was reached (Operation timed out after "));
    assert!(result.ends_with(" milliseconds with 0 bytes received)"));
    thread_handle.join().unwrap();
    drop(server);
}

#[test]
fn test_download_404() {
    let server = SERVER.lock().unwrap();
    let server2 = server.clone();

    let thread_handle = std::thread::spawn(move || {
        let request = server2.recv().expect("the request should not fail");
        assert_eq!(request.url(), "/data/currency.json");
        let mut data = b"404 not found".to_owned();
        let cursor = std::io::Cursor::new(&mut data);
        request
            .respond(Response::new(StatusCode(404), vec![], cursor, None, None))
            .expect("the response should go through");
    });
    let result = rink::config::download_to_file(
        &PathBuf::from("currency.json"),
        "http://127.0.0.1:3090/data/currency.json",
        Duration::from_millis(2000),
    );
    let result = result.expect_err("this should always fail");
    assert_eq!(
        result.to_string(),
        "Received status 404 while downloading http://127.0.0.1:3090/data/currency.json"
    );
    thread_handle.join().unwrap();
    drop(server);
}

#[test]
fn test_download_success() {
    let server = SERVER.lock().unwrap();
    let server2 = server.clone();

    let thread_handle = std::thread::spawn(move || {
        let request = server2.recv().expect("the request should not fail");
        assert_eq!(request.url(), "/data/currency.json");
        let mut data = include_bytes!("../../core/tests/currency.snapshot.json").to_owned();
        let cursor = std::io::Cursor::new(&mut data);
        request
            .respond(Response::new(StatusCode(200), vec![], cursor, None, None))
            .expect("the response should go through");
    });
    let result = rink::config::download_to_file(
        &PathBuf::from("currency.json"),
        "http://127.0.0.1:3090/data/currency.json",
        Duration::from_millis(2000),
    );
    let mut result = result.expect("this should succeed");
    let mut string = String::new();
    result
        .read_to_string(&mut string)
        .expect("the file should exist");
    assert_eq!(
        string,
        include_str!("../../core/tests/currency.snapshot.json")
    );
    thread_handle.join().unwrap();
    drop(server);
}

#[test]
fn test_force_refresh_success() {
    let config = rink::config::Currency {
        enabled: true,
        behavior: rink::config::CurrencyBehavior::Default,
        fetch_on_startup: false,
        endpoint: "http://127.0.0.1:3090/data/currency.json".to_owned(),
        cache_duration: Duration::ZERO,
        timeout: Duration::from_millis(2000),
    };

    let server = SERVER.lock().unwrap();
    let server2 = server.clone();

    let thread_handle = std::thread::spawn(move || {
        let request = server2.recv().expect("the request should not fail");
        assert_eq!(request.url(), "/data/currency.json");
        let mut data = include_bytes!("../../core/tests/currency.snapshot.json").to_owned();
        let cursor = std::io::Cursor::new(&mut data);
        request
            .respond(Response::new(StatusCode(200), vec![], cursor, None, None))
            .expect("the response should go through");
    });
    let result = rink::config::force_refresh_currency(&config);
    let result = result.expect("this should succeed");
    assert!(result.starts_with("Fetched 6599 byte currency file after "));
    thread_handle.join().unwrap();
    drop(server);
}

#[test]
fn test_force_refresh_timeout() {
    let config = rink::config::Currency {
        enabled: true,
        behavior: rink::config::CurrencyBehavior::Default,
        fetch_on_startup: false,
        endpoint: "http://127.0.0.1:3090/data/currency.json".to_owned(),
        cache_duration: Duration::ZERO,
        timeout: Duration::from_millis(5),
    };

    let server = SERVER.lock().unwrap();
    let server2 = server.clone();

    let thread_handle = std::thread::spawn(move || {
        let request = server2.recv().expect("the request should not fail");
        assert_eq!(request.url(), "/data/currency.json");
        std::thread::sleep(Duration::from_millis(100));
    });
    let result = rink::config::force_refresh_currency(&config);
    let result = result.expect_err("this should timeout");
    assert_eq!(result.to_string(), "Fetching currency data failed");
    thread_handle.join().unwrap();
    drop(server);
}

#[test]
fn test_run_with_currency() {
    let server = SERVER.lock().unwrap();
    let server2 = server.clone();

    // first, make sure the app runs
    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_sandboxed_local_server.toml")
        .write_stdin("3 feet to meters\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(predicate::str::starts_with(
            "0.9144 meter (length)\nFinished in ",
        ));

    let thread_handle = std::thread::spawn(move || {
        let request = server2.recv().expect("the request should not fail");
        assert_eq!(request.url(), "/data/currency.json");
        let mut data = include_bytes!("../../core/tests/currency.snapshot.json").to_owned();
        let cursor = std::io::Cursor::new(&mut data);
        request
            .respond(Response::new(StatusCode(200), vec![], cursor, None, None))
            .expect("the response should go through");
    });

    let mut cmd = Command::cargo_bin("rink").unwrap();
    cmd.arg("-c")
        .arg("tests/config_sandboxed_local_server.toml")
        .write_stdin("$\ny\n")
        .env("NO_COLOR", "true")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "\nDefinition: USD = (1 / 1.0852) EUR = approx. 921.4891 millieuro (money; EUR). Sourced from European Central Bank. Current as of 2024-05-31.\nFinished in ",
        ));

    thread_handle.join().unwrap();
    drop(server);
}
