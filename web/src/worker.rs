use libc;
use std::io::Error;
use ipc_channel::ipc::{IpcOneShotServer, IpcSender};
use std::process::{Command, Stdio};
use std::env;
use rink;
use std::os::unix::process::ExitStatusExt;

pub fn worker(server_name: &str, query: &str) -> ! {
    let tx = IpcSender::connect(server_name.to_owned()).unwrap();

    tx.send("".to_owned()).unwrap();

    unsafe {
        let limit = libc::rlimit {
            // 100 megabytes
            rlim_cur: 100_000_000,
            rlim_max: 100_000_000,
        };
        let res = libc::setrlimit(libc::RLIMIT_AS, &limit);
        if res == -1 {
            panic!("Setrlimit RLIMIT_AS failed: {}", Error::last_os_error())
        }
        let limit = libc::rlimit {
            // 15 seconds
            rlim_cur: 15,
            rlim_max: 15
        };
        let res = libc::setrlimit(libc::RLIMIT_CPU, &limit);
        if res == -1 {
            panic!("Setrlimit RLIMIT_AS failed: {}", Error::last_os_error())
        }
    }

    let mut ctx = rink::load().unwrap();
    ctx.short_output = true;
    let reply = match rink::one_line(&mut ctx, query) {
        Ok(v) => v,
        Err(e) => e
    };
    tx.send(reply).unwrap();

    ::std::process::exit(0)
}

pub fn eval(query: &str) -> String {
    let (server, server_name) = IpcOneShotServer::new().unwrap();

    let res = Command::new(env::current_exe().unwrap())
        .arg("--sandbox")
        .arg(server_name)
        .arg(query)
        .stdin(Stdio::null())
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .env("RUST_BACKTRACE", "1")
        .spawn()
        .map_err(|x| format!("{}", x));
    let child = match res {
        Ok(s) => s,
        Err(e) => return format!("Failed to run sandbox: {}", e)
    };
    let (rx, _) = server.accept().unwrap();

    match rx.recv() {
        Ok(s) => s,
        Err(e) => {
            let output = match child.wait_with_output() {
                Ok(v) => v,
                Err(e) => return format!("{}", e)
            };
            match output.status.signal() {
                Some(libc::SIGXCPU) => return format!("Calculation went over time limit"),
                _ => ()
            };
            format!(
                "Receiving reply from sandbox failed: {}\n\
                 Signal: {}\n\
                 Sandbox stdout: {}\n\
                 Sandbox stderr: {}",
                e,
                output.status.signal().map(|x| format!("{}", x)).unwrap_or("None".to_owned()),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            )
        }
    }
}
