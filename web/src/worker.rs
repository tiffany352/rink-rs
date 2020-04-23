use libc;
use ipc_channel::ipc::{IpcOneShotServer, IpcSender, IpcReceiver};
use std::process::{Command, Stdio};
use std::env;
use rink;
use rink::reply::{QueryReply, QueryError};
use std::os::unix::process::ExitStatusExt;
use std::io;
use rustc_serialize;
use serde_json;
use serde::ser::{Serialize, Serializer};

#[derive(Debug)]
pub enum Error {
    Rink(QueryError),
    Time,
    Memory,
    Generic(String),
}

impl Serialize for Error {
    fn serialize<S>(
        &self, ser: &mut S
    ) -> Result<(), S::Error> where S: Serializer {
        match *self {
            Error::Rink(ref e) =>
                ser.serialize_newtype_variant("Error", 0, "Rink", e),
            Error::Time =>
                ser.serialize_newtype_variant("Error", 1, "Time", true),
            Error::Memory =>
                ser.serialize_newtype_variant("Error", 2, "Memory", true),
            Error::Generic(ref e) =>
                ser.serialize_newtype_variant("Error", 3, "Generic", e),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Generic(err.to_string())
    }
}

pub fn worker(server_name: &str, query: &str) -> ! {
    let tx = IpcSender::connect(server_name.to_owned()).unwrap();

    tx.send(Err(QueryError::Generic("".to_owned()))).unwrap();

    unsafe {
        let limit = libc::rlimit {
            // 100 megabytes
            rlim_cur: 100_000_000,
            rlim_max: 100_000_000,
        };
        let res = libc::setrlimit(libc::RLIMIT_AS, &limit);
        if res == -1 {
            panic!("Setrlimit RLIMIT_AS failed: {}", io::Error::last_os_error())
        }
        let limit = libc::rlimit {
            // 15 seconds
            rlim_cur: 15,
            rlim_max: 15
        };
        let res = libc::setrlimit(libc::RLIMIT_CPU, &limit);
        if res == -1 {
            panic!("Setrlimit RLIMIT_AS failed: {}", io::Error::last_os_error())
        }
    }

    let mut ctx = rink::load().unwrap();
    ctx.short_output = true;
    let mut iter = rink::text_query::TokenIterator::new(query).peekable();
    let expr = rink::text_query::parse_query(&mut iter);
    let reply = ctx.eval_outer(&expr);
    tx.send(reply).unwrap();

    ::std::process::exit(0)
}

pub fn eval(query: &str) -> Result<QueryReply, Error> {
    let (server, server_name) = IpcOneShotServer::new().unwrap();

    let res = Command::new(env::current_exe().unwrap())
        .arg("--sandbox")
        .arg(server_name)
        .arg(query)
        .stdin(Stdio::null())
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .env("RUST_BACKTRACE", "1")
        .spawn();
    let child = try!(res);
    let (rx, _) = server.accept().unwrap();
    let rx: IpcReceiver<Result<QueryReply, QueryError>> = rx;

    match rx.recv() {
        Ok(s) => {
            try!(child.wait_with_output());
            s.map_err(Error::Rink)
        },
        Err(e) => {
            let output = try!(child.wait_with_output());
            match output.status.signal() {
                Some(libc::SIGXCPU) => return Err(Error::Time),
                // SIGABRT doesn't necessarily mean OOM, but GMP will raise it when it happens
                Some(libc::SIGABRT) => return Err(Error::Memory),
                _ => ()
            };
            Err(Error::Generic(format!(
                "Receiving reply from sandbox failed: {}\n\
                 Signal: {}\n\
                 Sandbox stdout: {}\n\
                 Sandbox stderr: {}",
                e,
                output.status.signal().map(|x| x.to_string()).unwrap_or("None".to_owned()),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            )))
        }
    }
}

pub fn eval_text(query: &str) -> String {
    match eval(query) {
        Ok(v) => v.to_string(),
        Err(Error::Generic(e)) => e.to_string(),
        Err(Error::Memory) => "Calculation ran out of memory".to_string(),
        Err(Error::Time) => "Calculation timed out".to_string(),
        Err(Error::Rink(e)) => e.to_string(),
    }
}

pub fn eval_json(query: &str) -> rustc_serialize::json::Json {
    let res = eval(query);
    rustc_serialize::json::Json::from_str(&serde_json::ser::to_string(&res).unwrap()).unwrap()
}
