use async_std::io::{prelude::*, Split};
use async_std::{
    future::timeout,
    io::{BufReader, Error as IoError},
    process::{Child, ChildStdin, ChildStdout, Command, Stdio},
};
use displaydoc::Display;
use futures_util::StreamExt;
use rink_core::reply::{QueryError, QueryReply};
use serde_derive::{Deserialize, Serialize};
use std::io::ErrorKind;
use std::{env, time::Duration};
use thiserror::Error;

pub struct Sandbox {
    process: Child,
    stdin: ChildStdin,
    stdout: Split<BufReader<ChildStdout>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SandboxReply {
    pub result: QueryReply,
    pub memory_used: usize,
    pub time_taken: Duration,
}

#[derive(Display, Error, Debug)]
pub enum SandboxError {
    /// Query error: {0}
    Query(QueryError),
    /// IO error
    Io(#[source] IoError),
    /// Timed out after {0:?}
    Timeout(Duration),
    /// Serde
    Serde(#[source] serde_json::Error),
    /// Child process crashed
    Crashed,
}

impl From<QueryError> for SandboxError {
    fn from(err: QueryError) -> Self {
        SandboxError::Query(err)
    }
}

impl From<IoError> for SandboxError {
    fn from(err: IoError) -> Self {
        SandboxError::Io(err)
    }
}

impl From<serde_json::Error> for SandboxError {
    fn from(err: serde_json::Error) -> Self {
        SandboxError::Serde(err)
    }
}

impl Sandbox {
    pub fn start() -> Sandbox {
        let program = env::current_exe().expect("Couldn't find current executable");
        let mut process = Command::new(program)
            .arg("--service")
            .stderr(Stdio::inherit())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .expect("Couldn't spawn process");
        let stdin = process.stdin.take().unwrap();
        let stdout = BufReader::new(process.stdout.take().unwrap()).split(b'\0');

        Sandbox {
            process,
            stdin,
            stdout,
        }
    }

    fn is_dead(&mut self) -> bool {
        if let Ok(Some(_)) = self.process.try_status() {
            true
        } else {
            false
        }
    }

    pub fn kill(&mut self) -> Result<(), IoError> {
        self.process.kill()
    }

    pub fn restart_if_dead(&mut self) -> Result<(), IoError> {
        if self.is_dead() {
            *self = Self::start();
        }
        Ok(())
    }

    pub fn restart(&mut self) -> Result<(), IoError> {
        if let Err(err) = self.process.kill() {
            // PermissionDenied occurs if the process is already dead.
            if err.kind() != ErrorKind::PermissionDenied {
                return Err(err);
            }
        }
        *self = Self::start();
        Ok(())
    }

    pub async fn eval(
        &mut self,
        query: String,
        exec_time: Duration,
    ) -> Result<SandboxReply, SandboxError> {
        if self.is_dead() {
            *self = Self::start();
        }
        self.stdin.write(query.as_bytes()).await?;
        self.stdin.write(&[b'\0']).await?;
        let response = match timeout(exec_time, async { self.stdout.next().await }).await {
            Ok(None) => {
                self.restart()?;
                return Err(SandboxError::Crashed);
            }
            Ok(Some(response)) => response?,

            Err(_timeout) => {
                self.restart()?;
                return Err(SandboxError::Timeout(exec_time));
            }
        };
        let result = serde_json::from_slice::<Result<SandboxReply, QueryError>>(&response)?;
        Ok(result?)
    }
}
