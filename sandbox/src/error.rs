use async_std::channel::RecvError;
use displaydoc::Display;
use serde_derive::{Deserialize, Serialize};
use std::{io::Error as IoError, time::Duration};
use thiserror::Error;

/// All of the errors that can result while managing the child process.
#[derive(Error, Display, Debug)]
#[non_exhaustive]
pub enum Error {
    /// IO error
    Io(#[source] IoError),
    /// {0}
    Recv(RecvError),
    /// Failed to send {0}
    Send(&'static str),
    /// Timed out after {0:?}
    Timeout(Duration),
    /// Bincode
    Bincode(#[source] bincode::Error),
    /// Panic: {0}
    Panic(String),
    /// Failed to start child process
    InitFailure(#[source] IoError),
    /// Failed to read from child
    ReadFailed(#[source] IoError),
    /// Failed to write to child
    WriteFailed(#[source] IoError),
    /// Failed to start child process: {0}
    HandshakeFailure(String),
    /// Child process crashed
    Crashed,
    /// Interrupted
    Interrupted,
}

impl From<IoError> for Error {
    fn from(err: IoError) -> Self {
        Error::Io(err)
    }
}

impl From<bincode::Error> for Error {
    fn from(err: bincode::Error) -> Self {
        Error::Bincode(err)
    }
}

impl From<RecvError> for Error {
    fn from(err: RecvError) -> Self {
        Error::Recv(err)
    }
}

impl From<ErrorResponse> for Error {
    fn from(err: ErrorResponse) -> Self {
        match err {
            ErrorResponse::Panic(message) => Error::Panic(message),
        }
    }
}

impl From<Error> for IoError {
    fn from(err: Error) -> IoError {
        IoError::new(std::io::ErrorKind::Other, format!("{}", err))
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) enum ErrorResponse {
    Panic(String),
}
