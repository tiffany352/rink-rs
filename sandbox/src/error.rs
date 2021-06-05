use displaydoc::Display;
use serde_derive::{Deserialize, Serialize};
use std::{io::Error as IoError, time::Duration};
use thiserror::Error;

#[derive(Error, Display, Debug)]
pub enum Error {
    /// IO error
    Io(#[source] IoError),
    /// Timed out after {0:?}
    Timeout(Duration),
    /// Bincode
    Bincode(#[source] bincode::Error),
    /// Panic: {0}
    Panic(String),
    /// Failed to start child process: {0}
    InitFailure(String),
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

impl From<ErrorResponse> for Error {
    fn from(err: ErrorResponse) -> Self {
        match err {
            ErrorResponse::Panic(message) => Error::Panic(message),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) enum ErrorResponse {
    Panic(String),
}
