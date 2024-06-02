// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use async_std::channel::RecvError;
use core::fmt;
use serde_derive::{Deserialize, Serialize};
use std::{io::Error as IoError, time::Duration};
use thiserror::Error;

/// All of the errors that can result while managing the child process.
#[derive(Error, Debug)]
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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(_) => write!(f, "IO error"),
            Error::Recv(err) => write!(f, "{}", err),
            Error::Send(err) => write!(f, "Failed to send {err}"),
            Error::Timeout(err) => write!(f, "Timed out after {err:?}"),
            Error::Bincode(_) => write!(f, "Bincode"),
            Error::Panic(err) => write!(f, "Panic: {err}"),
            Error::InitFailure(_) => write!(f, "Failed to start child process"),
            Error::ReadFailed(_) => write!(f, "Failed to read from child"),
            Error::WriteFailed(_) => write!(f, "Failed to write to child"),
            Error::HandshakeFailure(err) => write!(f, "Failed to start child process: {err}"),
            Error::Crashed => write!(f, "Child process crashed"),
            Error::Interrupted => write!(f, "Interrupted"),
        }
    }
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
