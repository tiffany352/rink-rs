// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use core::fmt;
use serde_derive::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::io::Error as IoError;
use std::sync::mpsc::RecvError;
use std::time::Duration;

/// All of the errors that can result while managing the child process.
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    /// IO error
    Io(IoError),
    /// {0}
    Recv(RecvError),
    /// Failed to send {0}
    Send(&'static str),
    /// Timed out after {0:?}
    Timeout(Duration),
    /// Bincode
    Bincode(bincode::Error),
    /// Panic: {0}
    Panic(String),
    /// Failed to start child process
    InitFailure(IoError),
    /// Failed to read from child
    ReadFailed(IoError),
    /// Failed to write to child
    WriteFailed(IoError),
    /// Failed to call std::io::pipe()
    CreatePipeFailed(IoError),
    /// Failed to kill child process
    KillFailed(IoError),
    /// Failed to start child process: {0}
    HandshakeFailure(String),
    /// Child process crashed
    Crashed,
    /// Interrupted
    Interrupted,
    /// Read channel disconnected
    Disconnected,
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
            Error::CreatePipeFailed(_) => write!(f, "Failed to create pipe"),
            Error::KillFailed(_) => write!(f, "Failed to terminate subprocess"),
            Error::HandshakeFailure(err) => write!(f, "Failed to start subprocess: {err}"),
            Error::Crashed => write!(f, "Child process crashed"),
            Error::Interrupted => write!(f, "Interrupted"),
            Error::Disconnected => write!(f, "Disconnected"),
        }
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

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Error::Io(error) => Some(error),
            Error::Recv(error) => Some(error),
            Error::Bincode(error) => Some(error),
            Error::InitFailure(error) => Some(error),
            Error::ReadFailed(error) => Some(error),
            Error::WriteFailed(error) => Some(error),
            Error::CreatePipeFailed(error) => Some(error),
            Error::KillFailed(error) => Some(error),
            _ => None,
        }
    }

    fn cause(&self) -> Option<&dyn StdError> {
        self.source()
    }
}
