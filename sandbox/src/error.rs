// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use core::fmt;
use serde_derive::{Deserialize, Serialize};
use std::error::Error as StdError;
use std::io::Error as IoError;
use std::time::Duration;

/// All of the errors that can result while managing the child process.
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    /// Timed out after {0:?}
    Timeout(Duration),
    /// Panic: {0}
    Panic(String),
    /// When starting the child process fails
    HandshakeFailure(String),
    /// Child process crashed
    Crashed,
    /// Interrupted (Ctrl+C)
    Interrupted,
    /// Read channel disconnected (other end hung up)
    Disconnected,

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
    /// When writing a frame fails
    SerializeFailed(bincode::Error),
    /// When reading a frame fails
    DeserializeFailed(bincode::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Timeout(duration) => write!(f, "Timed out after {duration:?}"),
            Error::Panic(message) => write!(f, "Panic: {message}"),
            Error::HandshakeFailure(message) => write!(f, "Failed to start subprocess: {message}"),
            Error::Crashed => write!(f, "Subprocess crashed"),
            Error::Interrupted => write!(f, "Interrupted"),
            Error::Disconnected => write!(f, "Disconnected"),

            Error::InitFailure(_) => write!(f, "Failed to start subprocess"),
            Error::ReadFailed(_) => write!(f, "Failed to read from subprocess"),
            Error::WriteFailed(_) => write!(f, "Failed to write to subprocess"),
            Error::CreatePipeFailed(_) => write!(f, "Failed to create pipe"),
            Error::KillFailed(_) => write!(f, "Failed to terminate subprocess"),
            Error::SerializeFailed(_) => write!(f, "Failed to serialize message"),
            Error::DeserializeFailed(_) => write!(f, "Failed to deserialize message"),
        }
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
        IoError::new(std::io::ErrorKind::Other, err)
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) enum ErrorResponse {
    Panic(String),
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Error::InitFailure(error) => Some(error),
            Error::ReadFailed(error) => Some(error),
            Error::WriteFailed(error) => Some(error),
            Error::CreatePipeFailed(error) => Some(error),
            Error::KillFailed(error) => Some(error),
            Error::SerializeFailed(error) => Some(error),
            Error::DeserializeFailed(error) => Some(error),
            _ => None,
        }
    }

    fn cause(&self) -> Option<&dyn StdError> {
        self.source()
    }
}
