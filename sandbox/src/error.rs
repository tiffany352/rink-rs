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
    /// Panic
    Panic(PanicInfo),
    /// Timed out after {0:?}
    Timeout(Duration),
    /// Child process crashed
    Crashed,
    /// Interrupted (Ctrl+C)
    Interrupted,
    /// Read channel disconnected (other end hung up)
    Disconnected,

    /// When starting the child process fails
    HandshakeFailure(SerializableError),
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Panic(panic_info) => write!(f, "{}", panic_info),
            Error::Timeout(duration) => write!(f, "Timed out after {duration:?}"),
            Error::Crashed => write!(f, "Subprocess crashed"),
            Error::Interrupted => write!(f, "Interrupted"),
            Error::Disconnected => write!(f, "Disconnected"),

            Error::HandshakeFailure(_) => {
                write!(f, "Failed to start communicating with subprocess")
            }
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

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Error::HandshakeFailure(error) => Some(error),
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

#[derive(Serialize, Deserialize)]
pub struct SerializableError {
    display: String,
    debug: String,
    source: Option<Box<SerializableError>>,
}

impl fmt::Display for SerializableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.display)
    }
}

impl fmt::Debug for SerializableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug)
    }
}

impl StdError for SerializableError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        if let Some(ref source) = self.source {
            Some(source)
        } else {
            None
        }
    }
}

impl SerializableError {
    pub fn new<E>(error: E) -> Self
    where
        E: StdError,
    {
        SerializableError {
            display: format!("{}", error),
            debug: format!("{:?}", error),
            source: if let Some(source) = error.source() {
                Some(Box::new(SerializableError::new(source)))
            } else {
                None
            },
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub(crate) enum ErrorResponse {
    Panic(PanicInfo),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct PanicInfo {
    pub(crate) message: Option<String>,
    pub(crate) location: Option<LocationInfo>,
    pub(crate) backtrace: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct LocationInfo {
    pub(crate) file: String,
    pub(crate) line: u32,
}

const BACKTRACE_OMITTED: &'static str = "

Backtrace omitted.
Run with RUST_BACKTRACE=1 environment variable to display it.
Run with RUST_BACKTRACE=full to include source snippets.";

impl fmt::Display for PanicInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "The subprocess panicked (crashed).")?;
        if let Some(ref message) = self.message {
            write!(f, "\nMessage: {}", message)?;
        }
        if let Some(ref location) = self.location {
            write!(f, "\nLocation: {}:{}", location.file, location.line)?;
        }
        if let Some(ref backtrace) = self.backtrace {
            write!(f, "\n\nBacktrace:\n{backtrace}")?;
        } else {
            write!(f, "{BACKTRACE_OMITTED}")?;
        }
        Ok(())
    }
}
