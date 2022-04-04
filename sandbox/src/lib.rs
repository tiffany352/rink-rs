// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! This crate provides a way to run code in a context with limited
//! memory, execution time, and the ability to be interrupted.
//!
//! For an example, see `examples/add_two.rs`.

#![deny(missing_docs)]

use serde::de::DeserializeOwned;
use serde_derive::{Deserialize, Serialize};
use std::{ffi::OsString, io::Error as IoError, panic::RefUnwindSafe, time::Duration};
use thiserror::Error;

mod alloc;
mod child;
mod error;
mod frame;
mod parent;

pub use alloc::Alloc;
pub use child::become_child;
pub use error::Error;
pub(crate) use error::ErrorResponse;
pub use parent::Sandbox;

/// Contains response data, as well as statistics like memory usage.
#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
#[non_exhaustive]
pub struct Response<Data> {
    /// The response from the service.
    pub result: Data,
    /// The peak memory usage while servicing the query, in bytes.
    pub memory_used: usize,
    /// The amount of time taken to service the query.
    pub time_taken: Duration,
    /// Logs collected while servicing the query.
    pub stdout: String,
}

impl<Data> Response<Data> {
    /// Replaces the [`Response::result`] with a new value.
    pub fn replace<New>(self, result: New) -> Response<New> {
        let Response {
            memory_used,
            time_taken,
            stdout,
            ..
        } = self;
        Response {
            result,
            memory_used,
            time_taken,
            stdout,
        }
    }

    /// Replaces the [`Response::result`] with the value returned by func.
    pub fn map<New>(self, func: impl FnOnce(Data) -> New) -> Response<New> {
        let Response {
            result,
            memory_used,
            time_taken,
            stdout,
        } = self;
        Response {
            result: func(result),
            memory_used,
            time_taken,
            stdout,
        }
    }
}

/// In order to sandbox some logic, there needs to be an implementation
/// of this trait for it.
pub trait Service: Sized + RefUnwindSafe + 'static {
    /// The request, sent from the parent to the child.
    type Req: serde::Serialize + DeserializeOwned;
    /// The response, sent from the child to the parent after the
    /// request has been processed.
    type Res: serde::Serialize + DeserializeOwned;
    /// The config is passed to the child on startup.
    type Config: serde::Serialize + DeserializeOwned + Clone + 'static;

    /// When your app is passed these CLI flags, it should call
    /// [`become_child`].
    fn args(config: &Self::Config) -> Vec<OsString>;

    /// The amount of time that can be spent servicing queries.
    fn timeout(config: &Self::Config) -> Duration;

    /// Creates an instance of the service from the config.
    fn create(config: Self::Config) -> Result<Self, IoError>;

    /// Responds to one request, returning a result.
    fn handle(&self, request: Self::Req) -> Self::Res;
}

pub(crate) type MessageRequest<S> = <S as Service>::Req;
pub(crate) type MessageResponse<S> = Response<Result<<S as Service>::Res, ErrorResponse>>;
pub(crate) type HandshakeRequest<S> = <S as Service>::Config;
pub(crate) type HandshakeResponse = Response<Result<(), String>>;
