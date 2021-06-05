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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Response<Data> {
    pub result: Data,
    pub memory_used: usize,
    pub time_taken: Duration,
    pub stdout: String,
}

pub trait Service: Sized + RefUnwindSafe {
    type Req: serde::Serialize + DeserializeOwned;
    type Res: serde::Serialize + DeserializeOwned;
    type Config: serde::Serialize + DeserializeOwned + Clone + 'static;

    fn args(config: &Self::Config) -> Vec<OsString>;
    fn create(config: Self::Config) -> Result<Self, IoError>;

    fn handle(&self, request: Self::Req) -> Self::Res;
}

pub(crate) type MessageRequest<S> = <S as Service>::Req;
pub(crate) type MessageResponse<S> = Response<Result<<S as Service>::Res, ErrorResponse>>;
pub(crate) type HandshakeRequest<S> = <S as Service>::Config;
pub(crate) type HandshakeResponse = Response<Result<(), String>>;
