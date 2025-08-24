use serde::de::DeserializeOwned;
use std::ffi::OsString;
use std::io::Error as IoError;
use std::panic::RefUnwindSafe;
use std::path::PathBuf;
use std::time::Duration;

use crate::error::{ErrorResponse, SerializableError};
use crate::response::Response;

/// In order to sandbox some logic, there needs to be an implementation
/// of this trait for it.
pub trait Service: Sized + RefUnwindSafe + 'static {
    /// The request, sent from the parent to the child.
    type Req: serde::Serialize + DeserializeOwned + Send + Sync;
    /// The response, sent from the child to the parent after the
    /// request has been processed.
    type Res: serde::Serialize + DeserializeOwned + Send + Sync;
    /// The config is passed to the child on startup.
    type Config: serde::Serialize + DeserializeOwned + Clone + 'static + Send + Sync;

    /// Returns the path to the executable, or None if the current
    /// executable should be used.
    fn program() -> Option<PathBuf> {
        None
    }

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
pub(crate) type HandshakeResponse = Response<Result<(), SerializableError>>;
