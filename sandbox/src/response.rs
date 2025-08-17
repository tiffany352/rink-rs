use serde_derive::{Deserialize, Serialize};
use std::time::Duration;

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
}

impl<Data> Response<Data> {
    /// Replaces the [`Response::result`] with a new value.
    pub fn replace<New>(self, result: New) -> Response<New> {
        let Response {
            memory_used,
            time_taken,
            ..
        } = self;
        Response {
            result,
            memory_used,
            time_taken,
        }
    }

    /// Replaces the [`Response::result`] with the value returned by func.
    pub fn map<New>(self, func: impl FnOnce(Data) -> New) -> Response<New> {
        let Response {
            result,
            memory_used,
            time_taken,
        } = self;
        Response {
            result: func(result),
            memory_used,
            time_taken,
        }
    }
}
