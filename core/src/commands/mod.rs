mod factorize;
mod search;

pub use factorize::{factorize, Factors};
pub use search::{search, SearchResult};

pub(crate) use search::search_impl;
