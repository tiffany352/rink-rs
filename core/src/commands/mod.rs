mod factorize;
mod search;

pub(crate) use factorize::fast_decompose;
pub use factorize::{factorize, Factors};
pub(crate) use search::search_impl;
pub use search::{search, SearchResult};
