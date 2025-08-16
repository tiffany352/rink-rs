// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! This crate provides a way to run code in a context with limited
//! memory, execution time, and the ability to be interrupted.
//!
//! For an example, see `examples/add_two.rs`.

#![deny(missing_docs)]

mod alloc;
mod become_child;
mod child_guard;
mod error;
mod frame;
mod message;
mod reader_thread;
mod response;
mod sandbox;
mod service;

pub use alloc::Alloc;
pub use become_child::become_child;
pub use error::Error;
pub use response::Response;
pub use sandbox::Sandbox;
pub use service::Service;
