// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use rink_sandbox::Alloc;

pub use helper::RinkHelper;
pub mod config;
pub mod currency;
pub mod fmt;
pub mod helper;
pub mod repl;
pub mod runner;
pub mod service;
pub mod style_ser;

#[global_allocator]
pub(crate) static GLOBAL: Alloc = Alloc::new(usize::MAX);
