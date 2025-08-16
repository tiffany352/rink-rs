// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::Error;
use serde::{de::DeserializeOwned, Serialize};
use std::io::{Read, Write};

pub(crate) struct Frame {
    buf: Vec<u8>,
}

impl Frame {
    pub(crate) fn new() -> Frame {
        Frame { buf: vec![] }
    }

    pub(crate) fn read_sync<V, R>(&mut self, reader: &mut R) -> Result<V, Error>
    where
        V: DeserializeOwned,
        R: Read,
    {
        let mut len = [0, 0, 0, 0];
        Read::read_exact(reader, &mut len).map_err(Error::ReadFailed)?;
        let len = u32::from_ne_bytes(len);

        self.buf.resize(len as usize, 0);
        Read::read_exact(reader, &mut self.buf).map_err(Error::ReadFailed)?;

        let value = bincode::deserialize(&self.buf)?;

        Ok(value)
    }

    pub(crate) fn write_sync<V, W>(&mut self, writer: &mut W, value: &V) -> Result<(), Error>
    where
        W: Write,
        V: Serialize,
    {
        let bytes = bincode::serialize(value)?;
        let len = u32::to_ne_bytes(bytes.len() as u32);
        writer.write_all(&len).map_err(Error::WriteFailed)?;
        writer.write_all(&bytes).map_err(Error::WriteFailed)?;
        writer.flush().map_err(Error::WriteFailed)?;

        Ok(())
    }
}
