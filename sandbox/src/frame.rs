use crate::Error;
use async_std::io::{prelude::WriteExt, ReadExt};
use serde::{de::DeserializeOwned, Serialize};
use std::{
    io::{Read, Write},
    pin::Pin,
};

pub(crate) struct Frame {
    buf: Vec<u8>,
}

impl Frame {
    pub(crate) fn new() -> Frame {
        Frame { buf: vec![] }
    }

    pub(crate) async fn read_async<V, R>(&mut self, mut reader: Pin<&mut R>) -> Result<V, Error>
    where
        V: DeserializeOwned,
        R: ReadExt,
    {
        let mut len = [0, 0, 0, 0];
        reader.read_exact(&mut len).await?;
        let len = u32::from_ne_bytes(len);

        self.buf.resize(len as usize, 0);
        reader.read_exact(&mut self.buf).await?;

        let value = bincode::deserialize(&self.buf)?;

        Ok(value)
    }

    pub(crate) fn read_sync<V, R>(&mut self, reader: &mut R) -> Result<V, Error>
    where
        V: DeserializeOwned,
        R: Read,
    {
        let mut len = [0, 0, 0, 0];
        Read::read_exact(reader, &mut len)?;
        let len = u32::from_ne_bytes(len);

        self.buf.resize(len as usize, 0);
        Read::read_exact(reader, &mut self.buf)?;

        let value = bincode::deserialize(&self.buf)?;

        Ok(value)
    }

    pub(crate) async fn write_async<V, W>(
        &mut self,
        mut writer: Pin<&mut W>,
        value: &V,
    ) -> Result<(), Error>
    where
        W: WriteExt,
        V: Serialize,
    {
        let bytes = bincode::serialize(value)?;
        let len = u32::to_ne_bytes(bytes.len() as u32);
        writer.write_all(&len).await?;
        writer.write_all(&bytes).await?;
        writer.flush().await?;

        Ok(())
    }

    pub(crate) fn write_sync<V, W>(&mut self, writer: &mut W, value: &V) -> Result<(), Error>
    where
        W: Write,
        V: Serialize,
    {
        let bytes = bincode::serialize(value)?;
        let len = u32::to_ne_bytes(bytes.len() as u32);
        writer.write_all(&len)?;
        writer.write_all(&bytes)?;
        writer.flush()?;

        Ok(())
    }
}
