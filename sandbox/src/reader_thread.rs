use std::io::{ErrorKind, PipeReader};
use std::sync::mpsc::SyncSender;

use serde::de::DeserializeOwned;

use crate::error::ErrorResponse;
use crate::frame::Frame;
use crate::message::Message;
use crate::response::Response;
use crate::Error;

pub fn reader_thread<Res>(mut input: PipeReader, output: SyncSender<Message<Res>>)
where
    Res: DeserializeOwned,
{
    let mut frame = Frame::new();
    loop {
        let mut stop = false;
        let message =
            match frame.read_sync::<Result<Response<Res>, ErrorResponse>, PipeReader>(&mut input) {
                Ok(Ok(message)) => Message::Response(message),
                Ok(Err(err)) => Message::ErrorResponse(err),
                Err(Error::ReadFailed(err)) if err.kind() == ErrorKind::UnexpectedEof => {
                    stop = true;
                    Message::RecvError(Error::ReadFailed(err))
                }
                Err(err) => Message::RecvError(err),
            };
        // Only fails if channel is broken, meaning app is shutting down (or crashing).
        if output.send(message).is_err() || stop {
            break;
        }
    }
}
