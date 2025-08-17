// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::io::{ErrorKind, PipeWriter};
use std::marker::PhantomData;
use std::process::Stdio;
use std::sync::mpsc::{sync_channel, Receiver, RecvTimeoutError, SyncSender};
use std::thread::JoinHandle;

use crate::child_guard::ChildGuard;
use crate::error::ErrorResponse;
use crate::frame::Frame;
use crate::message::Message;
use crate::reader_thread::reader_thread;
use crate::response::Response;
use crate::service::{HandshakeResponse, Service};
use crate::Error;

#[derive(Debug)]
struct Worker {
    process: ChildGuard,
    reader_thread: JoinHandle<()>,
    parent_writer: PipeWriter,
}

/// A handle to the child process.
#[derive(Debug)]
pub struct Sandbox<S>
where
    S: Service,
{
    worker: Option<Worker>,
    config: S::Config,
    send_message: SyncSender<Message<S::Res>>,
    recv_message: Receiver<Message<S::Res>>,
    terminated: bool,
    _phantom: PhantomData<S>,
}

impl<S> Sandbox<S>
where
    S: Service,
{
    /// Creates a new sandbox. This will start up an asynchronous task
    /// that creates the child process.
    pub fn new(config: S::Config) -> Result<Sandbox<S>, Error> {
        let (send_message, recv_message) = sync_channel(10);

        let send_ctrlc = send_message.clone();
        let _ = ctrlc::set_handler(move || send_ctrlc.send(Message::Interrupt).unwrap());

        Ok(Sandbox {
            worker: None,
            config,
            send_message,
            recv_message,
            terminated: false,
            _phantom: PhantomData,
        })
    }

    /// Kill the child process without restarting it.
    pub fn terminate(&mut self) -> Result<(), Error> {
        self.terminated = true;
        self.stop()
    }

    /// Stops the child process if it is running, then starts it.
    pub fn restart(&mut self) -> Result<(), Error> {
        self.stop()?;

        let program = S::program()
            .or_else(|| std::env::current_exe().ok())
            .expect("Couldn't find executable");
        let args = S::args(&self.config);

        let (child_reader, mut parent_writer) = std::io::pipe()?;
        let (mut parent_reader, child_writer) = std::io::pipe()?;

        let process = ChildGuard(
            std::process::Command::new(program)
                .args(args)
                .stderr(Stdio::inherit())
                .stdin(child_reader)
                .stdout(child_writer)
                .spawn()
                .map_err(Error::InitFailure)?,
        );

        let mut frame = Frame::new();
        frame.write_sync(&mut parent_writer, &self.config)?;
        let response = frame.read_sync::<HandshakeResponse, _>(&mut parent_reader)?;
        response.result.map_err(Error::HandshakeFailure)?;

        let send_message = self.send_message.clone();
        let reader_thread = std::thread::spawn(move || reader_thread(parent_reader, send_message));

        self.worker = Some(Worker {
            process,
            reader_thread,
            parent_writer,
        });

        Ok(())
    }

    /// Kills the child process
    pub fn stop(&mut self) -> Result<(), Error> {
        if let Some(mut worker) = self.worker.take() {
            worker.process.kill()?;
            let _ = worker.reader_thread.join();
        }
        Ok(())
    }

    /// Pass a query to the child process and return a response once it
    /// finishes.
    ///
    /// If the child process crashes or is killed during execution, it
    /// will be automatically restarted.
    ///
    /// # Panics
    ///
    /// Panics if called after [`Sandbox::terminate`].
    pub fn execute(&mut self, req: S::Req) -> Result<Response<S::Res>, Error> {
        if self.terminated {
            panic!("Sandbox::execute() called after terminated");
        }

        // Flush any existing messages, because they are stale.
        for _ in self.recv_message.try_iter() {
            // do nothing
        }

        if self.worker.is_none() {
            self.restart()?;
        }

        let mut try_again = false;
        let mut frame = Frame::new();
        if let Some(ref mut worker) = self.worker {
            match frame.write_sync(&mut worker.parent_writer, &req) {
                Err(Error::WriteFailed(err)) if err.kind() == ErrorKind::BrokenPipe => {
                    try_again = true;
                }
                Err(err) => return Err(err),
                _ => (),
            }
        } else {
            return Err(Error::Crashed);
        }

        // If there was a broken pipe error, it means the child
        // process was in a bad state. So restart it.
        if try_again {
            self.restart()?;
            // flush messages
            for _ in self.recv_message.try_iter() {}
            let worker = self.worker.as_mut().ok_or(Error::Crashed)?;
            frame.write_sync(&mut worker.parent_writer, &req)?;
        }
        let timeout = S::timeout(&self.config);

        let result = self.recv_message.recv_timeout(timeout);
        match result {
            Ok(Message::Response(res)) => Ok(res),
            Ok(Message::ErrorResponse(ErrorResponse::Panic(message))) => Err(Error::Panic(message)),
            Ok(Message::Interrupt) => {
                self.stop()?;
                Err(Error::Interrupted)
            }
            Ok(Message::RecvError(Error::ReadFailed(err)))
                if err.kind() == ErrorKind::UnexpectedEof =>
            {
                self.stop()?;
                Err(Error::Crashed)
            }
            Ok(Message::RecvError(err)) => Err(err),
            Err(RecvTimeoutError::Timeout) => {
                self.stop()?;
                Err(Error::Timeout(timeout))
            }
            Err(RecvTimeoutError::Disconnected) => {
                self.stop()?;
                Err(Error::Disconnected)
            }
        }
    }
}
