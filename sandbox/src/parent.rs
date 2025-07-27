// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use async_ctrlc::CtrlC;
use smol::channel::bounded;
use smol::channel::Receiver;
use smol::channel::Sender;
use smol::future::FutureExt;
use smol::process::Command;
use smol::stream::StreamExt;
use smol::Timer;
use std::cell::Cell;
use std::env;
use std::fmt;
use std::io::ErrorKind;
use std::marker::PhantomData;
use std::pin::Pin;
use std::process::Stdio;

use crate::frame::Frame;
use crate::HandshakeResponse;
use crate::MessageRequest;
use crate::MessageResponse;
use crate::Service;
use crate::{Error, Response};

/// A handle to the child process.
pub struct Sandbox<S>
where
    S: Service,
{
    send_request: Sender<S::Req>,
    recv_response: Receiver<Result<Response<S::Res>, Error>>,
    join_handle: Cell<Option<smol::Task<Result<(), Error>>>>,
    terminated: bool,
    _phantom: PhantomData<S>,
}

impl<S> Sandbox<S>
where
    S: Service,
{
    async fn run_task(
        config: S::Config,
        recv_request: Receiver<S::Req>,
        send_response: Sender<Result<Response<S::Res>, Error>>,
    ) -> Result<(), Error> {
        let mut frame = Frame::new();
        let mut ctrlc = CtrlC::new().unwrap();

        loop {
            let program = S::program()
                .or_else(|| env::current_exe().ok())
                .expect("Couldn't find executable");
            let args = S::args(&config);

            let mut process = Command::new(program)
                .args(args)
                .stderr(Stdio::inherit())
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .kill_on_drop(true)
                .spawn()
                .map_err(Error::InitFailure)?;
            let mut stdin = process.stdin.take().unwrap();
            let mut stdout = process.stdout.take().unwrap();

            frame.write_async(Pin::new(&mut stdin), &config).await?;

            let response = frame
                .read_async::<HandshakeResponse, _>(Pin::new(&mut stdout))
                .await?;
            response.result.map_err(Error::HandshakeFailure)?;

            loop {
                let request = recv_request.recv().await?;

                frame
                    .write_async::<MessageRequest<S>, _>(Pin::new(&mut stdin), &request)
                    .await?;

                let interrupt = async {
                    ctrlc.next().await;
                    Err(Error::Interrupted)
                };

                let next_frame = async {
                    frame
                        .read_async::<MessageResponse<S>, _>(Pin::new(&mut stdout))
                        .await
                };

                let exec_time = S::timeout(&config);
                let timeout = async {
                    Timer::after(exec_time).await;
                    Err(Error::Timeout(exec_time))
                };
                let pending = interrupt.race(next_frame).race(timeout);

                let mut break_out = false;
                let response = match pending.await {
                    Ok(Response {
                        result: Ok(result),
                        memory_used,
                        time_taken,
                        stdout,
                    }) => Ok(Response {
                        result,
                        memory_used,
                        time_taken,
                        stdout,
                    }),
                    Ok(Response {
                        result: Err(err), ..
                    }) => Err(err.into()),
                    Err(Error::ReadFailed(err)) if err.kind() == ErrorKind::UnexpectedEof => {
                        break_out = true;
                        Err(Error::Crashed)
                    }
                    Err(Error::Interrupted) => {
                        break_out = true;
                        Err(Error::Interrupted)
                    }
                    Err(Error::Timeout(timeout)) => {
                        break_out = true;
                        Err(Error::Timeout(timeout))
                    }
                    Err(err) => Err(err),
                };

                send_response
                    .send(response)
                    .await
                    .map_err(|_| Error::Send("response to caller"))?;

                if break_out {
                    match process.kill() {
                        Ok(()) => {}
                        // This happens when the process is already dead.
                        Err(ref err)
                            if err.kind() == ErrorKind::PermissionDenied
                                || err.kind() == ErrorKind::InvalidInput => {}
                        Err(err) => return Err(err.into()),
                    };
                    break;
                }
            }
        }
    }

    /// Creates a new sandbox. This will start up an asynchronous task
    /// that creates the child process.
    pub async fn new(config: S::Config) -> Result<Sandbox<S>, Error> {
        let (send_request, recv_request) = bounded(1);
        let (send_response, recv_response) = bounded(1);
        let join_handle = smol::spawn(Self::run_task(config, recv_request, send_response));

        Ok(Sandbox {
            send_request,
            recv_response,
            join_handle: Cell::new(Some(join_handle)),
            terminated: false,
            _phantom: PhantomData,
        })
    }

    /// Kill the child process without restarting it.
    pub async fn terminate(&self) -> Result<(), Error> {
        if let Some(res) = self.join_handle.take().unwrap().cancel().await {
            res
        } else {
            Ok(())
        }
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
    pub async fn execute(&self, req: S::Req) -> Result<Response<S::Res>, Error> {
        if self.terminated {
            panic!("Sandbox::execute() called after terminated");
        }

        self.send_request
            .send(req)
            .await
            .map_err(|_| Error::Send("request to child"))?;

        self.recv_response.recv().await?
    }
}

impl<S> fmt::Debug for Sandbox<S>
where
    S: Service,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let handle = self.join_handle.take();
        let res = f
            .debug_struct("Sandbox")
            .field("send_request", &self.send_request)
            .field("recv_response", &self.recv_response)
            .field("terminated", &self.terminated)
            .field("join_handle", &handle)
            .finish();
        self.join_handle.set(handle);
        res
    }
}

impl<S> Drop for Sandbox<S>
where
    S: Service,
{
    /// When Sandbox is dropped, the task managing the child process is
    /// cancelled, and the child process is killed.
    fn drop(&mut self) {
        let handle = self.join_handle.take();
        if let Some(handle) = handle {
            let _ = handle.cancel();
        }
    }
}
