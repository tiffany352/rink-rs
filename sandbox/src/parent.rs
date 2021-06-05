use async_ctrlc::CtrlC;
use async_std::channel::bounded;
use async_std::channel::Receiver;
use async_std::channel::Sender;
use async_std::task::spawn_local;
use async_std::task::JoinHandle;
use async_std::{
    future::timeout,
    prelude::{FutureExt, StreamExt},
    process::{Command, Stdio},
};
use std::cell::Cell;
use std::env;
use std::io::ErrorKind;
use std::marker::PhantomData;
use std::pin::Pin;

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
    join_handle: Cell<Option<JoinHandle<Result<(), Error>>>>,
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
            let program = env::current_exe().expect("Couldn't find current executable");
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
                let pending = timeout(exec_time, interrupt.race(next_frame));

                let mut break_out = false;
                let response = match pending.await {
                    Ok(Ok(Response {
                        result: Ok(result),
                        memory_used,
                        time_taken,
                        stdout,
                    })) => Ok(Response {
                        result,
                        memory_used,
                        time_taken,
                        stdout,
                    }),
                    Ok(Ok(Response {
                        result: Err(err), ..
                    })) => Err(err.into()),
                    Ok(Err(Error::ReadFailed(err))) if err.kind() == ErrorKind::UnexpectedEof => {
                        break_out = true;
                        Err(Error::Crashed)
                    }
                    Ok(Err(Error::Interrupted)) => {
                        break_out = true;
                        Err(Error::Interrupted)
                    }
                    Ok(Err(err)) => Err(err),
                    Err(_timeout) => {
                        break_out = true;
                        Err(Error::Timeout(exec_time))
                    }
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
        let join_handle = spawn_local(Self::run_task(config, recv_request, send_response));

        Ok(Sandbox {
            send_request,
            recv_response,
            join_handle: Cell::new(Some(join_handle)),
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

    /// Pass a query to the child process and return a response once it finishes.
    pub async fn execute(&self, req: S::Req) -> Result<Response<S::Res>, Error> {
        self.send_request
            .send(req)
            .await
            .map_err(|_| Error::Send("request to child"))?;

        self.recv_response.recv().await?
    }
}
