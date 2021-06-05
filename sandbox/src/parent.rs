use async_ctrlc::CtrlC;
use async_std::sync::Mutex;
use async_std::{
    future::timeout,
    io::Error as IoError,
    prelude::{FutureExt, StreamExt},
    process::{Child as ChildProcess, ChildStdin, ChildStdout, Command, Stdio},
};
use serde::Serialize;
use std::ffi::OsString;
use std::io::ErrorKind;
use std::marker::PhantomData;
use std::pin::Pin;
use std::{env, time::Duration};

use crate::frame::Frame;
use crate::HandshakeResponse;
use crate::MessageRequest;
use crate::MessageResponse;
use crate::Service;
use crate::{Error, Response};

struct ReadWrite {
    stdin: ChildStdin,
    stdout: ChildStdout,
}

struct Child {
    process: Mutex<ChildProcess>,
    readwrite: Mutex<Option<ReadWrite>>,
    startup: Mutex<Response<()>>,
}

impl Child {
    async fn handshake(
        config: &impl Serialize,
        mut stdin: &mut ChildStdin,
        mut stdout: &mut ChildStdout,
    ) -> Result<Response<()>, Error> {
        let mut frame = Frame::new();
        frame.write_async(Pin::new(&mut stdin), config).await?;

        let response = frame
            .read_async::<HandshakeResponse, _>(Pin::new(&mut stdout))
            .await?;
        response.result.map_err(Error::InitFailure)?;

        Ok(Response {
            result: (),
            time_taken: response.time_taken,
            memory_used: response.memory_used,
            stdout: response.stdout,
        })
    }

    async fn new(config: &impl Serialize, args: Vec<OsString>) -> Result<Child, Error> {
        let program = env::current_exe().expect("Couldn't find current executable");
        let mut process = Command::new(program)
            .args(args)
            .stderr(Stdio::inherit())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()?;
        let mut stdin = process.stdin.take().unwrap();
        let mut stdout = process.stdout.take().unwrap();

        let startup = Self::handshake(config, &mut stdin, &mut stdout).await?;

        Ok(Child {
            process: Mutex::new(process),
            readwrite: Mutex::new(Some(ReadWrite { stdin, stdout })),
            startup: Mutex::new(startup),
        })
    }

    async fn start(&self, config: &impl Serialize, args: Vec<OsString>) -> Result<(), Error> {
        let program = env::current_exe().expect("Couldn't find current executable");
        let mut process = Command::new(program)
            .args(args)
            .stderr(Stdio::inherit())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()?;
        let mut stdin = process.stdin.take().unwrap();
        let mut stdout = process.stdout.take().unwrap();

        let startup = Self::handshake(config, &mut stdin, &mut stdout).await?;

        *self.process.lock().await = process;
        *self.readwrite.lock().await = Some(ReadWrite { stdin, stdout });
        *self.startup.lock().await = startup;

        Ok(())
    }

    async fn is_dead(&self) -> bool {
        if let Ok(Some(_)) = self.process.lock().await.try_status() {
            true
        } else {
            false
        }
    }

    pub async fn kill(&self) -> Result<(), IoError> {
        match self.process.lock().await.kill() {
            Ok(()) => Ok(()),
            // PermissionDenied occurs if the process is already dead.
            Err(ref err) if err.kind() == ErrorKind::PermissionDenied => Ok(()),
            Err(err) => Err(err),
        }
    }
}

/// A handle to the child process.
pub struct Sandbox<S>
where
    S: Service,
{
    config: S::Config,
    child: Child,
    _phantom: PhantomData<S>,
}

impl<S> Sandbox<S>
where
    S: Service,
{
    pub async fn new(config: S::Config) -> Result<Sandbox<S>, Error> {
        Ok(Sandbox {
            child: Child::new(&config, S::args(&config)).await?,
            _phantom: PhantomData,
            config,
        })
    }

    pub async fn get_startup(&self) -> Response<()> {
        self.child.startup.lock().await.clone()
    }

    pub async fn kill(&self) -> Result<(), Error> {
        self.child.kill().await?;
        Ok(())
    }

    pub async fn restart(&self) -> Result<(), Error> {
        self.kill().await?;
        self.child
            .start(&self.config, S::args(&self.config))
            .await?;
        Ok(())
    }

    pub async fn restart_if_dead(&self) -> Result<(), Error> {
        if self.child.is_dead().await {
            self.child
                .start(&self.config, S::args(&self.config))
                .await?;
        }

        Ok(())
    }

    pub async fn eval(
        &self,
        req: S::Req,
        ctrlc: &mut CtrlC,
        exec_time: Duration,
    ) -> Result<Response<S::Res>, Error> {
        let interrupted = async {
            ctrlc.next().await;
            Err(Error::Interrupted)
        };
        let task = async { self.eval_impl(req, exec_time).await };

        let result = interrupted.race(task).await;
        let is_err = result.is_err();
        if is_err {
            self.restart().await?;
        } else {
            self.restart_if_dead().await?;
        }

        result
    }

    async fn eval_impl(&self, req: S::Req, exec_time: Duration) -> Result<Response<S::Res>, Error> {
        self.restart_if_dead().await?;

        let mut frame = Frame::new();

        let mut readwrite = self.child.readwrite.lock().await.take().unwrap();
        frame
            .write_async::<MessageRequest<S>, _>(Pin::new(&mut readwrite.stdin), &req)
            .await?;

        let next_frame = async {
            let value = frame
                .read_async::<MessageResponse<S>, _>(Pin::new(&mut readwrite.stdout))
                .await?;

            // Restore the readwrite, in the case where the process doesn't time out.
            let mut orig = self.child.readwrite.lock().await;
            if orig.is_none() {
                *orig = Some(readwrite);
            }

            Ok::<MessageResponse<S>, Error>(value)
        };

        let response = match timeout(exec_time, next_frame).await {
            Ok(result) => result?,
            Err(_timeout) => {
                return Err(Error::Timeout(exec_time));
            }
        };

        Ok(Response {
            result: match response.result {
                Ok(payload) => payload,
                Err(err) => return Err(err.into()),
            },
            memory_used: response.memory_used,
            time_taken: response.time_taken,
            stdout: response.stdout,
        })
    }
}
