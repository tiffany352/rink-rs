use rink_sandbox::{Alloc, Sandbox, Service};
use serde_derive::{Deserialize, Serialize};
use std::{env, ffi::OsString, io::Error as IoError, time::Duration};

struct AddTwoService;

#[derive(Serialize, Deserialize)]
enum Request {
    AddTwo(i64, i64),
    ThrowPanic,
}

// Global allocators can't be swapped out at runtime, so your entire app
// will have to use the allocator. That said, this shouldn't have much
// overhead and the limit can be set to `usize::MAX` when not in use.
#[global_allocator]
pub(crate) static GLOBAL: Alloc = Alloc::new(usize::MAX);

// In order to keep the types the same between the parent and child
// processes, a Service trait is used. It's still possible to implement
// the trait twice and get bincode errors, but in most cases there will
// be 1 implementation of this trait.
impl Service for AddTwoService {
    // All of the associated types here have to be Serialize +
    // Deserialize, so that they can be sent over IPC.

    // Sent from the parent to the child to evaluate.
    type Req = Request;
    // Sent from the child to the parent after finishing.
    type Res = i64;
    // Passed from the parent to the child on startup.
    type Config = ();

    // Args to be passed to the child process. When the child receives
    // these arguments, it should call `rink_sandbox::become_child`.
    fn args(_config: &Self::Config) -> Vec<OsString> {
        vec!["--child".into()]
    }

    // The timeout to use for query execution.
    fn timeout(_config: &Self::Config) -> Duration {
        Duration::from_millis(100)
    }

    // On the child, the service will be created from the config on startup.
    fn create(_config: Self::Config) -> Result<Self, IoError> {
        Ok(AddTwoService)
    }

    // Process 1 message.
    fn handle(&self, request: Self::Req) -> Self::Res {
        match request {
            Request::AddTwo(left, right) => left + right,
            Request::ThrowPanic => {
                panic!("test panic");
            }
        }
    }
}

#[async_std::main]
async fn main() -> Result<(), IoError> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() > 1 && args[1] == "--child" {
        GLOBAL.set_limit(1_000_000);
        rink_sandbox::become_child::<AddTwoService, _>(&GLOBAL);
    }

    let sandbox = Sandbox::<AddTwoService>::new(()).await?;

    let response = sandbox.execute(Request::AddTwo(1, 2)).await.unwrap();
    assert_eq!(response.result, 3);

    let response = sandbox.execute(Request::AddTwo(17, 9)).await.unwrap();
    assert_eq!(response.result, 26);

    let response = sandbox.execute(Request::ThrowPanic).await.unwrap_err();
    assert!(response.to_string().contains("test panic"));

    println!("OK");
    Ok(())
}
