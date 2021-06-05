use async_std::io::{prelude::WriteExt, stdin, stdout};
use rink_sandbox::{Alloc, Sandbox, Service};
use std::{env, ffi::OsString, io::Error as IoError, time::Duration};

struct AddTwoService;

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
    type Req = (i64, i64);
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
        Duration::from_secs(2)
    }

    // On the child, the service will be created from the config on startup.
    fn create(_config: Self::Config) -> Result<Self, IoError> {
        Ok(AddTwoService)
    }

    // Process 1 message.
    fn handle(&self, (left, right): Self::Req) -> Self::Res {
        left + right
    }
}

#[async_std::main]
async fn main() -> Result<(), IoError> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() > 1 && args[1] == "--child" {
        rink_sandbox::become_child::<AddTwoService>(&GLOBAL);
    }

    // The Sandbox object is how the parent process starts up and manipulates the child process.
    let sandbox = Sandbox::<AddTwoService>::new(()).await?;

    loop {
        // This code is a manually written out prompter. You should
        // generally use a crate for this, like rustyline.
        print!("> ");
        stdout().flush().await?;

        let mut line = String::new();
        stdin().read_line(&mut line).await?;

        let numbers = line
            .split('+')
            .map(|string| i64::from_str_radix(string.trim(), 10))
            .collect::<Result<Vec<_>, _>>();

        let pair = match numbers {
            Ok(list) if list.len() == 2 => (list[0], list[1]),
            Ok(_) => {
                println!("Usage: 1 + 1");
                continue;
            }
            Err(err) => {
                println!("{}", err);
                continue;
            }
        };

        // Here's the actually important part. This sends the request
        // off to the child for execution, and then returns the response
        // when it's done. The response contains the time and memory
        // usage on a per-query basis.
        //
        // If something goes wrong, you can match on the Error object to
        // find out why. It has values for interrupts (ctrl+C),
        // timeouts, panics, and more.
        let result = sandbox.execute(pair).await;
        match result {
            Ok(res) => println!(
                "Success! Calculated {} + {} = {} in {:?} with {}K of memory",
                pair.0,
                pair.1,
                res.result,
                res.time_taken,
                res.memory_used / 1000,
            ),
            Err(err) => println!("{}", err),
        }
    }
}
