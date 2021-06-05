# rink-sandbox

Rink-sandbox was designed to sandbox Rink queries (which can take
arbitrarily large memory/time). Rink's syntax does not expose any type
of IO, so a full security sandbox is not required.

This crate was designed with some effort to making it general purpose
for other types of applications, but it may not perfectly match some
usecases.

Rink-sandbox is a crate for running app code in a contained environment, specifically:

- Memory usage is limited.
- Execution time is limited.
- Tasks can be interrupted with Ctrl+C.
- Panics are caught and surfaced to the caller.

This is achieved by running tasks in a child process. Memory is limited
using `#[global_allocator]`. Execution time is limited using a simple
timeout, rather than relying on OS scheduler-level timing.

There's no platform specific code or unsafe usage in this crate, this is
handled by other dependencies.

Messages are serialized using bincode and sent through the child
process's stdin/stdout channels for best portability. Generally the
child will hold the lock on these channels to prevent stray messages,
but you should avoid writing to them in app code if possible.

Specifically, avoid using `println!()` in favor of `eprintln!()`.
