# rink-sandbox

Rink-sandbox was designed to sandbox Rink queries (which can take
arbitrarily large memory/time). Rink's syntax does not expose any type
of IO, so a full security sandbox is not required.

This crate supports Windows, macOS, and Linux.

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

There's no platform specific code in this crate, this is handled by
other dependencies. Unsafe code is limited to only the allocator, where
it's required.

Messages are serialized using bincode and sent through the child
process's stdin/stdout channels for best portability.

## Limitations

- This is not a security solution on its own. All network, disk, and
  other environment access is still allowed in the child process.
- Your app code needs to avoid reading from stdin or writing to stdout
  when becoming the child process. Unfortunately, there's no good
  solution for redirecting or capturing stdout: gag and stdio-redirect
  are Linux only, and shh doesn't allow reading without blocking until
  the pipe is closed. The next best solution is to write a logger and
  expect all app code to use `log` instead of `println`.
- This might not work well outside of the usecase of a CLI app, but this
  could be improved in the future.
