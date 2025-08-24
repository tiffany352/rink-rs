# Rink security model

The Rink language is explicitly not turing complete and has no access to
IO with the outside world. It can only operate on data which was passed
into it.

As long as Rink is run with tight limits set on CPU and memory usage, it
should be safe to run untrusted or even malicious queries.

Rink's CLI comes with a sandboxing feature which can be enabled through
the config file. This runs queries in a child process with limited CPU
time and memory.

Queries that cause these limits to be reached are not considered
security defects or even bugs.

Queries that cause either the host or child process to panic() or
abort() are considered bugs, not security defects.

If there was a query that could escape Rink's sandbox, for example by:

- Running arbitrary code
- Reading or writing arbitrary files on the machine
- Performing network requests

These would be security bugs and would warrant putting out a hotfix
release.
