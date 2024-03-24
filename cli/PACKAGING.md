# Distro Packaging

A makefile is provided for easier packaging.

Requirements:

- Rust compiler toolchain (stable build)
- `asciidoctor` for building manpages

Running `make all` will build both the program and the man pages, and
`make install` will install them.

Rink requires several data files in order to work. By default these are
baked into the binary so that `cargo install` will work, but for distro
packaging these should probably go into `/usr/share` instead. The
makefile will do this automatically.

Note that the makefile accepts _both_ the `prefix` and `DESTDIR`
arguments. `prefix` is where Rink should look for its files, this should
generally be `/usr`. `DESTDIR` is where the files will be copied to upon
running `make install`.
