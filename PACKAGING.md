# Distro Packaging

Requirements:

- Rust compiler toolchain (stable build)
- `asciidoctor` for building manpages

Additionally, if these libraries are present on the system, they will be
used, otherwise they are statically linked:

- `libcurl`
- `libssl` (OpenSSL)

Rink requires several data files in order to work. By default these are
baked into the binary so that `cargo install` will work, but for distro
packaging these should probably go into `/usr/share` instead. The
makefile will do this automatically.

An example `.desktop` file is available at `cli/rink.desktop`. The
website's favicon from `web/site/favicon.png` can be reused as an icon.

## Makefile-based method

```sh
make fetch
make all prefix=/usr
make install prefix=/usr DESTDIR=$pkgdir
```

Running `make all` will build both the program and the man pages, and
`make install` will install them.

Note that the makefile accepts _both_ the `prefix` and `DESTDIR`
arguments. `prefix` is where Rink should look for its files, this should
generally be `/usr`. `DESTDIR` is where the files will be copied to upon
running `make install`.

`make man` can be used to build just the manpages if desired. They're
placed into the `build/` directory. Similarly, `make installman` will
install just the manpages.

The makefile doesn't automatically install the .desktop file, see below.

## Manual packaging

Build the program using cargo. The RINK_PATH environment variable can be
set to `/usr/share/rink` so that rink will look in this directory for
its files. Passing `--no-default-features` will turn off rink bundling
the files into its executable file.

1. Install `target/release/rink` -> `/usr/bin/rink`.
2. Install `core/definitions.units` ->
   `/usr/share/rink/definitions.units`.
3. Install `core/currency.units` -> `/usr/share/rink/currency.units`.
4. Install `datepatterns.txt` -> `/usr/share/rink/datepatterns.txt`.

### Manpages

The manpages in `docs/` can be built using asciidoctor, and then
installed into the relevant directory.

### `.desktop` file

1. Install `cli/rink.desktop` -> `/usr/share/applications/rink.desktop`.
2. Install `web/site/favicon.png` -> `/usr/share/pixmaps/rink.png`.
