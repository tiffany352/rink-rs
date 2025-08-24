# Development setup

This page is a lot but most of it you can ignore. The only important
thing is being able to compile rust and run `cargo test`.

## Basic setup

- Recent Rust compiler (`rustup update stable`)
- (Optional) Rust LSP in your code editor

On Arch Linux:

```sh
# Basic
pacman -S rustup
rustup update stable
```

Common commands:

```sh
cargo test --all # Run all tests (slow)
cargo test -p rink-core # Run rink-core tests (faster)
cargo fmt # Reformat your code
```

## Code coverage

- [cargo llvm-cov](https://github.com/taiki-e/cargo-llvm-cov)
- (For `make coverage-report`) [Python uv](https://docs.astral.sh/uv/)
- (Optional) Extension for your code editor to render coverage info

On Arch Linux:

```sh
pacman -S cargo-llvm-cov uv
```

Common commands:

```sh
make coverage # Creates the lcov.info file
make coverage-report # Uses lcov.info to make report.md
```

## Manpages

Requires [AsciiDoctor](https://docs.asciidoctor.org/asciidoctor/latest/). (`pacman -S asciidoctor`)

```sh
make man
```

They will be placed in the `build/` folder and can be viewed by passing
them to man, like `man build/rink.1`

## Website

See [web/README.adoc](./web/README.adoc)
