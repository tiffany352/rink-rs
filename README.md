# Rink

A unit conversion tool in Rust. The name is a subset of `frink`,
another unit conversion tool, to indicate that this tool implements a
subset of frink's features.

## Install

`cargo install rink`

## Examples

```
> kWh/year -> W
0.11407955270702466 (dimensionless)
```

```
> W -> J
Conformance error
   Left side: 1 m^2 kg / s^3 (power)
  Right side: 1 m^2 kg / s^2 (energy)
  Suggestion: multiply left side by time
              multiply right side by frequency
```

```
> oil tonne -> kWh
11630 (dimensionless)
```

The number of atoms in 12kg of lead:
```
> 12 kg / lead avogadro
34877263650579153000000000 (dimensionless)
```

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
rink = "0.1"
```

and this to your crate root:

```rust
extern crate num
```

## License

Source code licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

The data file `units.txt` is from `frink` and is assumed to be
licensed under the `GNU GPL v3` because it is based on the data file from
`GNU Units(1)`, however the `frink` author has not acknowledged this
and the copyright header from the file has been removed.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.
