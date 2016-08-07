# Rink

A unit conversion tool in Rust. The name is a subset of `frink`,
another unit conversion tool, to indicate that this tool implements a
subset of frink's features.

Select features:
- High-precision bignum rational arithmetic
- Detailed error messages
- IRC bot

## Install

`cargo install rink`

You must then download
[units.txt](https://raw.githubusercontent.com/tiffany352/rink-rs/master/units.txt)
and install it in `~/.config/rink/units.txt` (Linux),
`%APPDATA%\rink\units.txt` (Windows), or `~/Library/Application
Support/rink/units.txt` (MacOS).

## Examples

```
> kWh/year -> W
approx. 0.1140795 W (power)
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
11630 kWh (energy)
```

```
> googol^100
1.0e10000 (dimensionless)
```

The number of atoms in 12kg of lead:
```
> 12 kg / lead avogadro
approx. 3.487726e25 (dimensionless)
```

## Library Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
rink = "0.2"
```

and this to your crate root:

```rust
extern crate rink;
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

## Changelog

### 0.2.0
- Errors for division by zero
- Better conversion (->) output
- Bignum arithmetic
- API docs
- IRC bot
- Addition

### 0.1.1
- Search for units.txt in standard directories
- Rustyline

### 0.1.0
- Initial release
