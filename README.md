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
[definitions.units](https://raw.githubusercontent.com/tiffany352/rink-rs/master/definitions.units)
and install it in `~/.config/rink/definitions.units` (Linux),
`%APPDATA%\rink\definitions.units` (Windows), or `~/Library/Application
Support/rink/definitions.units` (MacOS).

## Examples

```
> kWh/year -> W
approx. 0.1140795 W (power)
```

```
> W -> J
Conformance error
   Left side: 1 watt (power)
  Right side: 1 joule (energy)
  Suggestion: multiply left side by time
              multiply right side by frequency
```

```
> gallon gasoline -> kWh
approx. 36.63388 kWh (energy)
```

```
> googol^100
1.0e10000 (dimensionless)
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

Rink source code is licensed under the Mozilla Public License, version
2. See [LICENSE-MPL](./LICENSE-MPL) for details.

The data file `definitions.units` is licensed under the GNU General
Public License, version 3. See [LICENSE-GPL](./LICENSE-GPL) for details.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work will grant the rights lined out in
the MPL, including larger works with secondary licenses.

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
