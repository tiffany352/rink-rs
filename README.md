# Rink

[![codecov](https://codecov.io/gh/tiffany352/rink-rs/branch/master/graph/badge.svg)](https://codecov.io/gh/tiffany352/rink-rs)
[![crates.io](https://img.shields.io/crates/v/rink)](https://crates.io/crates/rink)
[![downloads](https://img.shields.io/crates/d/rink)](https://crates.io/crates/rink)

Rink is a unit-aware calculator. It can be used for physics and
engineering calculations, as well as dimensionality analysis.

Rink supports most systems of measurements including SI, CGS, natural,
international customary, US customary, UK customary, as well as
historical measurements. In addition, Rink supports currency
conversions.

Unique features:

- High-precision bignum rational arithmetic
- Detailed error messages
- Shows SI physical quantities
- Finds applicable SI derived units automatically
- Helps with dimensionality analysis, such as by offering unit
  factorizations and finding units for quantities.
- Open source
- First-class support for non-absolute temperature scales

## [Manual](./docs/rink.7.adoc)

Describes (hopefully) everything you need to know to use Rink's
expression language.

* [CLI man page](./docs/rink.1.adoc)
* [CLI config.toml](./docs/rink.5.adoc)the trick is that 

## [Web Interface: rinkcalc.app](https://rinkcalc.app)

Rink is available via a web interface in addition to the terminal-based
interface. Useful for doing calculations on mobile.

## Install

The most common version of Rink is the command line interface `rink`.

| Package Manager | Command              |
| --------------- | -------------------- |
| Cargo           | `cargo install rink` |
| Pacman          | `pacman -S rink`     |
| Nix             | `nix-env -i rink`    |
| Scoop           | `scoop install rink` |

## Examples

```
> kWh/year -> W
0.1140795 watt (power)
```

```
> W -> J
Conformance error: 1 watt (power) != 1 joule (energy)
Suggestions: multiply left side by time, multiply right side by frequency
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

[![docs.rs](https://img.shields.io/docsrs/rink-core)](https://docs.rs/rink-core/latest/rink_core/)

```toml
[dependencies]
rink-core = "0.6"
```

## License

Rink source code is licensed under the Mozilla Public License, version 2. See [LICENSE-MPL](./LICENSE-MPL) for details.

The data file `definitions.units` is licensed under the GNU General
Public License, version 3. See [LICENSE-GPL](./LICENSE-GPL) for details.

### Contribution

Contribution guide can be found on the wiki: https://github.com/tiffany352/rink-rs/wiki#contribution-guide

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work will grant the rights lined out in
the MPL, including larger works with secondary licenses.

## Changelog

See here: https://github.com/tiffany352/rink-rs/releases
