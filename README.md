# Rink

[![codecov](https://codecov.io/gh/tiffany352/rink-rs/branch/master/graph/badge.svg)](https://codecov.io/gh/tiffany352/rink-rs)
[![crates.io](https://img.shields.io/crates/v/rink)](https://crates.io/crates/rink)
[![downloads](https://img.shields.io/crates/d/rink)](https://crates.io/crates/rink)

https://rinkcalc.app/about

Rink is an open source unit-aware calculator. It can be used for physics
and engineering calculations, as well as dimensionality analysis.

Rink supports most systems of measurements including SI, CGS, natural,
international customary, US customary, UK customary, as well as
historical measurements. In addition, Rink supports currency
conversions.

## Features

* Arbitrary precision math
* Shows SI physical quantities
* Finds applicable SI derived units automatically
* Detailed error messages
* Helps with dimensionality analysis, such as by offering unit
  factorizations and finding units for quantities.
* Temperature conversions

## [Manual](https://rinkcalc.app/manual)

Describes (hopefully) everything you need to know to use Rink's
expression language.

* [CLI man page](https://rinkcalc.app/cli-manpage)
* [CLI config.toml](https://rinkcalc.app/configuration)

## Install

The most common version of Rink is the command line interface `rink`.

| Package Manager | Command              |
| --------------- | -------------------- |
| Cargo           | `cargo install rink` |
| Homebrew        | `brew install rink`  |
| Pacman          | `pacman -S rink`     |
| Nix             | `nix-env -i rink`    |
| Scoop           | `scoop install rink` |

Downloads are also available at <https://rinkcalc.app/releases>.

## [Web Interface: rinkcalc.app](https://rinkcalc.app)

Rink is available via a web interface in addition to the terminal-based
interface. Useful for doing calculations on mobile.

## Examples

How much does it cost to run my computer each year? Say it uses 100
watts for 4 hours per day, and use the [US average electricity
cost][elec].

[elec]: https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=epmt_5_6_a

```
> 0.1545$/kWh * 100W * (4 hours / day) to $/year
approx. 22.57196 USD / tropicalyear
```

If you made a solid sphere of gold the size of the moon, what would the
surface gravity be?

```
> volume of moon * (19.283 g/cm^3) * G / (radius of moon)^2
approx. 9.365338 meter / second^2 (acceleration)
> ans to gravity
approx. 0.9549987 gravity (acceleration)
```

Ever heard someone joke about Americans measuring fuel efficiency as
rods per hogshead? Let's try with the [average US car fuel
efficiency][eff].

[eff]: https://www.bts.gov/content/average-fuel-efficiency-us-passenger-cars-and-light-trucks

```
> 9.4 km/l to mpg
approx. 22.11017 mpg (fuel_efficiency)
> 9.4 km/l to rods per hogshead
approx. 445741.0 rod / ushogshead (fuel_efficiency)
```

And then you wonder, wait, what even are these units anyway?

```
> hogshead
Definition: ushogshead = 2 liquidbarrel = approx. 238480942.3 millimeter^3 (volume; m^3)
> liquidbarrel
Definition: liquidbarrel = 31.5 usgallon = approx. 119240471.1 millimeter^3 (volume; m^3)
> rod
Definition: rod = 5.5 yard = 5.0292 meter (length; m)
```

## Library Usage

[![docs.rs](https://img.shields.io/docsrs/rink-core)](https://docs.rs/rink-core/latest/rink_core/)

```toml
[dependencies]
rink-core = "0.8"
```

## License

Rink source code is licensed under the Mozilla Public License,
version 2. See [LICENSE-MPL](./LICENSE-MPL) for details.

The data file `definitions.units` is licensed under the GNU General
Public License, version 3. See [LICENSE-GPL](./LICENSE-GPL) for details.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work will grant the rights lined out in
the MPL, including larger works with secondary licenses.

Check out [ARCHITECTURE.md](./ARCHITECTURE.md) for an overview of how
the project is structured, and [DEVELOPMENT.md](./DEVELOPMENT.md) for
local dev setup.

## Security

See [SECURITY.md](./SECURITY.md)

## Changelog

See here: https://rinkcalc.app/releases
