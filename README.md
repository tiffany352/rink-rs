# Rink

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

## [Manual](https://github.com/tiffany352/rink-rs/wiki/Rink-Manual)

Describes (hopefully) everything you need to know to use Rink's
expression language.

## [Web Interface](https://tiffnix.com/rink/)

Rink is available via the web, so that you don't have to install it.

## Install

`cargo install rink`

Running `rink` will give you a CLI interface for you to enter queries.

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

Add this to your `Cargo.toml`:

```toml
[dependencies]
rink = "0.4"
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

### 0.4.3
- Replace BTC-e because its creator was arrested, we use blockchain.info now
- Update linefeed to non-ancient version
- Make % an operator with higher precedence than pow
- Add fallback to CLI to a non-interactive prompt
- Fix unused mut warnings

### 0.4.2
- Add support for CRLF definitions.units, fixing 0.4.1 being broken

### 0.4.1
- Add unit categories
- Add `speed of light`
- Add CLI history
- Add `-> digits` and `-> digits N`
- Fix conversion to substance
- Fix division by float zero not throwing an error
- Automatically derive molar mass from formulas

### 0.4.0
- Rewritten web interface
- Add a notion of substances
- Add tzdb support
- Base conversions
- Change to precedence rules: multiplication by juxtaposition now has
  higher precedence than `/` and `*`, both of which now have the same
  precedence and are left associative
- Unit documentation
- Show definitions
- `_` and unicode thin space as digit separators
- `search` command
- Add float-based implementations of trig functions
- Fix root calculations
- Fix caching behavior with downloaded data
- Make `in` an alias for `->`
- Make Gb gigabits, not gilberts
- Make seconds optional in date literals
- Make colon optional in time offsets

### 0.3.2
- Time quantities now automatically display in human units (year,
  week, day, hour, minute, second)
- Currency conversions sourced from the European Central Bank, and the
  BTC-E bitcoin exchange
- Move all element units from being dimensionless to being g/mol
  (molar mass)
- Add atomic masses for elements 104 (rutherfordium) through 118
  (ununoctium)
- Add more date patterns and make them less strict
- Allow durations larger than ~200 years
- Fix viscosity quantity dimensionality
- Time offset conversions
- Fix bugs in date parser

### 0.3.1
- Automatically find SI prefixes
- Limit to one prefix on units to prevent very unintuitive results
- Switch to linefeed from rustyline
- Tab completion
- Fix a serious number printing bug
- Significantly improve canonicalizations
- Constants in right-hand side of conversions
- New quantities
- Fix parsec definition
- Reintroduce jerk, snap, crackle, and pop
- Added π, ¢, and ħ symbols
- Better determinism by removing hashmaps
- Typo suggestions
- Simple unit namespacing allowing for things like "british foot"
- Minor error message improvements
- Refactors
- Russian traditional measurements

### 0.3.0
- Use GNU units database instead of frink's
- Relicense under MPL
- Web interface
- Multi-server IRC in bot
- Date arithmetic
- Exact fraction printing
- Sandboxing
- Add temperature scale suffixes
- Factorize query
- Function call syntax
- Errors on division by zero
- Errors on unimplemented imaginary roots
- Inline unit definitions
- Bot name prefix is optional in private message
- Fix associativity of addition
- Rename commonly confused units
- Unit lists (hr;min;sec) in conversions
- "Units for" query
- Show unit definitions when they are the only term
- Make / lower precedence than multiplication, add | which is higher
  precedence
- Unit tests
- Output token symbols instead of internal names
- Canonicalize output units
- Try to find a matching derived unit for queries

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
