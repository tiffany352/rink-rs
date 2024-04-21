## Rink 0.8.0

`gpl` feature has been renamed to `bundle-files`.

`rink_core::DATES_FILE` and `rink_core::CURRENCY_FILE` are now `Option`s
and are only Some if `bundle-files` is set, otherwise they are None,
just like `DEFAULT_FILE`.

The intention is for distro packages to install these files to the
filesystem (e.g. in `/usr/share/rink/`) instead of embedding them in the
executable. See [PACKAGING.md](../PACKAGING.md) for more info.

### Behavior

- Unrecognized directives in definitions.units now print an error
  instead of being silently ignored.
- Categories for non-units are now ignored. This may be improved in the
  future, but will require changing categories to be namespaced.
- The output has changed slightly, as numbers can be represented as
  recurring digits now.

## Rink 0.7.0

### Behavior

- Quantities are now in a separate namespace from normal units.

### Changes

- `Dimensionality` changed from an alias of BTreeMap to its own struct.
- Several fields of `Context` were moved to the `loader::Registry`
  object stored as a field on it.
  - Also moved `lookup_exact` and `lookup_with_prefix` there.
  - `dimensions` field renamed to `base_units`.
  - `reverse` field renamed to `decomposition_units`.
  - `canonicalizations` field renamed to `base_unit_long_names`.
- `Def::Prefix` and `Def::SPrefix` have been merged to an `is_long` bool
  on `Def::Prefix`. `SPrefix` is `is_long == true`.
- `Def::Dimension` and `Def::Canonicalization` were merged to create
  `Def::BaseUnit`.

### Removals

- `Context::short_output`
- `factorize::fast_decompose`
- `search::search`

### Moves & Renames

| old                            | new                                 |
| ------------------------------ | ----------------------------------- |
| `bigint::BigInt`               | `types::BigInt`                     |
| `bigrat::BigRat`               | `types::BigRat`                     |
| `context::Context`             | `loader::Context`                   |
| `Context::eval_outer`          | `Context::eval_query`               |
| `date::from_duration`          | `parsing::datetime::from_duration`  |
| `date::GenericDateTime`        | `types::GenericDateTime`            |
| `date::parse_datefile`         | `parsing::datetime::parse_datefile` |
| `date::to_duration`            | `parsing::datetime::to_duration`    |
| `Def::Dimension`               | `Def::BaseUnit`                     |
| `factorize::factorize`         | `commands::factorize`               |
| `factorize::Factors`           | `commands::Factors`                 |
| `fmt`                          | `output::fmt`                       |
| `formula`                      | `parsing::formula`                  |
| `gnu_units`                    | `loader::gnu_units`                 |
| `load::load_defs`              | `loader::load_defs`                 |
| `number::Dimension`            | `types::BaseUnit`                   |
| `number::Number`               | `types::Number`                     |
| `number::NumberParts`          | `output::NumberParts`               |
| `number::Quantity`             | `types::Dimensionality`             |
| `numeric::Digits`              | `output::Digits`                    |
| `numeric::Numeric`             | `types::Numeric`                    |
| `numeric::NumericParts`        | `output::NumericParts`              |
| `reply`                        | `output`                            |
| `search::query`                | `commands::search`                  |
| `search::SearchResult`         | `commands::SearchResult`            |
| `substance::Properties`        | `runtime::Properties`               |
| `substance::Property`          | `runtime::Property`                 |
| `substance::Substance`         | `runtime::Substance`                |
| `substance::SubstanceGetError` | `runtime::SubstanceGetError`        |
| `text_query`                   | `parsing::text_query`               |
| `value::Show`                  | `runtime::Show`                     |
| `value::Value`                 | `runtime::Value`                    |
