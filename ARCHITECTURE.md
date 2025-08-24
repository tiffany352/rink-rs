= Rink architecture

There are 3 high level pieces to Rink:

1. The units definition files
2. The Rink language and runtime
3. The frontends

## The unit definitions files

These are the source of most of the units and other information that
Rink has. The units file was forked from the one used in GNU Units in
2016. It adds several new syntax constructs, and some changes have been
made over time.

The format is documented here: https://rinkcalc.app/definitions-format/

The definitions are mostly in `core/definitions.units` with a few in
`core/currency.units`.

There is also a secondary format for date patterns, which is located at
`core/datepatterns.txt` with a format described here:
<https://rinkcalc.app/datepatterns-format/>

It's possible to make valuable contributions to Rink without ever
writing or reading a line of Rust code. Finding sources for obscure
units, and then adding them to the definitions file, is a great
contribution!

## The Rink language itself

This is what the `rink-core` crate, located in `core/` in the repo, is
mostly for.

The language has a top-level construct called the "query", this includes
things like conversions (`foo -> bar`). Inside the query are
"expressions", which are nested constructs like multiplication and
division.

The steps to running a Rink query are:

1. Tokenization
2. Parsing into an AST, using a hand-written recursive descent parser
3. Evaluation
4. Outputting the result to the caller
5. Printing / Formatting

During evaluation, the values produced from each expression are stored
in the Value object. This is an enum representing all of the possible
types that can be used in Rink, such as numbers, dates, and substances.

Numbers are made of two components: The `Numeric` and the
`Dimensionality`.

Numerics are normally stored as `BigRat`, which is short for big
rational. (Although you can imagine it as a capybara-sized rat if you
like.) This type is a ratio of two BigInt's, which are arbitrarily large
integers. Rink uses this to get higher precision and to work with very
large or very small numbers.

Floating point numbers are also sometimes used as a fallback, for
example for trigonometric functions.

The `Dimensionality` stores the physical quantities of the number, in
terms of base units. So for example, if you write `3 feet/minute`, the
number will be stored in memory as a value of `0.01524` and a
dimensionality of `m^1 s^-1` (meters per second). This reduction to base
units is how Rink handles all math on numbers that have units attached,
and why it's possible to do things like `1 meter + 1 inch`.

Substances are how rink represents "things" like chemicals and planets.
Substances have properties that can either be a ratio, such as density
(mass/volume), or a "constant" which is anything that isn't a ratio.
Substances can have a quantity attached - for example, `3 eggs`, `1ml
water`, etc. The reason why ratios are stored explicitly is to allow for
contextual properties, like `mass of 1ml water` (via the density).

The implementation of these types are located in the Runtime.

To evaluate queries, it's necessary to have the units database loaded
into memory. This is handled by the Context and Registry. The Registry
handles looking up unit names, while the Context stores the registry and
some other data.

The loader is repsonsible for constructing the Context+Registry. It
handles things like dependency resolution between units.

An important aspect of `rink-core` is that it shouldn't do any file IO
or network requests on its own. These are expected to be provided by the
caller, as the methods can vary between environments.

## The frontends

Rink has three official frontends: The CLI, the web interface, and the
IRC bot.

The CLI is the most featureful frontend, and the one that most users use
day to day. The web interface is useful on mobile phones and for new
users.

All frontends are ultimately wrappers around `rink-core`, and are
responsible for feeding it the data it needs, such as the unit
definition files and the currency data.

Currency data is downloaded live from
`https://rinkcalc.app/data/currency.json`.

## The website

The website houses 3 main parts:

1. Documentation
2. The `currency.json` endpoint
3. The Rink web interface

The documentation is a set of AsciiDoc files that are compiled into a
fully static website.

The `currency.json` file is created by a Javascript program running in
an hourly cron job. It collects data from various sources in various
formats, then writes a simple JSON file for frontends to download.

The web interface is a version of Rink compiled to WebAssembly, with a
small JavaScript wrapper program to set it up and handle the REPL/UI.
The WASM binary exports a small API which is implemented in `rink-js`.

WASM is used so that the app can run directly in your browser, and even
work offline. It also keeps the website as a static site (no backend
code).
