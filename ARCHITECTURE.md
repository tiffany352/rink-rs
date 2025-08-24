= Rink architectrue

There are 3 high level pieces to Rink:

1. The units definition files
2. The Rink language and runtime
3. The frontends

## The unit definitions files

These are the source of most of the units and other information that
Rink has. They are written in a custom format that is a derivative of
the format used by GNU Units.

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

The implementation of these types are located in the Runtime.

To evaluate queries, it's necessary to have the units database loaded
into memory. This is handled by the Context and Registry, which are used
while evaluating. These help with unit name lookups and other tasks.

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

All frontends are ultimately wrappers around rink-core, and are
responsible for feeding it the data it needs such as the unit definition
files and the currency data.

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
