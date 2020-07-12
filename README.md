# Aero

[![Build Status](https://travis-ci.org/bbridges/aero.svg?branch=master)](https://travis-ci.org/bbridges/aero)

Aero is an experimental programming language aimed to have a statically-typed
Actor model implementation similar to Erlang and the semantics more-or-less of
an ML.

*Programming language* is a strong term. It's currently nowhere near that.
Hopefully, this changes in the future!

## Bootstrapping

The bootstrap compiler is currently being written in Erlang with the language
keywords implemented in Elixir macros. The bootstrap compiler converts the
Aero AST into the Elixir AST and uses the Elixir compiler to generate
BEAM object code.

## License

Released under the MIT License (see `LICENSE`).
