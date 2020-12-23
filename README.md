# Aero

[![Build Status](https://www.travis-ci.com/aerolang/aero.svg?branch=master)](https://www.travis-ci.com/aerolang/aero)

Aero is an experimental programming language aimed to be statically typed,
purely functional with uniqueness types and eager evaluation, and oriented
entirely around the actor model: a blend of the ideas of Erlang, Elixir, Rust,
and Clean.

The language is currently under construction and so features listed above are
not necessarily implemented yet. The language is being bootstrapped with an
Erlang-based compiler targeting Core Erlang for a future self-hosting compiler.

## Building

Erlang and Elixir are required to build the compiler and Erlang is needed on the
host that Aero programs are run on.

The recommended way to install Erlang and Elixir is with
[asdf](https://asdf-vm.com). This allows for easy version managment. Aero
expects the Erlang/OTP version to at least be 23.0.

```sh
$ asdf plugin add erlang
$ asdf plugin add elixir

$ asdf install erlang 23.2
$ asdf install elixir 1.11.2-otp-23
```

Building the compiler uses `mix` from Elixir.

```sh
$ mix deps.get
$ mix test
$ mix escript.build
```

This generates an Escript in called `./aero` which can be used to compile Aero
programs.

## Examples

See `examples/` for sample Aero programs. They can be run like so:

```sh
$ ./aero compile examples/hello.aero --escript
$ ./out/bin/hello
```

## License

Released under the MIT License (see `LICENSE`).
