# Aero Agent Guide

This is Aero, a functional programming language in development.

## General Guidance

When asked to make a change, change the behavior without keeping backwards
compatibility. This repository has no external users.

Write confident, assertive code. Don't rely on fallback logic, instead, errors
must always be appropriately returned to the caller.

## AIR

Aero compiles down to AIR, its lower-level IR. AIR is designed to be a
barebones functional programming language.

The Bazel test target `//compiler/airc:integration_test` can be tested for
validating AIR changes for syntax, the compiler, or the runtime.

## Build System

The project uses Bazel as its exclusive build system. *DO NOT* attempt builds
or tests directly with Cargo or Zig.

A `justfile` in the project root also exists for convenience.

Run `just build` to build everything, `just test` to test everything, and
`just format` to format after making edits.

Some tools like `clangd` or `rust-analyzer` may have issues looking for
dependencies or generated files. `just dev` will set up `compile_commands.json`
and development symlinks.
