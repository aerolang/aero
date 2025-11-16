# Aero Agent Guide

This is Aero, a functional programming language in development.

Current work is going towards AIR. As we need to build our AIR before we can
focus on Aero which will run on top. For all intents and purposes, this
repository is about AIR right now.

## General Guidance

When asked to make a change, change the behavior without keeping backwards
compatibility. This repository has no external users.

Write confident, assertive code. Don't rely on fallback logic. Make sure it
works the intended way without unnecessary alternatives.

## AIR

Aero compiles down to AIR, it's lower-level IR. AIR is designed to be a
barebones functional programming language.

The Bazel test target `//compiler/airc:integration_test` can be tested for
validating AIR changes for syntax, compiler, or runtime.

## Build System

The project uses Bazel as its exclusive build system. Do not attempt builds
or tests directly with Cargo or Zig.

## Runtime

The runtime (located in `/runtime`) is written in Zig. It's important in the
runtime code to never swallow errors unless strictly required.
