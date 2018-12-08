// Part of the Aero programming language. License: MIT.

//! This crate has utilities for making Aero Erlang NIFs in Rust.

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rustler;

pub mod atoms;

mod decode;
mod encode;
mod export;
