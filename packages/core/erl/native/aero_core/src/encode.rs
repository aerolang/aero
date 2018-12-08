// Part of the Aero programming language. License: MIT.

/// Encode a Rust type as an Aero type.
#[macro_export]
macro_rules! aero_encode {
  ($expr:expr, $env:expr, i8) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, i16) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, i32) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, i64) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, u8) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, u16) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, u32) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, u64) => {
    aero_encode!(@internal_basic_encode $expr, $env);
  };
  ($expr:expr, $env:expr, f32) => {
    aero_encode!(@internal_aero_float $expr, $env);
  };
  ($expr:expr, $env:expr, f64) => {
    aero_encode!(@internal_aero_float $expr, $env);
  };
  (@internal_basic_encode $expr:expr, $env:expr) => {
    ::rustler::Encoder::encode(&$expr, $env)
  };
  (@internal_aero_float $float:expr, $env:expr) => {
    match $float.classify() {
      ::std::num::FpCategory::Nan if $float.is_sign_positive() =>
        aero_encode!(@internal_basic_encode $crate::atoms::nan(), $env),
      ::std::num::FpCategory::Nan =>
        aero_encode!(@internal_basic_encode $crate::atoms::neg_nan(), $env),
      ::std::num::FpCategory::Infinite if $float.is_sign_positive() =>
        aero_encode!(@internal_basic_encode $crate::atoms::inf(), $env),
      ::std::num::FpCategory::Infinite =>
        aero_encode!(@internal_basic_encode $crate::atoms::neg_inf(), $env),
      _ =>
        aero_encode!(@internal_basic_encode $float, $env),
    }
  }
}
