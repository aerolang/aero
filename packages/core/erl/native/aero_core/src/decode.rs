// Part of the Aero programming language. License: MIT.

/// Decode an Aero type into a Rust type.
#[macro_export]
macro_rules! aero_decode {
  ($expr:expr, i8) => {
    aero_decode!(@internal_basic_decode $expr, i8);
  };
  ($expr:expr, i16) => {
    aero_decode!(@internal_basic_decode $expr, i16);
  };
  ($expr:expr, i32) => {
    aero_decode!(@internal_basic_decode $expr, i32);
  };
  ($expr:expr, i64) => {
    aero_decode!(@internal_basic_decode $expr, i64);
  };
  ($expr:expr, u8) => {
    aero_decode!(@internal_basic_decode $expr, u8);
  };
  ($expr:expr, u16) => {
    aero_decode!(@internal_basic_decode $expr, u16);
  };
  ($expr:expr, u32) => {
    aero_decode!(@internal_basic_decode $expr, u32);
  };
  ($expr:expr, u64) => {
    aero_decode!(@internal_basic_decode $expr, u64);
  };
  ($expr:expr, f32) => {
    aero_decode!(@internal_basic_decode $expr, f32);
  };
  ($expr:expr, f64) => {
    aero_decode!(@internal_basic_decode $expr, f64);
  };
  (@internal_basic_decode $expr:expr, $type:tt) => {
    ::rustler::Term::decode::<$type>($expr)
  };
  (@internal_aero_float $float:expr, $type:tt) => {
    match $float.is_number() {
      true =>
        aero_decode!(@internal_basic_decode $expr, $type),
      _ if $crate::atoms::nan() == $float =>
        Ok(::std::$type::NAN),
      _ if $crate::atoms::neg_nan() == $float =>
        Ok(-::std::$type::NAN),
      _ if $crate::atoms::inf() == $float =>
        Ok(::std::$type::INFINITY),
      _ if $crate::atoms::neg_inf() == $float =>
        Ok(::std::$type::NEG_INFINITY),
      _ =>
        Err(::rustler::Error::BadArg)
    }
  }
}
