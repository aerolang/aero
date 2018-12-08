// Part of the Aero programming language. License: MIT.

/// Exports NIFs and handles Aero encoding and decoding.
///
/// Three arguments are passed in here. The first argument is the
/// module name the NIFs are exported for. The second argument is a
/// list of 4-tuples. The tuple has the target wrapped function name,
/// the rust function to wrap with Aero types, a tuple containing the
/// functions argument types, and the last being the return type. The
/// third argument is passed into the `rustler_export_nifs!` macro.
/// The functions to use for the rustler macro are those created in
/// the second argument.
#[macro_export]
macro_rules! aero_export {
  ($name:expr, [ $( $nif:tt ),* ], $rustler_nif:tt) => {
    // Generate the wrapped functions.
    $(
      aero_export! {
        @internal_decl_fn
        $nif
      }
    )*

    // TODO: It would be much better to generate the NIFs just based
    //       on the second argument. However nested macro expansion
    //       is a problem so we might need to roll our own macros for
    //       this.
    rustler_export_nifs! {
      $name,
      $rustler_nif,
      None
    }
  };
  // Create the wrapped function.
  (@internal_decl_fn ($name:ident, $fn:path, $args_t:tt, $ret_t:tt)) => {
    fn $name<'a>(
      env: ::rustler::Env<'a>,
      args: &[::rustler::Term<'a>]
    ) -> ::rustler::NifResult<::rustler::Term<'a>> {
      let ret = aero_export!(@internal_fn_call $fn, args, $args_t);
      Ok(aero_encode!(ret, env, $ret_t))
    }
  };
  (@internal_fn_call $fn:path, $args:expr, ()) => {
    $fn()
  };
  (@internal_fn_call $fn:path, $args:expr, (
    $t_1:tt
  )) => {
    $fn(
      aero_decode!($args[0], $t_1)?
    )
  };
  (@internal_fn_call $fn:path, $args:expr, (
    $t_1:tt, $t_2:tt
  )) => {
    $fn(
      aero_decode!($args[0], $t_1)?,
      aero_decode!($args[1], $t_2)?
    )
  };
  (@internal_fn_call $fn:path, $args:expr, (
    $t_1:tt, $t_2:tt, $t_3:tt
  )) => {
    $fn(
      aero_decode!($args[0], $t_1)?,
      aero_decode!($args[1], $t_2)?,
      aero_decode!($args[2], $t_3)?
    )
  };
  (@internal_fn_call $fn:path, $args:expr, (
    $t_1:tt, $t_2:tt, $t_3:tt, $t_4:tt
  )) => {
    $fn(
      aero_decode!($args[0], $t_1)?,
      aero_decode!($args[1], $t_2)?,
      aero_decode!($args[2], $t_3)?,
      aero_decode!($args[3], $t_4)?
    )
  };
  (@internal_fn_call $fn:path, $args:expr, (
    $t_1:tt, $t_2:tt, $t_3:tt, $t_4:tt, $t_5:tt
  )) => {
    $fn(
      aero_decode!($args[0], $t_1)?,
      aero_decode!($args[1], $t_2)?,
      aero_decode!($args[2], $t_3)?,
      aero_decode!($args[3], $t_4)?,
      aero_decode!($args[4], $t_5)?,
      aero_decode!($args[5], $t_6)?
    )
  };
  (@internal_fn_call $fn:path, $args:expr, (
    $t_1:tt, $t_2:tt, $t_3:tt, $t_4:tt, $t_5:tt, $t_6:tt
  )) => {
    $fn(
      aero_decode!($args[0], $t_1)?,
      aero_decode!($args[1], $t_2)?,
      aero_decode!($args[2], $t_3)?,
      aero_decode!($args[3], $t_4)?,
      aero_decode!($args[4], $t_5)?,
      aero_decode!($args[5], $t_6)?
    )
  }
}
