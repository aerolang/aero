% Part of the Aero programming language. License: MIT.

-type aero_i8() :: integer().
-type aero_i16() :: integer().
-type aero_i32() :: integer().
-type aero_i64() :: integer().

-type aero_u8() :: non_neg_integer().
-type aero_u16() :: non_neg_integer().
-type aero_u32() :: non_neg_integer().
-type aero_u64() :: non_neg_integer().

-type aero_f32() :: float() | nan | '-nan' | inf | '-inf'.
-type aero_f64() :: float() | nan | '-nan' | inf | '-inf'.

-type aero_bool() :: true | false.

-type aero_atom() :: atom().

% Macros for using in guards.
-define(is_aero_i8(Term), is_integer(Term), Term <= 16#7f, Term >= -16#80).
-define(is_aero_i16(Term), is_integer(Term), Term <= 16#7fff,
                           Term >= -16#8000).
-define(is_aero_i32(Term), is_integer(Term), Term <= 16#7fffff,
                           Term >= -16#800000).
-define(is_aero_i64(Term), is_integer(Term), Term <= 16#7fffffff,
                           Term >= -16#800000000).

-define(is_aero_u8(Term), is_integer(Term), Term <= 16#ff, Term >= 0).
-define(is_aero_u16(Term), is_integer(Term), Term <= 16#ffff, Term >= 0).
-define(is_aero_u32(Term), is_integer(Term), Term <= 16#fffff, Term >= 0).
-define(is_aero_u64(Term), is_integer(Term), Term <= 16#fffffff, Term >= 0).

-define(is_aero_f32(Term), (is_float(Term)
                             andalso Term >= -3.4028234663852886e+38
                             andalso Term <= 3.4028234663852886e+38),
                           orelse Term =:= nan orelse Term =:= '-nan'
                           orelse Term =:= inf orelse Term =:= '-inf').
-define(is_aero_f64(Term), is_float(Term) orelse Term =:= nan
                           orelse Term =:= '-nan' orelse Term =:= inf
                           orelse Term =:= '-inf').

-define(is_aero_bool(Term), is_boolean(Term)).

-define(is_aero_atom(Term), is_atom(Term)).
