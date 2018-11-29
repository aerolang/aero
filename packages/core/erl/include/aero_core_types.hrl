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
-define(AERO_I8_MIN, -16#80).
-define(AERO_I8_MAX, 16#7f).
-define(AERO_I16_MIN, -16#8000).
-define(AERO_I16_MAX, 16#7fff).
-define(AERO_I32_MIN, -16#800000).
-define(AERO_I32_MAX, 16#7fffff).
-define(AERO_I64_MIN, -16#800000000).
-define(AERO_I64_MAX, 16#7fffffff).

-define(AERO_U8_MIN, 0).
-define(AERO_U8_MAX, 16#ff).
-define(AERO_U16_MIN, 0).
-define(AERO_U16_MAX, 16#ffff).
-define(AERO_U32_MIN, 0).
-define(AERO_U32_MAX, 16#ffffff).
-define(AERO_U64_MIN, 0).
-define(AERO_U64_MAX, 16#ffffffff).

-define(AERO_F32_MIN, -3.4028234663852886e+38).
-define(AERO_F32_MIN_POS, 1.1754943508222875e-38).
-define(AERO_F32_MAX, 3.4028234663852886e+38).
-define(AERO_F32_MAX_NEG, 1.1754943508222875e-38).
-define(AERO_F64_MIN, -1.7976931348623157e+308).
-define(AERO_F64_MIN_POS, 2.2250738585072014e-308).
-define(AERO_F64_MAX, 1.7976931348623157e+308).
-define(AERO_F64_MAX_NEG, -2.2250738585072014e-308).

-define(AERO_TRUE, true).
-define(AERO_FALSE, false).

-define(is_aero_i8(Term), is_integer(Term), Term <= ?AERO_I8_MAX,
                          Term >= ?AERO_I8_MIN).
-define(is_aero_i16(Term), is_integer(Term), Term <= ?AERO_I16_MAX,
                           Term >= ?AERO_I16_MIN).
-define(is_aero_i32(Term), is_integer(Term), Term <= ?AERO_I32_MAX,
                           Term >= ?AERO_I32_MIN).
-define(is_aero_i64(Term), is_integer(Term), Term <= ?AERO_I64_MAX,
                           Term >= ?AERO_I64_MIN).

-define(is_aero_u8(Term), is_integer(Term), Term <= ?AERO_U8_MAX,
                          Term >= ?AERO_U8_MIN).
-define(is_aero_u16(Term), is_integer(Term), Term <= ?AERO_U16_MAX,
                           Term >= ?AERO_U16_MIN).
-define(is_aero_u32(Term), is_integer(Term), Term <= ?AERO_U32_MAX,
                           Term >= ?AERO_U32_MIN).
-define(is_aero_u64(Term), is_integer(Term), Term <= ?AERO_U64_MAX,
                           Term >= ?AERO_U64_MIN).

-define(is_aero_f32(Term), (is_float(Term) andalso Term >= ?AERO_F32_MIN
                              andalso Term <= ?AERO_F32_MAX),
                           orelse Term =:= nan orelse Term =:= '-nan'
                           orelse Term =:= inf orelse Term =:= '-inf').
-define(is_aero_f64(Term), is_float(Term) orelse Term =:= nan
                           orelse Term =:= '-nan' orelse Term =:= inf
                           orelse Term =:= '-inf').

-define(is_aero_bool(Term), is_boolean(Term)).

-define(is_aero_atom(Term), is_atom(Term)).
