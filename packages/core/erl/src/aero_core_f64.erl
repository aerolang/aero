% Part of the Aero programming language. License: MIT.

%%% Constants and utilties for the `f64` implementation.
%%%
%%% This implementation is based on the Rust `core` crate. See the
%%% [Rust core `f64` module] for source.
%%%
%%% The module API is based on the Rust `core::f64` and `std::f64`
%%% modules and the traits that Rust's `f64` implements. See the
%%% [Rust `core::f64` module] and [Rust `f64` primitive type].
%%%
%%% [Rust `core::f64` module]:
%%%   https://doc.rust-lang.org/core/f64/index.html
%%% [Rust `std::f64` module]:
%%%   https://doc.rust-lang.org/std/primitive.f64.html

% NOTE: Erlang cannot express nan, -nan, inf, -inf, and signed zeros
%       using floats. For compatibility reasons when implementing the
%       aero core module on other platforms we would expect them to
%       implement the IEEE 754 spec a bit closer. `f64` uses the nan,
%       '-nan', inf, and '-inf' atoms to mitigate this. This requires
%       some wrapping around regular mathematic operations on floats.

% FIXME: Will need to check bounds on Erlang arithmetic since it will
%        error on overflow. It might be good to implement a '-0.0'
%        atom for floats as well.

-module(aero_core_f64).

-include("aero_core_types.hrl").

-export(['RADIX'/0, 'MANTISSA_DIGITS'/0, 'DIGITS'/0, 'EPSILON'/0, 'MIN'/0,
         'MIN_POS'/0, 'MAX'/0, 'MAX_NEG'/0, 'MIN_EXP'/0, 'MAX_EXP'/0,
         'MIN_10_EXP'/0, 'MAX_10_EXP'/0, 'NAN'/0, 'NEG_NAN'/0, 'INF'/0,
         'NEG_INF'/0, 'PI'/0, 'E'/0]).

-export([is_nan/1, is_infinite/1, is_finite/1, is_normal/1, classify/1,
         is_sign_pos/1, is_sign_neg/1]).

-export([add/2, sub/2, mul/2, 'div'/2, 'rem'/2, pos/1, neg/1]).

-export([eq/2, ne/2, lt/2, le/2, gt/2, ge/2, partial_cmp/2, min/2, max/2]).

-compile(inline).

%% Radix or base of the internal representation of `f64`.
-spec 'RADIX'() -> aero_u32().
'RADIX'() ->
  2.

%% Number of significant digits in base 2.
-spec 'MANTISSA_DIGITS'() -> aero_u32().
'MANTISSA_DIGITS'() ->
  53.

%% Appoximate number of significant digits in base 10.
-spec 'DIGITS'() -> aero_u32().
'DIGITS'() ->
  15.

%% Machine epsilon for `f64`.
-spec 'EPSILON'() -> aero_f64().
'EPSILON'() ->
  2.220446049250313e-16.

%% Smallest finite `f64` value.
-spec 'MIN'() -> aero_f64().
'MIN'() ->
  ?AERO_F64_MIN.

%% Smallest positive normal `f64` value.
-spec 'MIN_POS'() -> aero_f64().
'MIN_POS'() ->
  ?AERO_F64_MIN_POS.

%% Largest finite `f64` value.
-spec 'MAX'() -> aero_f64().
'MAX'() ->
  ?AERO_F64_MAX.

%% Largest negative normal `f64` value.
-spec 'MAX_NEG'() -> aero_f64().
'MAX_NEG'() ->
  ?AERO_F64_MAX_NEG.

%% One greater than the minimum possible normal power of 2 exponent.
-spec 'MIN_EXP'() -> aero_i32().
'MIN_EXP'() ->
  -1021.

%% Maximum possible power of 2 exponent.
-spec 'MAX_EXP'() -> aero_i32().
'MAX_EXP'() ->
  1024.

%% Minimum possible normal power of 10 exponent.
-spec 'MIN_10_EXP'() -> aero_i32().
'MIN_10_EXP'() ->
  -307.

%% Maximum possible power of 10 exponent.
-spec 'MAX_10_EXP'() -> aero_i32().
'MAX_10_EXP'() ->
  308.

%% Not a Number (`nan`).
-spec 'NAN'() -> aero_f64().
'NAN'() ->
  nan.

%% Negative Not a Number (`-nan`).
-spec 'NEG_NAN'() -> aero_f64().
'NEG_NAN'() ->
  '-nan'.

%% Infinity (`inf`).
-spec 'INF'() -> aero_f64().
'INF'() ->
  inf.

%% Negative infinity (`-inf`).
-spec 'NEG_INF'() -> aero_f64().
'NEG_INF'() ->
  '-inf'.

%% Pi.
-spec 'PI'() -> aero_f64().
'PI'() ->
  3.141592653589793.

%% Euler's number.
-spec 'E'() -> aero_f64().
'E'() ->
  2.718281828459045.

%% Returns `true` if the float value is `nan`.
-spec is_nan(aero_f64()) -> aero_bool().
is_nan(nan) ->
  true;
is_nan('-nan') ->
  true;
is_nan(_Float) when is_float(_Float) ->
  false.

%% Returns `true` if the float value is infinite.
%%
%% It can either be positive or aero_bool infinity.
-spec is_infinite(aero_f64()) -> aero_bool().
is_infinite(inf) ->
  true;
is_infinite('-inf') ->
  true;
is_infinite(_Float) when is_float(_Float) ->
  false.

%% Returns `true` if the float value is not infinite or `nan`.
-spec is_finite(aero_f64()) -> aero_bool().
is_finite(nan) ->
  false;
is_finite('-nan') ->
  false;
is_finite(inf) ->
  false;
is_finite('-inf') ->
  false;
is_finite(_Float) when is_float(_Float) ->
  true.

%% Returns `true` if the number is not zero, infinite, subnormal, or
%% nan.
%%
%% Values between 0 and the minimum float are subnormal.
-spec is_normal(aero_f64()) -> aero_bool().
is_normal(Float) when is_float(Float) ->
  case classify(Float) of
    normal ->
      true;
    _ ->
      false
  end.

%% Returns the floating point category of a number.
-spec classify(aero_f64()) -> zero | infinite | subnormal | normal | nan.
classify(nan) ->
  nan;
classify('-nan') ->
  nan;
classify(inf) ->
  infinite;
classify('-inf') ->
  infinite;
classify(Float) when is_float(Float) ->
  case <<Float:64/float>> of
    <<_Sign:1, 0:63>> ->
      zero;
    <<_Sign:1, 0:11, _Mantissa:52>> ->
      subnormal;
    _ ->
      normal
  end.

%% Returns `true` if the float value has a positive sign.
%%
%% Because Erlang does not have signed zeros, a zero will always
%% return `true`.
-spec is_sign_pos(aero_f64()) -> aero_bool().
is_sign_pos(nan) ->
  true;
is_sign_pos('-nan') ->
  false;
is_sign_pos(inf) ->
  true;
is_sign_pos('-inf') ->
  false;
is_sign_pos(Float) when is_float(Float) ->
  case <<Float:64/float>> of
    <<0:1, _Exponent:11, _Mantissa:52>> ->
      true;
    _ ->
      false
  end.

%% Returns `true` if the float value has a negative sign.
%%
%% See the note for `is_sign_pos`.
-spec is_sign_neg(aero_f64()) -> aero_bool().
is_sign_neg(Float) when is_float(Float) ->
  not is_sign_pos(Float).

%% Add two numbers.
-spec add(aero_f64(), aero_f64()) -> aero_f64().
add(nan, Right) when ?is_aero_f64(Right) ->
  nan;
add('-nan', Right) when ?is_aero_f64(Right) ->
  '-nan';
add(Left, nan) when ?is_aero_f64(Left) ->
  nan;
add(Left, '-nan') when ?is_aero_f64(Left) ->
  '-nan';
add(inf, inf) ->
  inf;
add(inf, '-inf') ->
  nan;
add(inf, Right) when is_float(Right) ->
  inf;
add('-inf', inf) ->
  nan;
add('-inf', '-inf') ->
  '-inf';
add('-inf', Right) when is_float(Right) ->
  '-inf';
add(Left, inf) when is_float(Left) ->
  inf;
add(Left, '-inf') when is_float(Left) ->
  '-inf';
add(Left, Right) when is_float(Left), is_float(Right) ->
  Left + Right.

%% Subtract a number from another number.
-spec sub(aero_f64(), aero_f64()) -> aero_f64().
sub(nan, Right) when ?is_aero_f64(Right) ->
  nan;
sub('-nan', Right) when ?is_aero_f64(Right) ->
  '-nan';
sub(Left, nan) when ?is_aero_f64(Left) ->
  '-nan';
sub(Left, '-nan') when ?is_aero_f64(Left) ->
  nan;
sub(inf, inf) ->
  nan;
sub(inf, '-inf') ->
  inf;
sub(inf, Right) when is_float(Right) ->
  inf;
sub('-inf', inf) ->
  '-inf';
sub('-inf', '-inf') ->
  nan;
sub('-inf', Right) when is_float(Right) ->
  '-inf';
sub(Left, inf) when is_float(Left) ->
  '-inf';
sub(Left, '-inf') when is_float(Left) ->
  inf;
sub(Left, Right) when is_float(Left), is_float(Right) ->
  Left - Right.

%% Multiply two numbers.
-spec mul(aero_f64(), aero_f64()) -> aero_f64().
mul(nan, Right) when ?is_aero_f64(Right) ->
  nan;
mul('-nan', Right) when ?is_aero_f64(Right) ->
  nan;
mul(Left, nan) when ?is_aero_f64(Left) ->
  nan;
mul(Left, '-nan') when ?is_aero_f64(Left) ->
  nan;
mul(inf, inf) ->
  inf;
mul(inf, '-inf') ->
  '-inf';
mul(inf, 0.0) ->
  nan;
mul(inf, Right) when is_float(Right), Right > 0.0 ->
  inf;
mul(inf, Right) when is_float(Right), Right < 0.0 ->
  '-inf';
mul('-inf', inf) ->
  '-inf';
mul('-inf', '-inf') ->
  inf;
mul('-inf', 0.0) ->
  nan;
mul('-inf', Right) when is_float(Right), Right > 0.0 ->
  '-inf';
mul('-inf', Right) when is_float(Right), Right < 0.0 ->
  inf;
mul(0.0, inf) ->
  nan;
mul(0.0, '-inf') ->
  nan;
mul(Left, inf) when is_float(Left), Left > 0.0 ->
  inf;
mul(Left, '-inf') when is_float(Left), Left < 0.0 ->
  '-inf';
mul(Left, Right) when is_float(Left), is_float(Right) ->
  Left * Right.

%% Divide a number from another number.
-spec 'div'(aero_f64(), aero_f64()) -> aero_f64().
'div'(nan, Right) when ?is_aero_f64(Right) ->
  nan;
'div'('-nan', Right) when ?is_aero_f64(Right) ->
  nan;
'div'(Left, nan) when ?is_aero_f64(Left) ->
  nan;
'div'(Left, '-nan') when ?is_aero_f64(Left) ->
  nan;
'div'(inf, inf) ->
  nan;
'div'(inf, '-inf') ->
  nan;
'div'(inf, Right) when is_float(Right), Right >= 0.0 ->
  inf;
'div'(inf, Right) when is_float(Right), Right < 0.0 ->
  '-inf';
'div'('-inf', inf) ->
  nan;
'div'('-inf', '-inf') ->
  nan;
'div'('-inf', Right) when is_float(Right), Right >= 0.0 ->
  '-inf';
'div'('-inf', Right) when is_float(Right), Right < 0.0 ->
  inf;
'div'(Left, inf) when is_float(Left), Left > 0.0 ->
  0.0;
'div'(Left, '-inf') when is_float(Left), Left < 0.0 ->
  0.0;
'div'(0.0, 0.0) ->
  nan;
'div'(Left, 0.0) when is_float(Left), Left > 0 ->
  inf;
'div'(Left, 0.0) when is_float(Left), Left < 0 ->
  '-inf';
'div'(Left, Right) when is_float(Left), is_float(Right) ->
  Left / Right.

%% Get the remainder in division.
-spec 'rem'(aero_f64(), aero_f64()) -> aero_f64().
'rem'(nan, Right) when ?is_aero_f64(Right) ->
  nan;
'rem'('-nan', Right) when ?is_aero_f64(Right) ->
  '-nan';
'rem'(Left, nan) when ?is_aero_f64(Left) ->
  nan;
'rem'(Left, '-nan') when ?is_aero_f64(Left) ->
  nan;
'rem'(inf, inf) ->
  nan;
'rem'(inf, '-inf') ->
  nan;
'rem'(inf, Right) when is_float(Right) ->
  nan;
'rem'('-inf', inf) ->
  nan;
'rem'('-inf', '-inf') ->
  nan;
'rem'('-inf', Right) when is_float(Right) ->
  nan;
'rem'(Left, inf) when is_float(Left) ->
  Left;
'rem'(Left, '-inf') when is_float(Left) ->
  Left;
'rem'(Left, 0.0) when is_float(Left) ->
  nan;
'rem'(Left, Right) when is_float(Left), is_float(Right) ->
  math:fmod(Left, Right).

%% Returns back a float.
-spec pos(aero_f64()) -> aero_f64().
pos(nan) ->
  nan;
pos('-nan') ->
  '-nan';
pos(inf) ->
  inf;
pos('-inf') ->
  '-inf';
pos(Float) when is_float(Float) ->
  Float.

%% Negates a float.
-spec neg(aero_f64()) -> aero_f64().
neg(nan) ->
  '-nan';
neg('-nan') ->
  nan;
neg(inf) ->
  '-inf';
neg('-inf') ->
  inf;
neg(Float) when is_float(Float) ->
  -Float.

%% Check two floats for equality.
-spec eq(aero_f64(), aero_f64()) -> aero_bool().
eq(nan, Right) when ?is_aero_f64(Right) ->
  false;
eq('-nan', Right) when ?is_aero_f64(Right) ->
  false;
eq(Left, nan) when ?is_aero_f64(Left) ->
  false;
eq(Left, '-nan') when ?is_aero_f64(Left) ->
  false;
eq(inf, inf) ->
  true;
eq(inf, '-inf') ->
  false;
eq(inf, Right) when is_float(Right) ->
  false;
eq('-inf', inf) ->
  false;
eq('-inf', '-inf') ->
  true;
eq('-inf', Right) when is_float(Right) ->
  false;
eq(Left, inf) when is_float(Left) ->
  false;
eq(Left, '-inf') when is_float(Left) ->
  false;
eq(Left, Right) when is_float(Left), is_float(Right) ->
  Left =:= Right.

%% Check if two floats are not equal.
-spec ne(aero_f64(), aero_f64()) -> aero_bool().
ne(nan, Right) when ?is_aero_f64(Right) ->
  true;
ne('-nan', Right) when ?is_aero_f64(Right) ->
  true;
ne(Left, nan) when ?is_aero_f64(Left) ->
  true;
ne(Left, '-nan') when ?is_aero_f64(Left) ->
  true;
ne(inf, inf) ->
  false;
ne(inf, '-inf') ->
  true;
ne(inf, Right) when is_float(Right) ->
  false;
ne('-inf', inf) ->
  true;
ne('-inf', '-inf') ->
  false;
ne('-inf', Right) when is_float(Right) ->
  true;
ne(Left, inf) when is_float(Left) ->
  true;
ne(Left, '-inf') when is_float(Left) ->
  true;
ne(Left, Right) when is_float(Left), is_float(Right) ->
  Left =/= Right.

%% true` if the left is less than the right.
-spec lt(aero_f64(), aero_f64()) -> aero_bool().
lt(nan, Right) when ?is_aero_f64(Right) ->
  false;
lt('-nan', Right) when ?is_aero_f64(Right) ->
  false;
lt(Left, nan) when ?is_aero_f64(Left) ->
  false;
lt(Left, '-nan') when ?is_aero_f64(Left) ->
  false;
lt(inf, inf) ->
  false;
lt(inf, '-inf') ->
  false;
lt(inf, Right) when is_float(Right) ->
  false;
lt('-inf', inf) ->
  true;
lt('-inf', '-inf') ->
  false;
lt('-inf', Right) when is_float(Right) ->
  true;
lt(Left, inf) when is_float(Left) ->
  true;
lt(Left, '-inf') when is_float(Left) ->
  false;
lt(Left, Right) when is_float(Left), is_float(Right) ->
  Left < Right.

%% `true` if the left is less than or equal to the right.
-spec le(aero_f64(), aero_f64()) -> aero_bool().
le(nan, Right) when ?is_aero_f64(Right) ->
  false;
le('-nan', Right) when ?is_aero_f64(Right) ->
  false;
le(Left, nan) when ?is_aero_f64(Left) ->
  false;
le(Left, '-nan') when ?is_aero_f64(Left) ->
  false;
le(inf, inf) ->
  true;
le(inf, '-inf') ->
  false;
le(inf, Right) when is_float(Right) ->
  false;
le('-inf', inf) ->
  true;
le('-inf', '-inf') ->
  true;
le('-inf', Right) when is_float(Right) ->
  true;
le(Left, inf) when is_float(Left) ->
  true;
le(Left, '-inf') when is_float(Left) ->
  false;
le(Left, Right) when is_float(Left), is_float(Right) ->
  Left =< Right.

%% `true` if the left is greater than the right.
-spec gt(aero_f64(), aero_f64()) -> aero_bool().
gt(nan, Right) when ?is_aero_f64(Right) ->
  false;
gt('-nan', Right) when ?is_aero_f64(Right) ->
  false;
gt(Left, nan) when ?is_aero_f64(Left) ->
  false;
gt(Left, '-nan') when ?is_aero_f64(Left) ->
  false;
gt(inf, inf) ->
  false;
gt(inf, '-inf') ->
  true;
gt(inf, Right) when is_float(Right) ->
  true;
gt('-inf', inf) ->
  false;
gt('-inf', '-inf') ->
  false;
gt('-inf', Right) when is_float(Right) ->
  false;
gt(Left, inf) when is_float(Left) ->
  false;
gt(Left, '-inf') when is_float(Left) ->
  true;
gt(Left, Right) when is_float(Left), is_float(Right) ->
  Left > Right.

%% `true` if the left is greater than or equal to the right.
-spec ge(aero_f64(), aero_f64()) -> aero_bool().
ge(nan, Right) when ?is_aero_f64(Right) ->
  false;
ge('-nan', Right) when ?is_aero_f64(Right) ->
  false;
ge(Left, nan) when ?is_aero_f64(Left) ->
  false;
ge(Left, '-nan') when ?is_aero_f64(Left) ->
  false;
ge(inf, inf) ->
  true;
ge(inf, '-inf') ->
  true;
ge(inf, Right) when is_float(Right) ->
  true;
ge('-inf', inf) ->
  false;
ge('-inf', '-inf') ->
  true;
ge('-inf', Right) when is_float(Right) ->
  false;
ge(Left, inf) when is_float(Left) ->
  false;
ge(Left, '-inf') when is_float(Left) ->
  true;
ge(Left, Right) when is_float(Left), is_float(Right) ->
  Left >= Right.

%% Return the partial ordering of the left and right.
-spec partial_cmp(aero_f64(), aero_f64()) -> {some, less} | {some, equal}
                                             | {some, greater} | none.
partial_cmp(nan, Right) when ?is_aero_f64(Right) ->
  false;
partial_cmp('-nan', Right) when ?is_aero_f64(Right) ->
  none;
partial_cmp(Left, nan) when ?is_aero_f64(Left) ->
  none;
partial_cmp(Left, '-nan') when ?is_aero_f64(Left) ->
  none;
partial_cmp(inf, inf) ->
  {some, equal};
partial_cmp(inf, '-inf') ->
  {some, less};
partial_cmp(inf, Right) when is_float(Right) ->
  {some, less};
partial_cmp('-inf', inf) ->
  {some, greater};
partial_cmp('-inf', '-inf') ->
  {some, equal};
partial_cmp('-inf', Right) when is_float(Right) ->
  {some, greater};
partial_cmp(Left, inf) when is_float(Left) ->
  {some, less};
partial_cmp(Left, '-inf') when is_float(Left) ->
  {some, greater};
partial_cmp(0.0, 0.0) ->
  {some, equal};
partial_cmp(Left, Right) when is_float(Left), is_float(Right), Left < Right ->
  {some, less};
partial_cmp(Left, Right) when is_float(Left), is_float(Right), Right > Left ->
  {some, greater}.

%% Return the minimum of two floats.
-spec min(aero_f64(), aero_f64()) -> aero_f64().
min(nan, nan) ->
  nan;
min(nan, '-nan') ->
  nan;
min(nan, inf) ->
  inf;
min(nan, Right) when is_float(Right) ->
  Right;
min('-nan', nan) ->
  nan;
min('-nan', '-nan') ->
  nan;
min('-nan', inf) ->
  inf;
min('-nan', Right) when is_float(Right) ->
  Right;
min(inf, inf) ->
  inf;
min(inf, nan) ->
  inf;
min(inf, '-nan') ->
  inf;
min(inf, Right) when is_float(Right) ->
  Right;
min('-inf', Right) when ?is_aero_f64(Right) ->
  '-inf';
min(Left, nan) when is_float(Left) ->
  Left;
min(Left, '-nan') when is_float(Left) ->
  Left;
min(Left, inf) when is_float(Left) ->
  Left;
min(Left, '-inf') when ?is_aero_f64(Left) ->
  '-inf';
min(Left, Right) when is_float(Left), is_float(Right) ->
  erlang:min(Left, Right).

%% Return the maximum of two floats.
-spec max(aero_f64(), aero_f64()) -> aero_f64().
max(nan, nan) ->
  nan;
max(nan, '-nan') ->
  nan;
max(nan, '-inf') ->
  '-inf';
max(nan, Right) when is_float(Right) ->
  Right;
max('-nan', nan) ->
  nan;
max('-nan', '-nan') ->
  nan;
max('-nan', '-inf') ->
  '-inf';
max('-nan', Right) when is_float(Right) ->
  Right;
max(inf, Right) when ?is_aero_f64(Right) ->
  inf;
max('-inf', inf) ->
  '-inf';
max('-inf', nan) ->
  '-inf';
max('-inf', '-nan') ->
  '-inf';
max('-inf', Right) when is_float(Right) ->
  Right;
max(Left, nan) when is_float(Left) ->
  Left;
max(Left, '-nan') when is_float(Left) ->
  Left;
max(Left, inf) when ?is_aero_f64(Left) ->
  inf;
max(Left, '-inf') when is_float(Left) ->
  Left;
max(Left, Right) when is_float(Left), is_float(Right) ->
  erlang:max(Left, Right).
