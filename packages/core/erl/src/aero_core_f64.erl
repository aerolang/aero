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

-module(aero_core_f64).

-include("aero_core_types.hrl").

% Constants.
-export(['RADIX'/0, 'MANTISSA_DIGITS'/0, 'DIGITS'/0, 'EPSILON'/0, 'MIN'/0,
         'MIN_POS'/0, 'MAX'/0, 'MAX_NEG'/0, 'MIN_EXP'/0, 'MAX_EXP'/0,
         'MIN_10_EXP'/0, 'MAX_10_EXP'/0, 'NAN'/0, 'NEG_NAN'/0, 'INF'/0,
         'NEG_INF'/0, 'PI'/0, 'E'/0]).

% Utiliies for the `f64` representation.
-export([is_nan/1, is_infinite/1, is_finite/1, is_normal/1, classify/1,
         is_sign_pos/1, is_sign_neg/1]).

% Basic arithmetic.
-export([add/2, sub/2, mul/2, 'div'/2, 'rem'/2, pos/1, neg/1]).

% Comparisons.
-export([eq/2, ne/2, lt/2, le/2, gt/2, ge/2, partial_cmp/2, min/2, max/2]).

% Various mathematical functions.
-export([floor/1, ceil/1, round/1, trunc/1, fract/1, abs/1, signum/1,
         mul_add/3, powi/2, powf/2, inv/1, sqrt/1, cbrt/1, exp/1, exp2/1,
         exp_m1/1, ln/1, log/2, log2/1, log10/1, ln_1p/1, hypot/2, sin/1,
         cos/1, tan/1, asin/1, acos/1, atan/1, atan2/2, sinh/1, cosh/1, tanh/1,
         asinh/1, acosh/1, atanh/1]).

% Convenient angle conversions.
-export([to_deg/1, to_rad/1]).

-compile(inline).

% Load the `f64` utility NIF.
-on_load(init/0).

init() ->
  Priv = code:priv_dir(aero_core),
  Library = filename:join([Priv, 'native', 'libaero_core_f64']),
  erlang:load_nif(Library, 0).

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
add(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Subtract a number from another number.
-spec sub(aero_f64(), aero_f64()) -> aero_f64().
sub(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Multiply two numbers.
-spec mul(aero_f64(), aero_f64()) -> aero_f64().
mul(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Divide a number from another number.
-spec 'div'(aero_f64(), aero_f64()) -> aero_f64().
'div'(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Get the remainder in division.
-spec 'rem'(aero_f64(), aero_f64()) -> aero_f64().
'rem'(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

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

%% Return the largest integer less than or equal to a float.
-spec floor(aero_f64()) -> aero_f64().
floor(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the smallest integer greater than or equal to a float.
-spec ceil(aero_f64()) -> aero_f64().
ceil(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the nearest integer to a float.
%%
%% Half-way cases like `-0.5` and `0.5` round away from `0.0`.
-spec round(aero_f64()) -> aero_f64().
round(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the integer part of a float.
-spec trunc(aero_f64()) -> aero_f64().
trunc(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the fractional part of a float.
-spec fract(aero_f64()) -> aero_f64().
fract(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the absolute value of a float.
-spec abs(aero_f64()) -> aero_f64().
abs(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return a number that represents the sign of a float.
%%
%% - `1.0` if the float is positive, `0.0`, or `inf`.
%% - `-1.0` if the float is negative or `-inf`.
%% - `nan` if the float is `nan` or `-nan`.
-spec signum(aero_f64()) -> aero_f64().
signum(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Fused multiply-add.
%%
%% This computes `(x * a) + b)` with only one rounding error.
-spec mul_add(aero_f64(), aero_f64(), aero_f64()) -> aero_f64().
mul_add(_X, _A, _B) ->
  erlang:nif_error(nif_not_loaded).

%% Raise a float to an integer power.
-spec powi(aero_f64(), aero_i32()) -> aero_f64().
powi(_X, _N) ->
  erlang:nif_error(nif_not_loaded).

%% Raise a float to a float power.
-spec powf(aero_f64(), aero_f64()) -> aero_f64().
powf(_X, _N) ->
  erlang:nif_error(nif_not_loaded).

%% Take the inverse of a number.
-spec inv(aero_f64()) -> aero_f64().
inv(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Take the square root of a float.
-spec sqrt(aero_f64()) -> aero_f64().
sqrt(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Take the cubic root of a float.
-spec cbrt(aero_f64()) -> aero_f64().
cbrt(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the exponential function at a float.
-spec exp(aero_f64()) -> aero_f64().
exp(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return 2 to the power of a float.
-spec exp2(aero_f64()) -> aero_f64().
exp2(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return `exp(x) - 1` in a more accurately near zero.
-spec exp_m1(aero_f64()) -> aero_f64().
exp_m1(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the natural logarithm of a float.
-spec ln(aero_f64()) -> aero_f64().
ln(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the logarithm of a float with respect to any base.
-spec log(aero_f64(), aero_f64()) -> aero_f64().
log(_X, _Base) ->
  erlang:nif_error(nif_not_loaded).

%% Return the base 2 logarithm of a number.
-spec log2(aero_f64()) -> aero_f64().
log2(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the base 10 logarithm of a number.
-spec log10(aero_f64()) -> aero_f64().
log10(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return `ln(1 + float)` in a more accurate way.
-spec ln_1p(aero_f64()) -> aero_f64().
ln_1p(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the hypotenuse of a right triangle with legs x and y.
-spec hypot(aero_f64(), aero_f64()) -> aero_f64().
hypot(_X, _Y) ->
  erlang:nif_error(nif_not_loaded).

%% Sine of a float in radians.
-spec sin(aero_f64()) -> aero_f64().
sin(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Cosine of a float in radians.
-spec cos(aero_f64()) -> aero_f64().
cos(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Tangent of a float in radians.
-spec tan(aero_f64()) -> aero_f64().
tan(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Arcsine of a float.
%%
%% Return value is in radians and in the range [`-pi/2`, `pi/2`] or
%% `nan` if the float is outside the range [-1, 1].
-spec asin(aero_f64()) -> aero_f64().
asin(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Arccosine of a float.
%%
%% Return value is in radians and in the range [`0.0`, `pi`] or `nan`
%% if the float is outside the range [-1, 1].
-spec acos(aero_f64()) -> aero_f64().
acos(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Arctangent of a float.
%%
%% Return value is in radians and in the range [`-pi/2`, `pi/2`].
-spec atan(aero_f64()) -> aero_f64().
atan(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Four quadrant arctangent of a float y and x.
%%
%% - `x == 0.0, y == 0.0 -> 0.0`
%% - `x >= 0.0 -> atan(y / x)`
%% - `y >= 0.0 -> atan(y / x) + pi`
%% - `y < 0.0 -> atan(y / x) - pi`
-spec atan2(aero_f64(), aero_f64()) -> aero_f64().
atan2(_Y, _X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic sine of a float in radians.
-spec sinh(aero_f64()) -> aero_f64().
sinh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic cosine of a float in radians.
-spec cosh(aero_f64()) -> aero_f64().
cosh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic tangent of a float in radians.
-spec tanh(aero_f64()) -> aero_f64().
tanh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic arcsine of a float.
-spec asinh(aero_f64()) -> aero_f64().
asinh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic arccosine of a float.
-spec acosh(aero_f64()) -> aero_f64().
acosh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic arctangent of a float.
-spec atanh(aero_f64()) -> aero_f64().
atanh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Convert a float in radians to degrees.
-spec to_deg(aero_f64()) -> aero_f64().
to_deg(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Convert a float in degrees to radians.
-spec to_rad(aero_f64()) -> aero_f64().
to_rad(_X) ->
  erlang:nif_error(nif_not_loaded).
