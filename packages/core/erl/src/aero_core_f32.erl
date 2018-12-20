% Part of the Aero programming language. License: MIT.

%%% Constants and utilties for the `f32` implementation.
%%%
%%% This is implemented similar to `f64`. Note that Erlang only
%%% supports double precision floats and so this module simply
%%% removes precision off of normal Erlang floats. *They are still
%%% stored as double precision floats, but will stay within the
%%% bounds and precision of single precisi on floats.*

% NOTE: Since we can't type a `f32` literal in Erlang, the `f32`
%       constants are Erlang floats that internally have zeros at the
%       end of the mantissa. They are long due to the float to string
%       conversions. This module copies the API of `aero_core_f64`.

-module(aero_core_f32).

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

-define(unsafe_from_f64(F64), case F64 of
                                nan      -> nan;
                                '-nan'   -> '-nan';
                                inf      -> inf;
                                '-inf'   -> '-inf';
                                ErlFloat ->
                                  <<F32:32/float>> = <<ErlFloat:32/float>>,
                                  F32
                              end).

% Load the `f32` utility NIF.
-on_load(init/0).

init() ->
  Priv = code:priv_dir(aero_core),
  Library = filename:join([Priv, 'native', 'libaero_core_f32']),
  erlang:load_nif(Library, 0).

%% Radix or base of the internal representation of `f32`.
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

%% Machine epsilon for `f32`.
-spec 'EPSILON'() -> aero_f32().
'EPSILON'() ->
  2.220446049250313e-16.

%% Smallest finite `f32` value.
-spec 'MIN'() -> aero_f32().
'MIN'() ->
  ?AERO_F32_MIN.

%% Smallest positive normal `f32` value.
-spec 'MIN_POS'() -> aero_f32().
'MIN_POS'() ->
  ?AERO_F32_MIN_POS.

%% Largest finite `f32` value.
-spec 'MAX'() -> aero_f32().
'MAX'() ->
  ?AERO_F32_MAX.

%% Largest negative normal `f32` value.
-spec 'MAX_NEG'() -> aero_f32().
'MAX_NEG'() ->
  ?AERO_F32_MAX_NEG.

%% One greater than the minimum possible normal power of 2 exponent.
-spec 'MIN_EXP'() -> aero_i32().
'MIN_EXP'() ->
  -125.

%% Maximum possible power of 2 exponent.
-spec 'MAX_EXP'() -> aero_i32().
'MAX_EXP'() ->
  128.

%% Minimum possible normal power of 10 exponent.
-spec 'MIN_10_EXP'() -> aero_i32().
'MIN_10_EXP'() ->
  -37.

%% Maximum possible power of 10 exponent.
-spec 'MAX_10_EXP'() -> aero_i32().
'MAX_10_EXP'() ->
  38.

%% Not a Number (`nan`).
-spec 'NAN'() -> aero_f32().
'NAN'() ->
  nan.

%% Negative Not a Number (`-nan`).
-spec 'NEG_NAN'() -> aero_f32().
'NEG_NAN'() ->
  '-nan'.

%% Infinity (`inf`).
-spec 'INF'() -> aero_f32().
'INF'() ->
  inf.

%% Negative infinity (`-inf`).
-spec 'NEG_INF'() -> aero_f32().
'NEG_INF'() ->
  '-inf'.

%% Pi.
-spec 'PI'() -> aero_f32().
'PI'() ->
  3.1415927410125732.

%% Euler's number.
-spec 'E'() -> aero_f32().
'E'() ->
  2.7182817459106445.

%% Returns `true` if the float value is `nan`.
-spec is_nan(aero_f32()) -> aero_bool().
is_nan(Float) ->
  aero_core_f64:is_nan(Float).

%% Returns `true` if the float value is infinite.
%%
%% It can either be positive or aero_bool infinity.
-spec is_infinite(aero_f32()) -> aero_bool().
is_infinite(Float) ->
  aero_core_f64:is_infinite(Float).

%% Returns `true` if the float value is not infinite or `nan`.
-spec is_finite(aero_f32()) -> aero_bool().
is_finite(Float) ->
  aero_core_f64:is_finite(Float).

%% Returns `true` if the number is not zero, infinite, subnormal, or
%% nan.
%%
%% Values between 0 and the minimum float are subnormal.
-spec is_normal(aero_f32()) -> aero_bool().
is_normal(Float) ->
  aero_core_f64:is_normal(Float).

%% Returns the floating point category of a number.
-spec classify(aero_f32()) -> zero | infinite | subnormal | normal | nan.
classify(Float) ->
  aero_core_f64:classify(Float).

%% Returns `true` if the float value has a positive sign.
%%
%% Because Erlang does not have signed zeros, a zero will always
%% return `true`.
-spec is_sign_pos(aero_f32()) -> aero_bool().
is_sign_pos(Float) ->
  aero_core_f64:is_sign_pos(Float).

%% Returns `true` if the float value has a negative sign.
%%
%% See the note for `is_sign_pos`.
-spec is_sign_neg(aero_f32()) -> aero_bool().
is_sign_neg(Float) ->
  aero_core_f64:is_sign_neg(Float).

%% Add two numbers.
-spec add(aero_f32(), aero_f32()) -> aero_f32().
add(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Subtract a number from another number.
-spec sub(aero_f32(), aero_f32()) -> aero_f32().
sub(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Multiply two numbers.
-spec mul(aero_f32(), aero_f32()) -> aero_f32().
mul(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Divide a number from another number.
-spec 'div'(aero_f32(), aero_f32()) -> aero_f32().
'div'(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Get the remainder in division.
-spec 'rem'(aero_f32(), aero_f32()) -> aero_f32().
'rem'(_Left, _Right) ->
  erlang:nif_error(nif_not_loaded).

%% Returns back a float.
-spec pos(aero_f32()) -> aero_f32().
pos(Float) ->
  ?unsafe_from_f64(aero_core_f64:pos(Float)).

%% Negates a float.
-spec neg(aero_f32()) -> aero_f32().
neg(Float) ->
  ?unsafe_from_f64(aero_core_f64:neg(Float)).

%% Check two floats for equality.
-spec eq(aero_f32(), aero_f32()) -> aero_bool().
eq(Left, Right) ->
  aero_core_f64:eq(Left, Right).

%% Check if two floats are not equal.
-spec ne(aero_f32(), aero_f32()) -> aero_bool().
ne(Left, Right) ->
  aero_core_f64:ne(Left, Right).

%% true` if the left is less than the right.
-spec lt(aero_f32(), aero_f32()) -> aero_bool().
lt(Left, Right) ->
  aero_core_f64:lt(Left, Right).

%% `true` if the left is less than or equal to the right.
-spec le(aero_f32(), aero_f32()) -> aero_bool().
le(Left, Right) ->
  aero_core_f64:le(Left, Right).

%% `true` if the left is greater than the right.
-spec gt(aero_f32(), aero_f32()) -> aero_bool().
gt(Left, Right) ->
  aero_core_f64:gt(Left, Right).

%% `true` if the left is greater than or equal to the right.
-spec ge(aero_f32(), aero_f32()) -> aero_bool().
ge(Left, Right) ->
  aero_core_f64:ge(Left, Right).

%% Return the partial ordering of the left and right.
-spec partial_cmp(aero_f32(), aero_f32()) -> {some, less} | {some, equal}
                                             | {some, greater} | none.
partial_cmp(Left, Right) ->
  aero_core_f64:partial_cmp(Left, Right).

%% Return the minimum of two floats.
-spec min(aero_f32(), aero_f32()) -> aero_f32().
min(Left, Right) ->
  aero_core_f64:min(Left, Right).

%% Return the maximum of two floats.
-spec max(aero_f32(), aero_f32()) -> aero_f32().
max(Left, Right) ->
  aero_core_f64:max(Left, Right).

%% Return the largest integer less than or equal to a float.
-spec floor(aero_f32()) -> aero_f32().
floor(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the smallest integer greater than or equal to a float.
-spec ceil(aero_f32()) -> aero_f32().
ceil(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the nearest integer to a float.
%%
%% Half-way cases like `-0.5` and `0.5` round away from `0.0`.
-spec round(aero_f32()) -> aero_f32().
round(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the integer part of a float.
-spec trunc(aero_f32()) -> aero_f32().
trunc(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the fractional part of a float.
-spec fract(aero_f32()) -> aero_f32().
fract(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the absolute value of a float.
-spec abs(aero_f32()) -> aero_f32().
abs(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return a number that represents the sign of a float.
%%
%% - `1.0` if the float is positive, `0.0`, or `inf`.
%% - `-1.0` if the float is negative or `-inf`.
%% - `nan` if the float is `nan` or `-nan`.
-spec signum(aero_f32()) -> aero_f32().
signum(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Fused multiply-add.
%%
%% This computes `(x * a) + b)` with only one rounding error.
-spec mul_add(aero_f32(), aero_f32(), aero_f32()) -> aero_f32().
mul_add(_X, _A, _B) ->
  erlang:nif_error(nif_not_loaded).

%% Raise a float to an integer power.
-spec powi(aero_f32(), aero_i32()) -> aero_f32().
powi(_X, _N) ->
  erlang:nif_error(nif_not_loaded).

%% Raise a float to a float power.
-spec powf(aero_f32(), aero_f32()) -> aero_f32().
powf(_X, _N) ->
  erlang:nif_error(nif_not_loaded).

%% Take the inverse of a number.
-spec inv(aero_f32()) -> aero_f32().
inv(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Take the square root of a float.
-spec sqrt(aero_f32()) -> aero_f32().
sqrt(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Take the cubic root of a float.
-spec cbrt(aero_f32()) -> aero_f32().
cbrt(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the exponential function at a float.
-spec exp(aero_f32()) -> aero_f32().
exp(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return 2 to the power of a float.
-spec exp2(aero_f32()) -> aero_f32().
exp2(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return `exp(x) - 1` in a more accurately near zero.
-spec exp_m1(aero_f32()) -> aero_f32().
exp_m1(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the natural logarithm of a float.
-spec ln(aero_f32()) -> aero_f32().
ln(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the logarithm of a float with respect to any base.
-spec log(aero_f32(), aero_f32()) -> aero_f32().
log(_X, _Base) ->
  erlang:nif_error(nif_not_loaded).

%% Return the base 2 logarithm of a number.
-spec log2(aero_f32()) -> aero_f32().
log2(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the base 10 logarithm of a number.
-spec log10(aero_f32()) -> aero_f32().
log10(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return `ln(1 + float)` in a more accurate way.
-spec ln_1p(aero_f32()) -> aero_f32().
ln_1p(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Return the hypotenuse of a right triangle with legs x and y.
-spec hypot(aero_f32(), aero_f32()) -> aero_f32().
hypot(_X, _Y) ->
  erlang:nif_error(nif_not_loaded).

%% Sine of a float in radians.
-spec sin(aero_f32()) -> aero_f32().
sin(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Cosine of a float in radians.
-spec cos(aero_f32()) -> aero_f32().
cos(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Tangent of a float in radians.
-spec tan(aero_f32()) -> aero_f32().
tan(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Arcsine of a float.
%%
%% Return value is in radians and in the range [`-pi/2`, `pi/2`] or
%% `nan` if the float is outside the range [-1, 1].
-spec asin(aero_f32()) -> aero_f32().
asin(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Arccosine of a float.
%%
%% Return value is in radians and in the range [`0.0`, `pi`] or `nan`
%% if the float is outside the range [-1, 1].
-spec acos(aero_f32()) -> aero_f32().
acos(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Arctangent of a float.
%%
%% Return value is in radians and in the range [`-pi/2`, `pi/2`].
-spec atan(aero_f32()) -> aero_f32().
atan(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Four quadrant arctangent of a float y and x.
%%
%% - `x == 0.0, y == 0.0 -> 0.0`
%% - `x >= 0.0 -> atan(y / x)`
%% - `y >= 0.0 -> atan(y / x) + pi`
%% - `y < 0.0 -> atan(y / x) - pi`
-spec atan2(aero_f32(), aero_f32()) -> aero_f32().
atan2(_Y, _X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic sine of a float in radians.
-spec sinh(aero_f32()) -> aero_f32().
sinh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic cosine of a float in radians.
-spec cosh(aero_f32()) -> aero_f32().
cosh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic tangent of a float in radians.
-spec tanh(aero_f32()) -> aero_f32().
tanh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic arcsine of a float.
-spec asinh(aero_f32()) -> aero_f32().
asinh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic arccosine of a float.
-spec acosh(aero_f32()) -> aero_f32().
acosh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Hyperbolic arctangent of a float.
-spec atanh(aero_f32()) -> aero_f32().
atanh(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Convert a float in radians to degrees.
-spec to_deg(aero_f32()) -> aero_f32().
to_deg(_X) ->
  erlang:nif_error(nif_not_loaded).

%% Convert a float in degrees to radians.
-spec to_rad(aero_f32()) -> aero_f32().
to_rad(_X) ->
  erlang:nif_error(nif_not_loaded).
