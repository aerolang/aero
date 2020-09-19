%%% References to a span of the source string.
%%%
%%% The stopping point of a span is not inclusive. Index ignores graphemes.

-module(aero_span).

-export([new/2, merge/2, disjoint/2, start/1, stop/1]).

-export_type([span/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-record(span, {start :: non_neg_integer(), stop :: non_neg_integer()}).
-type span() :: #span{}.

%% Create a new span with start and stop positions.
-spec new(non_neg_integer(), non_neg_integer()) -> span().
new(Start, Stop) ->
  #span{start = Start, stop = Stop}.

%% Combine spans even if they are disjoint.
-spec merge(span(), span()) -> span().
merge(LeftSpan, RightSpan) ->
  Start = min(start(LeftSpan), start(RightSpan)),
  Stop = max(stop(LeftSpan), stop(RightSpan)),
  new(Start, Stop).

%% Check if two spans don't overlap.
-spec disjoint(span(), span()) -> boolean().
disjoint(LeftSpan, RightSpan) ->
  case stop(LeftSpan) < stop(RightSpan) of
    true  -> stop(LeftSpan) =< start(RightSpan);
    false -> stop(RightSpan) =< start(LeftSpan)
  end.

%% Get the beginning of a span.
-spec start(span()) -> non_neg_integer().
start(#span{start = Start}) ->
  Start.

%% Get the end of a span (not inclusive).
-spec stop(span()) -> non_neg_integer().
stop(#span{stop = Stop}) ->
  Stop.
