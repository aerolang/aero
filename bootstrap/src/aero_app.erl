-module(aero_app).
-behaviour(application).

-export([start/2, stop/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

start(_Type, _Args) ->
  aero_sup:start_link().

stop(_State) ->
  ok.
