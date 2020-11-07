%%% Manages file context.

-module(aero_context).

-export([new/1]).
-export([filename/1]).

-export_type([context/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-record(context, {filename}).

-type context() :: #context{}.

-spec new(binary()) -> context().
new(Filename) ->
  #context{filename = Filename}.

-spec filename(context()) -> binary().
filename(Context) ->
  Context#context.filename.
