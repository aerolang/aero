%%% Holds options from the user for compiling.

-module(aero_session).

-export([new/2]).
-export([root/1, pkg/1, out_dir/1, mode/1]).

-export_type([t/0, mode/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-record(session, {root    :: binary(),
                  pkg     :: atom(),
                  out_dir :: binary(),
                  mode    :: mode()}).

%% Aero compiler session.
-type t() :: #session{}.

-type mode() :: beam | escript | core.

%% Create a new session.
-spec new(binary(), [term()]) -> t().
new(Root, Options) ->
  #session{
    root = Root,
    pkg = binary_to_atom(filename:basename(Root, ".aero"), utf8),
    out_dir = proplists:get_value(out_dir, Options),
    mode = proplists:get_value(mode, Options, beam)
  }.

%% Get the root Aero file.
-spec root(t()) -> binary().
root(Session) ->
  Session#session.root.

%% Get the current package name.
-spec pkg(t()) -> atom().
pkg(Session) ->
  Session#session.pkg.

%% Get the output directory.
-spec out_dir(t()) -> binary().
out_dir(Session) ->
  Session#session.out_dir.

%% Get the compilation mode.
-spec mode(t()) -> mode().
mode(Session) ->
  Session#session.mode.
