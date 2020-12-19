%%% Converts the Aero AST down to Core Aero.
%%%
%%% This format is made to resemble Core Erlang with variables renamed and
%%% macros expanded giving a simple core language. For now we don't really have
%%% macros, so they're just manually expanded here.
%%%
%%% Expansion related to definitions, expressions, patterns, and types are
%%% broken out into separate modules.

-module(aero_expand).

-export([expand/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec expand(aero_ast:t(), aero_session:t()) -> {ok, aero_core:c_pkg()} | {error, term()}.
expand(Source, Session) ->
  try expand_source(Source, Session) of
    Package -> {ok, Package}
  catch
    throw:{expand_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

expand_source({source, _, SourceArgs}, Session) ->
  Filename = aero_session:root(Session),
  RootName = aero_session:pkg(Session),

  % Creating an environment from the root name. A `b` is prefixed as it's the
  % prefix for Aero Bootstrap modules.
  RootMod = aero_core:c_path([], [aero_core:c_var([], b), aero_core:c_var([], RootName)]),
  Env = aero_env:new(Filename, RootMod),

  {Defs, _, InnerModules} = aero_expand_def:expand_defs(SourceArgs, Env),
  Module = aero_core:c_mod([], aero_env:module(Env), [], Defs),

  aero_core:c_pkg([], RootName, [Module | InnerModules]);
expand_source(_, _) ->
  throw({expand_error, no_source}).
