%%% Converts the Aero AST down to Core Aero.
%%%
%%% This format is made to resemble Core Erlang with variables renamed and
%%% macros expanded giving a simple core language. For now we don't really have
%%% macros, so they're just manually expanded here.
%%%
%%% Expansion related to definitions, expressions, patterns, and types are
%%% broken out into separate modules.

-module(aero_expand).

-export([expand/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec expand(aero_ast:t()) -> {ok, aero_core:c_pkg()} | {error, term()}.
expand(Source) ->
  Filename = aero_session:root(),
  RootName = binary_to_atom(filename:basename(Filename, ".aero"), utf8),

  Env = aero_env:new(Filename, aero_core:c_path([], [aero_core:c_var([], RootName)])),

  try expand_source(Source, Env) of
    Package -> {ok, Package}
  catch
    throw:{expand_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

expand_source({source, _, SourceArgs}, Env) ->
  PkgName = aero_session:pkg(),

  {Defs, _, InnerModules} =
    lists:foldl(fun(SourceArg, {Defs, EnvAcc, InnerModulesAcc}) ->
      {Def, NewEnv, NewModules} = aero_expand_def:expand_def(SourceArg, EnvAcc),
      {[Def | Defs], NewEnv, [NewModules | InnerModulesAcc]}
    end, {[], Env, []}, SourceArgs),

  Module = aero_core:c_mod([], aero_env:module(Env), [], lists:reverse(Defs)),

  aero_core:c_pkg([], PkgName, [Module | lists:flatten(lists:reverse(InnerModules))]);
expand_source(_, _) ->
  throw({expand_error, no_source}).
