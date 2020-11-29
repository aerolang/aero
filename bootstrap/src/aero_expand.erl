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

-spec expand(aero_ast:t(), aero_env:t()) -> {ok, aero_core:c_pkg()} | {error, term()}.
expand(Source, Env) ->
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
  ModName =
    case aero_session:pkg() of
      aero -> binary_to_atom(filename:basename(aero_env:filename(Env), ".aero"), utf8);
      Pkg  -> Pkg
    end,

  {Defs, _, InnerModules} =
    lists:foldl(fun(SourceArg, {Defs, DefEnv, InnerModules}) ->
      {Def, NewEnv} = aero_expand_def:expand_def(SourceArg, DefEnv),
      {[Def | Defs], NewEnv, InnerModules}
    end, {[], Env, []}, SourceArgs),

  ModulePath = aero_core:c_path([], [aero_core:c_var([], ModName)]),
  Module = aero_core:c_mod([], ModulePath, [], lists:reverse(Defs)),

  aero_core:c_pkg([], PkgName, [Module | lists:reverse(InnerModules)]);
expand_source(_, _) ->
  throw({expand_error, no_source}).
