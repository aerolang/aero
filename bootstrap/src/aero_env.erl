%%% Aero scope environment information.
%%%
%%% The environment tracks the filename along with definitions, variables, and
%%% pattern variables.
%%%
%%% Definitions are tracked to make to possible to detect a duplicate definition
%%% name in the same scope. Variables are created with numbers at the end to be
%%% able to keep them seperate, this is done with a counter. Pattern variables
%%% use the same number suffixes.
%%%
%%% Type variables are available as well, but have a constant suffix to ensure
%%% they don't end up conflicting with intermediate inferred variables. Type
%%% vars can't shadow one another so incrementing numbers aren't required.

-module(aero_env).

-export([new/1, new/2, filename/1, defs/1, vars/1, pat_vars/1, type_vars/1, counter/1]).
-export([lookup_def/2, register_def/2, clear_defs/1, lookup_var/2, register_var/2, tmp_var/1,
         clear_vars/1, lookup_pat_var/2, register_pat_var/2, wildcard_pat_var/1, clear_pat_vars/1,
         lookup_type_var/2, register_type_var/2, inferred_type_var/1, clear_type_vars/1,
         clear_all_vars/1, reset_counter/1]).

-export_type([t/0, counter/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-record(env, {filename       :: binary(),
              defs      = [] :: [{atom(), aero_core:c_path()}],
              vars      = [] :: [{atom(), aero_core:c_var()}],
              pat_vars  = [] :: [{atom(), aero_core:c_pat_var()}],
              type_vars = [] :: [{atom(), aero_core:c_type_var()}],
              counter        :: counter()}).

%% Environment.
-type t() :: #env{}.

%% Environment variable counter.
-opaque counter() :: counters:counters_ref().

%% Create a new environment.
-spec new(binary()) -> t().
new(Filename) ->
  #env{filename = Filename, counter = counters:new(1, [])}.

%% Create an environment with an existing counter.
-spec new(binary(), counter()) -> t().
new(Filename, Counter) ->
  #env{filename = Filename, counter = Counter}.

%% Get the filename.
-spec filename(t()) -> binary().
filename(Env) ->
  Env#env.filename.

%% Get all definitions.
-spec defs(t()) -> [aero_core:c_path()].
defs(Env) ->
  element(2, lists:unzip(Env#env.defs)).

%% Get all variables.
-spec vars(t()) -> [aero_core:c_var()].
vars(Env) ->
  element(2, lists:unzip(Env#env.vars)).

%% Get all pattern variables.
-spec pat_vars(t()) -> [aero_core:c_pat_var()].
pat_vars(Env) ->
  element(2, lists:unzip(Env#env.pat_vars)).

%% Get all type variables.
-spec type_vars(t()) -> [aero_core:c_type_var()].
type_vars(Env) ->
  element(2, lists:unzip(Env#env.type_vars)).

%% Get the counter.
-spec counter(t()) -> counter().
counter(Env) ->
  Env#env.counter.

%% Get a definition by its name.
-spec lookup_def(t(), aero_ast:ident()) -> aero_core:c_path() | undefined.
lookup_def(Env, {ident, _, IdentName}) ->
  case proplists:get_value(IdentName, Env#env.defs) of
    undefined -> undefined;
    Def       -> Def
  end.

%% Create a definition from an identifier and save it to the env.
-spec register_def(t(), aero_ast:ident()) -> {t(), aero_core:c_path()}.
register_def(Env, {ident, _, IdentName}) ->
  Path = aero_core:c_path([], [aero_core:c_var([], IdentName)]),

  {Env#env{defs = [{IdentName, Path} | Env#env.defs]}, Path}.

%% Clear definitions in the environment.
-spec clear_defs(t()) -> t().
clear_defs(Env) ->
  Env#env{defs = []}.

%% Get a variable by its name.
-spec lookup_var(t(), aero_ast:ident()) -> aero_core:c_var() | undefined.
lookup_var(Env, {ident, _, IdentName}) ->
  case proplists:get_value(IdentName, Env#env.vars) of
    undefined -> undefined;
    Var       -> Var
  end.

%% Create a fresh variable name from an identifier and save it to the env.
-spec register_var(t(), aero_ast:ident()) -> {t(), aero_core:c_var()}.
register_var(Env, {ident, _, IdentName}) ->
  Num = incr_counter(Env),
  VarName = list_to_atom(atom_to_list(IdentName) ++ "_" ++ integer_to_list(Num)),
  Var = aero_core:c_var([], VarName),

  {Env#env{vars = [{IdentName, Var} | Env#env.vars]}, Var}.

%% Create a temporary variable, not saved to the environment.
-spec tmp_var(t()) -> aero_core:c_var().
tmp_var(Env) ->
  Num = incr_counter(Env),
  aero_core:c_var([], list_to_atom("_" ++ integer_to_list(Num))).

%% Clear variables in the environment.
-spec clear_vars(t()) -> t().
clear_vars(Env) ->
  Env#env{vars = []}.

%% Get a pattern variable by its name.
-spec lookup_pat_var(t(), aero_ast:ident()) -> aero_core:c_pat_var() | undefined.
lookup_pat_var(Env, {ident, _, IdentName}) ->
  case proplists:get_value(IdentName, Env#env.pat_vars) of
    undefined -> undefined;
    PatVar    -> PatVar
  end.

%% Create a fresh pattern variable from an identifier and save it to the env.
%%
%% lookup_pat_var/2 should be used to check for existing pattern variables
%% before registering. A regular variable is also saved to the environment for
%% expressions outside the pattern.
-spec register_pat_var(t(), aero_ast:ident()) -> {t(), aero_core:c_pat_var()}.
register_pat_var(Env, {ident, _, IdentName}) ->
  Num = incr_counter(Env),
  VarName = list_to_atom(atom_to_list(IdentName) ++ "_" ++ integer_to_list(Num)),
  Var = aero_core:c_var([], VarName),
  PatVar = aero_core:c_pat_var([], VarName),

  NewEnv = Env#env{
    vars = [{IdentName, Var} | Env#env.vars],
    pat_vars = [{IdentName, PatVar} | Env#env.pat_vars]
  },
  {NewEnv, PatVar}.

%% Create a wildcard pattern variable, not saved to the environment.
-spec wildcard_pat_var(t()) -> aero_core:c_pat_var().
wildcard_pat_var(Env) ->
  Num = incr_counter(Env),
  aero_core:c_pat_var([], list_to_atom("_" ++ integer_to_list(Num))).

%% Clear pattern variables in the environment.
-spec clear_pat_vars(t()) -> t().
clear_pat_vars(Env) ->
  Env#env{pat_vars = []}.

%% Get a type variable by its name.
-spec lookup_type_var(t(), aero_ast:type_param()) -> aero_core:c_type_var() | undefined.
lookup_type_var(Env, {type_param, _, ParamName}) ->
  case proplists:get_value(ParamName, Env#env.type_vars) of
    undefined -> undefined;
    TypeVar    -> TypeVar
  end.

%% Create a type variable from an identifier and save it to the env.
-spec register_type_var(t(), aero_ast:type_param()) -> {t(), aero_core:c_type_var()}.
register_type_var(Env, {type_param, _, ParamName}) ->
  TypeVarName = list_to_atom(atom_to_list(ParamName) ++ "_0"),
  TypeVar = aero_core:c_type_var([], TypeVarName),

  {Env#env{type_vars = [{ParamName, TypeVar} | Env#env.type_vars]}, TypeVar}.

%% Create an inferred type variable, not saved to the environment.
-spec inferred_type_var(t()) -> aero_core:c_type_var().
inferred_type_var(Env) ->
  Num = incr_counter(Env),
  aero_core:c_type_var([], list_to_atom("_" ++ integer_to_list(Num))).

%% Clear type variables in the environment.
-spec clear_type_vars(t()) -> t().
clear_type_vars(Env) ->
  Env#env{type_vars = []}.

%% Clear all variable types.
-spec clear_all_vars(t()) -> t().
clear_all_vars(Env) ->
  Env#env{vars = [], pat_vars = [], type_vars = []}.

%% Reset the counter.
-spec reset_counter(t()) -> t().
reset_counter(Env) ->
  Env#env{counter = counters:new(1, [])}.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

incr_counter(Env) ->
  counters:add(Env#env.counter, 1, 1),
  counters:get(Env#env.counter, 1).
