%%% Resolves paths through modules and visibilities.
%%%
%%% Takes care of finding the which function, constant, etc. it references by a
%%% path, and replaces the path with the full name if it's not local.

-module(aero_resolver).

-export([resolve/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec resolve([aero_expander:c_module()]) -> {ok, [aero_expander:c_module()]} | {error, term()}.
resolve(CoreModules) ->
  Index = index(CoreModules),

  try [resolve_module(Mod, Index) || Mod <- CoreModules] of
    Resolved -> {ok, Resolved}
  catch
    throw:{resolve_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

resolve_module({c_module, Meta, Path, Attrs, Defs}, Index) ->
  {c_module, Meta, Path, Attrs, [resolve_def(Def, set_path(Index, Path)) || Def <- Defs]}.

resolve_def({c_def_func, Meta, Path, Vis, Func}, Index) ->
  {c_def_func, Meta, Path, Vis, resolve_expr(Func, Index)};
resolve_def(Def, _Index) ->
  Def.

%% Resolving paths.
resolve_expr({c_path, Meta, _} = Path, Index) ->
  case proplists:get_bool(erl_path, Meta) of
    % Not resolving when calling into erlang functions.
    true ->
      Path;

    % Updating the path.
    false ->
      case lookup(Index, Path) of
        {NewPath, c_def_func} ->
          NewPath;

        % Replacing a constant reference with a call to it like a zero-arity
        % function. Constants aren't computed during compiling while bootstrapping.
        {NewPath, c_def_const} ->
          {c_call, [const_call | Meta], NewPath, []};

        none ->
          throw({resolve_error, {path_not_found, Meta, Path}})
      end
  end;

%% All other expressions.
resolve_expr({c_block, Meta, Exprs}, Index) ->
  {c_block, Meta, [resolve_expr(Expr, Index) || Expr <- Exprs]};

resolve_expr({c_tuple, Meta, Exprs}, Index) ->
  {c_tuple, Meta, [resolve_expr(Expr, Index) || Expr <- Exprs]};
resolve_expr({c_cons, Meta, Head, Tail}, Index) ->
  {c_cons, Meta, resolve_expr(Head, Index), resolve_expr(Tail, Index)};
resolve_expr({c_dict, Meta, Pairs}, Index) ->
  {c_dict, Meta,
           [{resolve_expr(Key, Index), resolve_expr(Value, Index)} || {Key, Value} <- Pairs]};

resolve_expr({c_func, Meta, Args, Result, Where, Body}, Index) ->
  {c_func, Meta, Args, Result, Where, resolve_expr(Body, Index)};
resolve_expr({c_call, Meta, Path, Args}, Index) ->
  {c_call, Meta, resolve_expr(Path, Index), [resolve_expr(Arg, Index) || Arg <- Args]};
resolve_expr({c_apply, Meta, Var, Args}, Index) ->
  {c_apply, Meta, Var, [resolve_expr(Arg, Index) || Arg <- Args]};

resolve_expr({c_let, Meta, Var, Type, Expr}, Index) ->
  {c_let, Meta, Var, Type, resolve_expr(Expr, Index)};
resolve_expr({c_letrec, Meta, Var, Type, Func}, Index) ->
  {c_letrec, Meta, Var, Type, resolve_expr(Func, Index)};

resolve_expr({c_match, Meta, Expr, Cases}, Index) ->
  {c_match, Meta,
            resolve_expr(Expr, Index),
            [{resolve_expr(Pat, Index), resolve_expr(Body, Index)} || {Pat, Body} <- Cases]};
resolve_expr({c_args, Meta, Args}, Index) ->
  {c_args, Meta, [resolve_expr(Arg, Index) || Arg <- Args]};

resolve_expr(Expr, _Index) ->
  Expr.

%% Stores information for lookups.
-record(index, {members, module}).

%% Produce an index of all module members for searching.
index(CoreModules) ->
  List = lists:flatmap(fun({c_module, _, ModPath, _, Defs}) ->
    lists:map(fun
      ({c_def_func, _, FuncPath, Vis, _}) ->
        {path_key([ModPath, FuncPath]), {Vis, c_def_func}};
      ({c_def_const, _, ConstPath, Vis, _, _}) ->
        {path_key([ModPath, ConstPath]), {Vis, c_def_const}}
    end, Defs)  
  end, CoreModules),

  #index{members = maps:from_list(List)}.

set_path(Index, Path) ->
  Index#index{module = Path}.

lookup(Index, Path) ->
  Key = path_key([Index#index.module, Path]),

  % Looking in the same path, any visibility is fine.
  case maps:get(Key, Index#index.members, none) of
    none         -> none;
    {_Vis, Type} -> {Path, Type}
  end.

%% Create a lookup key from the atoms inside a list of paths.
path_key(Paths) ->
  lists:flatmap(fun({c_path, _, Vars}) -> [Name || {c_var, _, Name} <- Vars] end, Paths).
