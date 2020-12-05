%%% Resolves paths through modules and visibilities.
%%%
%%% Takes care of finding the which function, constant, etc. it references by a
%%% path, and replaces the path with the full name if it's not local.

-module(aero_resolve).

-export([resolve/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec resolve(aero_core:c_pkg()) -> {ok, aero_core:c_pkg()} | {error, term()}.
resolve(Package) ->
  Index = index(Package),

  try [resolve_module(Mod, Index) || Mod <- element(4, Package)] of
    Resolved -> {ok, setelement(4, Package, Resolved)}
  catch
    throw:{resolve_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

resolve_module({c_mod, Meta, Path, Attrs, Defs}, Index) ->
  aero_core:c_mod(Meta, Path, Attrs, [resolve_def(Def, set_module(Index, Path)) || Def <- Defs]).

resolve_def({c_def_func, Meta, Path, Vis, Func}, Index) ->
  aero_core:c_def_func(Meta, Path, Vis, resolve_expr(Func, Index));
resolve_def({c_def_const, Meta, Path, Vis, Type, Expr}, Index) ->
  aero_core:c_def_const(Meta, Path, Vis, Type, resolve_expr(Expr, Index));
resolve_def({c_def_mod, _, _, _} = Def, _Index) ->
  Def.

%% Resolving paths.
resolve_expr({c_path, Meta, _} = Path, Index) ->
  case proplists:get_bool(erl_path, Meta) of
    % Not resolving when calling into erlang functions.
    true ->
      Path;

    % Updating the path.
    false ->
      case search_module(Index, Path) of
        {NewPath, c_def_func} ->
          NewPath;

        % Replacing a constant reference with a call to it like a zero-arity
        % function. Constants aren't computed during compiling while bootstrapping.
        {NewPath, c_def_const} ->
          aero_core:c_call([const_call | Meta], NewPath, []);

        undefined ->
          throw({resolve_error, {path_not_found, Meta, Path}})
      end
  end;

%% All other expressions.
resolve_expr({c_block, Meta, Exprs}, Index) ->
  aero_core:c_block(Meta, [resolve_expr(Expr, Index) || Expr <- Exprs]);

resolve_expr({c_tuple, Meta, Exprs}, Index) ->
  aero_core:c_tuple(Meta, [resolve_expr(Expr, Index) || Expr <- Exprs]);
resolve_expr({c_cons, Meta, Head, Tail}, Index) ->
  aero_core:c_cons(Meta, resolve_expr(Head, Index), resolve_expr(Tail, Index));
resolve_expr({c_dict, Meta, Pairs}, Index) ->
  aero_core:c_dict(Meta,
    [{resolve_expr(Key, Index), resolve_expr(Value, Index)} || {Key, Value} <- Pairs]
  );

resolve_expr({c_func, Meta, Args, Result, Where, Body}, Index) ->
  aero_core:c_func(Meta, Args, Result, Where, resolve_expr(Body, Index));
resolve_expr({c_call, Meta, Path, Args}, Index) ->
  aero_core:c_call(Meta, resolve_expr(Path, Index), [resolve_expr(Arg, Index) || Arg <- Args]);
resolve_expr({c_apply, Meta, Var, Args}, Index) ->
  aero_core:c_apply(Meta, Var, [resolve_expr(Arg, Index) || Arg <- Args]);

resolve_expr({c_let, Meta, Var, Type, Expr}, Index) ->
  aero_core:c_let(Meta, Var, Type, resolve_expr(Expr, Index));
resolve_expr({c_letrec, Meta, Var, Type, Func}, Index) ->
  aero_core:c_letrec(Meta, Var, Type, resolve_expr(Func, Index));

resolve_expr({c_match, Meta, Expr, Cases}, Index) ->
  aero_core:c_match(Meta,
    resolve_expr(Expr, Index),
    [{resolve_expr(Pat, Index), resolve_expr(Body, Index)} || {Pat, Body} <- Cases]
  );
resolve_expr({c_args, Meta, Args}, Index) ->
  aero_core:c_args(Meta, [resolve_expr(Arg, Index) || Arg <- Args]);

resolve_expr(Expr, _Index) ->
  Expr.

%% Stores information for lookups.
-record(index, {members, module}).

%% Produce an index of all module members for searching.
index({c_pkg, _, _, Modules}) ->
  List = lists:flatmap(fun({c_mod, _, Mod, _, Defs}) ->
    lists:map(fun
      ({c_def_func, _, Path, Vis, _}) ->
        {path_key(Mod, Path), {Vis, c_def_func}};
      ({c_def_const, _, Path, Vis, _, _}) ->
        {path_key(Mod, Path), {Vis, c_def_const}};
      ({c_def_mod, _, Path, Vis}) ->
        {path_key(Mod, Path), {Vis, c_def_mod}}
    end, Defs)  
  end, Modules),

  #index{members = maps:from_list(List)}.

%% Set the current module.
set_module(Index, ModPath) ->
  Index#index{module = ModPath}.

%% Find a path in the index.
search_module(Index, Path) ->
  Key = path_key(Path),
  search_module(Index#index.members, path_key(Index#index.module), [], [hd(Key)], tl(Key)).

search_module(Members, Mod, Base, Key, []) ->
  case maps:get(Mod ++ Base ++ Key, Members, none) of
    {Vis, Def} when Def =:= c_def_func orelse Def =:= c_def_const,
                    Vis =:= c_vis_pub orelse Base =:= [] ->
      case Base of
        [] -> {local_path(Key), Def};
        _  -> {global_path(Mod ++ Base ++ Key), Def}
      end;
    _ ->
      undefined
  end;
search_module(Members, Mod, Base, Key, Rem) ->
  case maps:get(Mod ++ Base ++ Key, Members, none) of
    {Vis, c_def_mod} when Vis =:= c_vis_pub orelse Base =:= [] ->
      search_module(Members, Mod, Base ++ Key, [hd(Rem)], tl(Rem));
    _ ->
      search_module(Members, Mod, Base, Key ++ [hd(Rem)], tl(Rem))
  end.

%% Create a lookup key from a path.
path_key({c_path, _, Vars}) ->
  [Name || {c_var, _, Name} <- Vars].

%% Create a lookup key using a module and path.
path_key(Mod, Path) ->
  path_key(Mod) ++ path_key(Path).

%% Get a global path from a lookup key.
global_path(Key) ->
  aero_core:c_path([], [aero_session:pkg() | [aero_core:c_var([], Name) || Name <- Key]]).

%% Get a local path from a lookup key.
local_path(Key) ->
  aero_core:c_path([], [aero_core:c_var([], Name) || Name <- Key]).
