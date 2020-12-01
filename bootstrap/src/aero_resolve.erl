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
  aero_core:c_mod(Meta, Path, Attrs, [resolve_def(Def, set_path(Index, Path)) || Def <- Defs]).

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
      case lookup(Index, Path) of
        {NewPath, c_def_func} ->
          NewPath;

        % Replacing a constant reference with a call to it like a zero-arity
        % function. Constants aren't computed during compiling while bootstrapping.
        {NewPath, c_def_const} ->
          aero_core:c_call([const_call | Meta], NewPath, []);

        none ->
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
  List = lists:flatmap(fun({c_mod, _, ParentPath, _, Defs}) ->
    lists:map(fun
      ({c_def_func, _, FuncPath, Vis, _}) ->
        {path_key([ParentPath, FuncPath]), {Vis, c_def_func}};
      ({c_def_const, _, ConstPath, Vis, _, _}) ->
        {path_key([ParentPath, ConstPath]), {Vis, c_def_const}};
      ({c_def_mod, _, ModPath, Vis}) ->
        {path_key([ParentPath, ModPath]), {Vis, c_def_mod}}
    end, Defs)  
  end, Modules),

  #index{members = maps:from_list(List)}.

set_path(Index, Path) ->
  Index#index{module = Path}.

%% Create a lookup key from the atoms inside a list of paths.
path_key(Paths) ->
  lists:flatmap(fun({c_path, _, Vars}) -> [Name || {c_var, _, Name} <- Vars] end, Paths).

%% Look up a name.
lookup(Index, {c_path, _, Vars} = Path) when length(Vars) =:= 1 ->
  Key = path_key([Index#index.module, Path]),

  % Looking in the same path, any visibility is fine.
  case maps:get(Key, Index#index.members) of
    {_Vis, c_def_func}  -> {Path, c_def_func};
    {_Vis, c_def_const} -> {Path, c_def_const};
    _                   -> none
  end;
lookup(Index, Path) ->
  % Not in the same directory, so we need to check if module definitions are
  % public along the way.
  lookup(Index, path_key([Index#index.module]), path_key([Path])).

lookup(Index, Mod, [Name]) ->
  Key = Mod ++ [Name],

  case maps:get(Key, Index#index.members) of
    {c_vis_pub, c_def_func}  -> {global_path(Key), c_def_func};
    {c_vis_pub, c_def_const} -> {global_path(Key), c_def_const};
    _                        -> none
  end;
lookup(Index, Base, [Mod | Name]) ->
  Key = Base ++ [Mod],

  case maps:get(Key, Index#index.members) of
    {c_vis_pub, c_def_mod}  -> lookup(Index, Key, Name);
    _                       -> none
  end.

%% Get a global path from a lookup key.
global_path(Key) ->
  aero_core:c_path([], [aero_session:pkg() | [aero_core:c_var([], Name) || Name <- Key]]).
 