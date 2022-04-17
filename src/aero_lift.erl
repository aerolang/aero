%%% Lifts non-literals out of expressions and gives them temporary variables.
%%%
%%% This pass makes expressions only use literals and variables with various
%%% Aero Core nodes, removing nested expressions for later passes in the
%%% compiler.

-module(aero_lift).

-export([lift/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Lifts up nested expressions into temporary variables.
-spec lift(aero_core:c_pkg()) -> {ok, aero_core:c_pkg()}.
lift({c_pkg, Meta, Name, Modules}) ->
  LiftedModules =
    lists:map(fun({c_mod, ModMeta, Path, Attrs, Defs}) ->
      aero_core:c_mod(ModMeta, Path, Attrs, lists:map(fun lift_def/1, Defs))
    end, Modules),

  {ok, aero_core:c_pkg(Meta, Name, LiftedModules)}.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------
 
lift_def({c_def_func, Meta, Path, Vis, Func}) ->
  aero_core:c_def_func(Meta, Path, Vis, lift_expr(Func, def_env(Meta)));
lift_def({c_def_const, Meta, Path, Vis, Type, Expr}) ->
  aero_core:c_def_const(Meta, Path, Vis, Type, lift_expr(Expr, def_env(Meta)));
lift_def({c_def_mod, _, _, _} = Def) ->
  Def.

lift_expr({c_block, Meta, Exprs}, Env) ->
  aero_core:c_block(Meta, [lift_expr(Expr, Env) || Expr <- Exprs]);
lift_expr({c_tuple, Meta, Exprs}, Env) ->
  {Lets, NewExprs} =
    lists:foldl(fun(Expr, {AccLets, AccExprs}) ->
      LiftedExpr = lift_expr(Expr, Env),
      case is_simple(LiftedExpr) of
        true ->
          {AccLets, [LiftedExpr | AccExprs]};
        false ->
          Var = aero_env:tmp_var(Env),
          Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedExpr),

          {[Let | AccLets], [Var | AccExprs]}
      end
    end, {[], []}, Exprs),
  case Lets of
    [] ->
      aero_core:c_tuple(Meta, lists:reverse(NewExprs));
    _  ->
      aero_core:c_block(Meta,
        lists:reverse([aero_core:c_tuple(Meta, lists:reverse(NewExprs)) | Lets])
      )
  end;
lift_expr({c_cons, Meta, Head, Tail}, Env) ->
  LiftedHead = lift_expr(Head, Env),
  LiftedTail = lift_expr(Tail, Env),
  case {is_simple(LiftedHead), is_simple(LiftedTail)} of
    % Both simple.
    {true, true} ->
      aero_core:c_cons(Meta, LiftedHead, LiftedTail);

    % Only head is simple.
    {true, false} ->
      Var = aero_env:tmp_var(Env),
      Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedTail),

      aero_core:c_block(Meta, [Let, aero_core:c_cons(Meta, LiftedHead, Var)]);

    % Only tail is simple.
    {false, true} ->
      Var = aero_env:tmp_var(Env),
      Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedHead),

      aero_core:c_block(Meta, [Let, aero_core:c_cons(Meta, Var, LiftedTail)]);

    % Neither are simple.
    {false, false} ->
      HeadVar = aero_env:tmp_var(Env),
      TailVar = aero_env:tmp_var(Env),
      HeadLet = aero_core:c_let([], HeadVar, aero_env:inferred_type_var(Env), LiftedHead),
      TailLet = aero_core:c_let([], TailVar, aero_env:inferred_type_var(Env), LiftedTail),

      aero_core:c_block(Meta, [HeadLet, TailLet, aero_core:c_cons(Meta, HeadVar, TailVar)])
  end;
lift_expr({c_dict, Meta, Pairs}, Env) ->
  {Lets, NewPairs} =
    lists:foldl(fun({Key, Value}, {AccLets, AccPairs}) ->
      LiftedKey = lift_expr(Key, Env),
      LiftedValue = lift_expr(Value, Env),
      case {is_simple(LiftedKey), is_simple(LiftedValue)} of
        % Both simple.
        {true, true} ->
          {AccLets, [{LiftedKey, LiftedValue} | AccPairs]};

        % Only key is simple.
        {true, false} ->
          Var = aero_env:tmp_var(Env),
          Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedValue),

          {[Let | AccLets], [{LiftedKey, Var} | AccPairs]};

        % Only value is simple.
        {false, true} ->
          Var = aero_env:tmp_var(Env),
          Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedKey),

          {[Let | AccLets], [{Var, LiftedValue} | AccPairs]};

        % Neither are simple.
        {false, false} ->
          KeyVar = aero_env:tmp_var(Env),
          ValueVar = aero_env:tmp_var(Env),
          KeyLet = aero_core:c_let([], KeyVar, aero_env:inferred_type_var(Env), LiftedKey),
          ValueLet = aero_core:c_let([], ValueVar, aero_env:inferred_type_var(Env), LiftedValue),

          {[KeyLet, ValueLet | AccLets], [{KeyVar, ValueVar} | AccPairs]}
      end
    end, {[], []}, Pairs),
  case Lets of
    [] ->
      aero_core:c_dict(Meta, lists:reverse(NewPairs));
    _  ->
      aero_core:c_block(Meta,
        lists:reverse([aero_core:c_dict(Meta, lists:reverse(NewPairs)) | Lets])
      )
  end;
lift_expr({c_func, Meta, Args, Result, Where, Body}, Env) ->
  aero_core:c_func(Meta, Args, Result, Where, lift_expr(Body, Env));
lift_expr({Call, Meta, Path, Args}, Env) when Call =:= c_call; Call =:= c_apply ->
  {Lets, NewArgs} =
    lists:foldl(fun(Arg, {AccLets, AccArgs}) ->
      LiftedArg = lift_expr(Arg, Env),
      case is_simple(LiftedArg) of
        true ->
          {AccLets, [LiftedArg | AccArgs]};
        false ->
          Var = aero_env:tmp_var(Env),
          Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedArg),

          {[Let | AccLets], [Var | AccArgs]}
      end
    end, {[], []}, Args),
  NewCall =
    case Call of
      c_call  -> aero_core:c_call(Meta, Path, lists:reverse(NewArgs));
      c_apply -> aero_core:c_apply(Meta, Path, lists:reverse(NewArgs))
    end,
  case Lets of
    [] -> NewCall;
    _  -> aero_core:c_block(Meta, lists:reverse([NewCall | Lets]))
  end;
lift_expr({c_let, Meta, Var, Type, Expr}, Env) ->
  aero_core:c_let(Meta, Var, Type, lift_expr(Expr, Env));
lift_expr({c_letrec, Meta, Var, Type, Func}, Env) ->
  aero_core:c_letrec(Meta, Var, Type, lift_expr(Func, Env));
lift_expr({c_match, Meta, Expr, Cases}, Env) ->
  LiftedExpr = lift_expr(Expr, Env),
  LiftedCases = lists:map(fun({Pat, Body}) -> {Pat, lift_expr(Body, Env)} end, Cases),
  case is_simple(LiftedExpr) of
    true ->
      aero_core:c_match(Meta, LiftedExpr, LiftedCases);
    false ->
      Var = aero_env:tmp_var(Env),
      Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedExpr),

      aero_core:c_block(Meta, [Let, aero_core:c_match(Meta, Var, LiftedCases)])
  end;
lift_expr(Expr, _Env) ->
  Expr.

%% Expressions without any nested expressions to evaluate inside.
is_simple({c_tuple, _, Exprs}) ->
  lists:all(fun is_literal/1, Exprs);
is_simple({c_cons, _, Head, Tail}) ->
  is_simple(Head) andalso is_literal(Tail);
is_simple({c_dict, _, Pairs}) ->
  lists:all(fun({K, V}) -> is_literal(K) andalso is_literal(V) end, Pairs);
is_simple({c_args, _, Exprs}) ->
  lists:all(fun is_simple/1, Exprs);
is_simple(Expr) ->
  is_literal(Expr) orelse is_var(Expr).

is_literal({c_bool, _, _})  -> true;
is_literal({c_int, _, _})   -> true;
is_literal({c_float, _, _}) -> true;
is_literal({c_sym, _, _})   -> true;
is_literal({c_str, _, _})   -> true;
is_literal({c_nil, _})      -> true;
is_literal({c_void, _})     -> true;
is_literal(_)               -> false.

is_var({c_var, _, _})  -> true;
is_var({c_path, _, _}) -> true;
is_var(_)              -> false.

def_env(Meta) ->
  proplists:get_value(env, Meta).
