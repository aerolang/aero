%%% Handles expansion for expressions.

-module(aero_expand_expr).

-export([expand_expr/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Expand an expression.
-spec expand_expr(aero_ast:ast(), aero_env:env()) -> aero_core:c_expr().

%% Blocks.
expand_expr({block, _, []}, _Env) ->
  aero_core:c_unit([]);
expand_expr({block, _, [{expand, _, {op, _, Arrow}, _} | _] = BlockExprs}, Env)
    when Arrow =:= '_->_'; Arrow =:= '_->>_' ->
  % When the first entry in the block is a function, this whole thing is treated
  % as an anonymous function with like a `match`. If the there's only one case
  % and it's only using variables or wildcards we'll treat it as a normal
  % function still. All the arrows have to match too.
  NormalFunc =
    case BlockExprs of
      [{expand, _, {op, _, Arrow}, [{args, _, Args}, _]}] ->
        lists:all(fun
          ({ident, _, _}) -> true;
          ({blank, _})    -> true;
          (_)             -> false
        end, Args);
      _ ->
        false
    end,
  case NormalFunc of
    true ->
      expand_expr(hd(BlockExprs), Env);
    false ->
      FuncCases =
        lists:map(fun
          ({expand, _, {op, _, A}, [Pat, Expr]}) when A =:= Arrow ->
            {CorePat, CoreEnv} = aero_expand_pat:expand_pat(Pat, Env),
            CoreExpr = expand_expr(Expr, CoreEnv),

            {CorePat, CoreExpr};
          (Arg) ->
            throw({expand_error, {block_func_case_invalid, aero_ast:meta(Arg)}})
        end, BlockExprs),
      
      % Get the arity from the first case.
      {{c_pat_args, _, FirstPatArgs}, _} = hd(FuncCases),
      Arity = length(FirstPatArgs),

      % Assert the arity is the same for each case.
      lists:foreach(fun
        ({{c_pat_args, _, CasePatArgs}, _}) when length(CasePatArgs) =:= Arity ->
          ok;
        ({{c_pat_args, Meta, _}, _}) ->
          throw({expand_error, {block_func_arity_mismatch, Meta}})
      end, tl(FuncCases)),

      % Create a function which just has a match inside.
      Vars = [aero_env:tmp_var(Env) || _ <- lists:seq(1, Arity)],
      FuncVars = [{Var, aero_env:inferred_type_var(Env)} || Var <- Vars],
      Match = aero_core:c_match([], aero_core:c_args([], Vars), FuncCases),

      aero_core:c_func([], FuncVars, aero_env:inferred_type_var(Env), [], Match)
  end;
expand_expr({block, _, BlockExprs}, Env) ->
  {Exprs, _} =
    lists:foldl(fun(BlockExpr, {ExprAcc, EnvAcc}) ->
      case BlockExpr of
        {expand, _, {op, _, '_=_'}, [Ident, RightExpr]} ->
          {NewEnv, Var} = aero_env:register_var(EnvAcc, Ident),
          RightCore = expand_expr(RightExpr, Env),
          LetExpr = aero_core:c_let([], Var, aero_env:inferred_type_var(EnvAcc), RightCore),

          {[LetExpr | ExprAcc], NewEnv};
        _ ->
          {[expand_expr(BlockExpr, EnvAcc) | ExprAcc], EnvAcc}
      end
    end, {[], Env}, BlockExprs),
  case Exprs of
    [Expr] ->
      Expr;
    _ ->
      % Unused expressions in a block are given variables (the last expression
      % isn't modified, however).
      aero_core:c_block([], lists:reverse(lists:map(fun(Expr) ->
        case Expr of
          {c_let, _, _, _, _} ->
            Expr;
          _ ->
            aero_core:c_let([], aero_env:tmp_var(Env), aero_env:inferred_type_var(Env), Expr)
        end
      end, tl(Exprs))) ++ [hd(Exprs)])
  end;

%% Literals.
expand_expr({ident, _, Bool}, _Env) when Bool =:= true; Bool =:= false ->
  aero_core:c_bool([], Bool);
expand_expr({int_lit, _, Integer}, _Env) ->
  aero_core:c_int([], Integer);
expand_expr({float_lit, _, Float}, _Env) ->
  aero_core:c_float([], Float);
expand_expr({atom_lit, _, Atom}, _Env) ->
  aero_core:c_atom([], Atom);
expand_expr({str_lit, _, String}, _Env) ->
  aero_core:c_str([], String);

%% Unit value.
expand_expr({expand, _, {op, _, '(_)'}, [{args, _, []}]}, _Env) ->
  aero_core:c_unit([]);

%% Tuples.
expand_expr({expand, _, {op, _, '(_)'}, [{args, _, Args}]}, Env) when length(Args) > 1 ->
  aero_core:c_tuple([], [expand_expr(Arg, Env) || Arg <- Args]);

%% Cons and nil.
expand_expr({expand, _, {op, _, '_::_'}, [Head, Tail]}, Env) ->
  aero_core:c_cons([], expand_expr(Head, Env), expand_expr(Tail, Env));
expand_expr({ident, _, nil}, _Env) ->
  aero_core:c_nil([]);

%% Dictionaries.
expand_expr({expand, _, {op, _, '#{_}'}, [{args, _, Args}]}, Env) ->
  Pairs =
    lists:map(fun(Arg) ->
      case Arg of
        {expand, _, {op, _, '_=>_'}, [Key, Value]} ->
          {expand_expr(Key, Env), expand_expr(Value, Env)};

        % We can have a tag in a dictionary for #{ atom: expr } syntax.
        % Needing to corece the left side into an atom.
        {tag, _, {ident, _, Key}, Value} ->
          {aero_core:c_atom([], Key), expand_expr(Value, Env)}
      end
    end, Args),
  aero_core:c_dict([], Pairs);

%% Anonymous functions.
expand_expr({expand, _, {op, _, Arrow}, [{args, _, Args}, Body]}, Env) when Arrow =:= '_->_';
                                                                            Arrow =:= '_->>_' ->
  % If the function is in the form `name(args) -> body`, then extract the name
  % for a recusive anonymous function.
  {FuncArgs, {HeadEnv, Var}} =
    case Args of
      [{expand, _, {op, _, '_(_)'}, [FuncIdent, {args, _, InnerArgs}]}] ->
        {InnerArgs, aero_env:register_var(Env, FuncIdent)};
      _ ->
        {Args, {Env, none}}
    end,
  % Not allowing any types to be used in anonymous functions.
  {CoreArgs, BodyEnv} =
    lists:foldl(fun(Arg, {ArgAcc, EnvAcc}) ->
      case Arg of
        {ident, _, _} = Ident ->
          {NewEnv, ArgVar} = aero_env:register_var(EnvAcc, Ident),
          ArgType = aero_env:inferred_type_var(EnvAcc),

          {[{ArgVar, ArgType} | ArgAcc], NewEnv};
        _ ->
          throw({expand_error, {func_arg_invalid, aero_ast:meta(Arg)}})
      end
    end, {[], HeadEnv}, FuncArgs),
  CoreBody = expand_expr(Body, BodyEnv),
  Func = aero_core:c_func(
    [], lists:reverse(CoreArgs), aero_env:inferred_type_var(Env), [], CoreBody),

  % If recursive then wrap it into a block with a letrec.
  case Var of
    none ->
      Func;
    _ ->
      aero_core:c_block(
        [], [aero_core:c_letrec([], Var, aero_env:inferred_type_var(Env), Func), Var])
  end;

%% Function calls.
expand_expr({expand, Meta, {op, _, '_(_)'}, [Callee, {args, _, Args}]}, Env) ->
  case Callee of
    % Constructors.
    {expand, _, {op, _, '#_'}, [Path]} ->
      case Path of
        {ident, _, ref} when length(Args) =:= 0 ->
          erl_call(erlang, make_ref, []);
        {ident, _, list} ->
          lists:foldr(fun(Arg, Acc) ->
            aero_core:c_cons([], expand_expr(Arg, Env), Acc)
          end, aero_core:c_nil([]), Args);
        {ident, _, mbox} when length(Args) =:= 0 ->
          erl_call(erlang, make_ref, []);
        _ ->
          throw({expand_error, {constructor_invalid, Meta}})
      end;

    % Normal function calls.
    _ ->
      CoreCallee = expand_expr(Callee, Env),
      CoreArgs = [expand_expr(Arg, Env) || Arg <- Args],

      case CoreCallee of
        {c_var, _, _} -> aero_core:c_apply([], CoreCallee, CoreArgs);
        _             -> aero_core:c_call([], CoreCallee, CoreArgs)
      end
  end;

%% Variables.
expand_expr({ident, _, Name} = Ident, Env) ->
  case aero_env:lookup_var(Env, Ident) of
    undefined -> aero_core:c_path([], [aero_core:c_var([], Name)]);
    Var       -> Var
  end;

%% Arithmetic operators.
expand_expr({expand, _, {op, _, '+_'}, [Value]}, Env) ->
  erl_call(erlang, '+', [expand_expr(Value, Env)]);
expand_expr({expand, _, {op, _, '-_'}, [Value]}, Env) ->
  erl_call(erlang, '-', [expand_expr(Value, Env)]);
expand_expr({expand, _, {op, _, '_+_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '+', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_-_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '-', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_*_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '*', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_/_'}, [Left, Right]}, Env) ->
  % Choosing between integer and float division.
  LeftVar = aero_env:tmp_var(Env),
  RightVar = aero_env:tmp_var(Env),

  LeftExpr = expand_expr(Left, Env),
  RightExpr = expand_expr(Right, Env),

  IsLeftInt = erl_call(erlang, is_integer, [LeftVar]),
  IsRightInt = erl_call(erlang, is_integer, [RightVar]),

  IntDiv = erl_call(erlang, 'div', [LeftVar, RightVar]),
  FloatDiv = erl_call(erlang, '/', [LeftVar, RightVar]),

  aero_core:c_block([], [
    aero_core:c_let([], LeftVar, aero_env:inferred_type_var(Env), LeftExpr),
    aero_core:c_let([], RightVar, aero_env:inferred_type_var(Env), RightExpr),
    aero_core:c_match([], IsLeftInt, [
      {aero_core:c_pat_bool([], true), aero_core:c_match([], IsRightInt, [
        {aero_core:c_pat_bool([], true), IntDiv},
        {aero_env:wildcard_pat_var(Env), FloatDiv}
      ])},
      {aero_env:wildcard_pat_var(Env), FloatDiv}
    ])
  ]);
expand_expr({expand, _, {op, _, '_%_'}, [Left, Right]}, Env) ->
  erl_call(erlang, 'rem', [expand_expr(Left, Env), expand_expr(Right, Env)]);

%% Bitwise operators.
expand_expr({expand, _, {op, _, '~~~_'}, [Value]}, Env) ->
  erl_call(erlang, 'bnot', [expand_expr(Value, Env)]);
expand_expr({expand, _, {op, _, '_&&&_'}, [Left, Right]}, Env) ->
  erl_call(erlang, 'band', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_|||_'}, [Left, Right]}, Env) ->
  erl_call(erlang, 'bor', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_^^^_'}, [Left, Right]}, Env) ->
  erl_call(erlang, 'bxor', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_<<<_'}, [Left, Right]}, Env) ->
  erl_call(erlang, 'bsl', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_>>>_'}, [Left, Right]}, Env) ->
  erl_call(erlang, 'bsr', [expand_expr(Left, Env), expand_expr(Right, Env)]);

%% Concatenation.
expand_expr({expand, _, {op, _, '_++_'}, [Left, Right]}, Env) ->
  % TODO: Choose between list and binary concatenation when we have binary
  %       constructors we can use for concatenation.
  erl_call(erlang, '++', [expand_expr(Left, Env), expand_expr(Right, Env)]);

%% Comparison operators.
expand_expr({expand, _, {op, _, '_==_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '=:=', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_<>_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '=/=', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_<_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '<', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_>_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '>', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_<=_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '=<', [expand_expr(Left, Env), expand_expr(Right, Env)]);
expand_expr({expand, _, {op, _, '_>=_'}, [Left, Right]}, Env) ->
  erl_call(erlang, '>=', [expand_expr(Left, Env), expand_expr(Right, Env)]);

%% Logical operators.
expand_expr({expand, _, {op, _, 'not_'}, [Value]}, Env) ->
  erl_call(erlang, 'not', [expand_expr(Value, Env)]);
expand_expr({expand, _, {op, _, '_and_'}, [Left, Right]}, Env) ->
  % Manually short-circuit with `match`.
  Cases = [
    {aero_core:c_pat_bool([], true), expand_expr(Right, Env)},
    {aero_env:wildcard_pat_var(Env), aero_core:c_bool([], false)}
  ],
  aero_core:c_match([], expand_expr(Left, Env), Cases);

expand_expr({expand, _, {op, _, '_or_'}, [Left, Right]}, Env) ->
  % Manually short-circuit with `match`.
  Cases = [
    {aero_core:c_pat_bool([], true), aero_core:c_bool([], true)},
    {aero_env:wildcard_pat_var(Env), expand_expr(Right, Env)}
  ],
  aero_core:c_match([], expand_expr(Left, Env), Cases);

%% `match` expression.
expand_expr({expand, Meta, {ident, _, 'match'}, Args}, _Env) when length(Args) < 2 ->
  throw({expand_error, {match_invalid, Meta}});
expand_expr({expand, Meta, {ident, _, 'match'}, Args}, Env) ->
  case lists:last(Args) of
    {block, _, BlockArgs} ->
      % Be permissive with multiple arguments before a block: they are
      % converted into a tuple (even though they really shouldn't be) for
      % ergonomic reasons.
      CoreExpr =
        case lists:droplast(Args) of
          [Expr] -> expand_expr(Expr, Env);
          Exprs  -> aero_core:c_tuple([], [expand_expr(Expr, Env) || Expr <- Exprs])
        end,
      Cases =
        lists:map(fun
          ({expand, _, {op, _, '_=>_'}, [Pat, Body]}) ->
            {CorePat, PatEnv} = aero_expand_pat:expand_pat(Pat, Env),
            CoreBody = expand_expr(Body, PatEnv),

            {CorePat, CoreBody};
          (CaseExpr) ->
            throw({expand_error, {match_case_invalid, aero_ast:meta(CaseExpr)}})
        end, BlockArgs),

      aero_core:c_match([], CoreExpr, Cases);
    _ ->
      throw({expand_error, {match_invalid, Meta}})
  end;

%% `when` expression.
expand_expr({expand, Meta, {ident, _, 'when'}, [{block, _, BlockArgs}]}, Env) ->
  Clauses =
    lists:map(fun
      ({expand, _, {op, _, '_=>_'}, [Cond, Body]}) ->
        {expand_expr(Cond, Env), expand_expr(Body, Env)};
      (Expr) ->
        throw({expand_error, {when_clause_invalid, aero_ast:meta(Expr)}})
    end, BlockArgs),

  case is_when_exhaustive(Clauses) of
    true ->
      RevClauses = lists:reverse(Clauses),
      {_, LastExpr} = hd(RevClauses),

      % Checking if the condition is `true`, otherwise using a wildcard pattern to
      % continue with a nested `match`.
      lists:foldl(fun({Cond, Expr}, Inner) ->
        aero_core:c_match([], Cond, [
          {aero_core:c_pat_bool([], true), Expr},
          {aero_env:wildcard_pat_var(Env), Inner}
        ])
      end, LastExpr, tl(RevClauses));

    false ->
      throw({expand_error, {when_inexhaustive, Meta}})
  end;
expand_expr({expand, Meta, {ident, _, 'when'}, _}, _Env) ->
  throw({expand_error, {when_invalid, Meta}});

%% `if` expression.
expand_expr({expand, Meta, {ident, _, 'if'}, [Cond, Next]}, Env) ->
  case Next of
    % With else block.
    {expand, _, {op, _, '_else_'}, [{block, _, _} = Then, {block, _, _} = Else]} ->
      Cases = [
        {aero_core:c_pat_bool([], true), expand_expr(Then, Env)},
        {aero_env:wildcard_pat_var(Env), expand_expr(Else, Env)}
      ],
      aero_core:c_match([], expand_expr(Cond, Env), Cases);

    % No else block, so wrapping with optional.
    {block, _, _} = Then ->
      Cases = [
        {aero_core:c_pat_bool([], true), aero_core:c_tuple([], [
          aero_core:c_atom([], some),
          expand_expr(Then, Env)
        ])},
        {aero_env:wildcard_pat_var(Env), aero_core:c_atom([], none)}
      ],
      aero_core:c_match([], expand_expr(Cond, Env), Cases);

    _ ->
      throw({expand_error, {if_invalid, Meta}})
  end;

expand_expr({expand, Meta, {ident, _, 'if'}, _}, _Env) ->
  throw({expand_error, {if_invalid, Meta}});

%% Logs.
expand_expr({expand, _, {ident, _, log}, [Message]}, Env) ->
  Args = [
    aero_core:c_atom([], standard_io),
    aero_core:c_cons([],
      expand_expr(Message, Env),
      aero_core:c_cons([],
        aero_core:c_int([], $\n),
        aero_core:c_nil([])
      )
    )
  ],
  erl_call(io, put_chars, Args);

%% Anything else...
expand_expr(Expr, _Env) ->
  throw({expand_error, {expr_invalid, aero_ast:meta(Expr)}}).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

erl_call(Mod, Func, Args) ->
  Callee = aero_core:c_path([erl_path], [aero_core:c_var([], Mod), aero_core:c_var([], Func)]),
  aero_core:c_call([], Callee, Args).

is_when_exhaustive([]) ->
  false;
is_when_exhaustive(Clauses) ->
  case lists:last(Clauses) of
    {{c_bool, _, true}, _} -> true;
    _                      -> false
  end.
