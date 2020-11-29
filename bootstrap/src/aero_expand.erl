%%% Converts the Aero AST down to Core Aero.
%%%
%%% This format is made to resemble Core Erlang with variables renamed and
%%% macros expanded giving a simple core language.
%%%
%%% For now we don't really have macros, so they're just manually expanded here.

-module(aero_expand).

-export([expand/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec expand(aero_ast:ast(), aero_env:env()) -> {ok, aero_core:c_pkg()} | {error, term()}.
expand(Source, Env) ->
  try expand_source(Source, Env) of
    Package -> {ok, Package}
  catch
    throw:{expand_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Definition Expanding
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
      {Def, NewEnv} = expand_def(SourceArg, DefEnv),
      {[Def | Defs], NewEnv, InnerModules}
    end, {[], Env, []}, SourceArgs),

  ModulePath = aero_core:c_path([], [aero_core:c_var([], ModName)]),
  Module = aero_core:c_mod([], ModulePath, [], lists:reverse(Defs)),

  aero_core:c_pkg([], PkgName, [Module | lists:reverse(InnerModules)]);
expand_source(_, _) ->
  throw({expand_error, no_source}).

%% Private definitions.
expand_def({expand, Meta, {ident, _, func}, Args}, Env) ->
  expand_func_def(Args, Meta, c_vis_priv, Env);
expand_def({expand, Meta, {ident, _, const}, Args}, Env) ->
  expand_const_def(Args, Meta, c_vis_priv, Env);

%% Public definitions.
expand_def({expand, Meta, {ident, _, pub}, PubArgs}, Env) ->
  case PubArgs of
    [{expand, _, {ident, _, func}, Args}] ->
      expand_func_def(Args, Meta, c_vis_pub, Env);
    [{expand, _, {ident, _, const}, Args}] ->
      expand_const_def(Args, Meta, c_vis_pub, Env);
    _ ->
      throw({expand_error, {pub_invalid, aero_ast:meta(Meta)}})
  end;

%% Anything else...
expand_def(Def, _Env) ->
  throw({expand_error, {def_invalid, aero_ast:meta(Def)}}).

expand_func_def([{expand, _, {op, _, '_=_'}, [FuncHead, FuncBody]}], FuncMeta, Vis, Env) ->
  % Function definition variant with assignment to an anonymous function on the right.
  Where = [],
  case FuncHead of
    {tag, _, {ident, _, _} = Ident, {expand, _, {op, _, Arrow}, [{args, _, Args}, Result]}}
        when Arrow =:= '_->_'; Arrow =:= '_->>_' ->
      % For function head.
      check_existing_def(Env, Ident),
      {DefEnv, Path} = aero_env:register_def(Env, Ident),

      {ArgTypes, ResultEnv} = aero_expand_type:expand_types(Args, aero_env:reset_counter(Env)),
      {ResultType, BodyEnv} = aero_expand_type:expand_type(Result, ResultEnv),

      % Expanding body and ensuring it gives a function.
      case expand_expr(FuncBody, BodyEnv) of
        {c_func, _, ExprArgs, _, _, ExprBody} when length(ExprArgs) =:= length(ArgTypes) ->
          ExprVars = [element(1, Arg) || Arg <- ExprArgs],
          NewArgs = lists:zip(ExprVars, ArgTypes),

          Func = aero_core:c_func([], NewArgs, ResultType, Where, lift(ExprBody, BodyEnv)),
          {aero_core:c_def_func([], Path, Vis, Func), DefEnv};
        {c_func, _, _, _, _, _} ->
          throw({expand_error, {func_def_eq_arity_mismatch, FuncMeta}});
        _ ->
          throw({expand_error, {func_def_eq_body_invalid, aero_ast:meta(FuncBody)}})
      end;
    _ ->
      throw({expand_error, {func_def_eq_head_invalid, aero_ast:meta(FuncHead)}})
  end;
expand_func_def([FuncHead, FuncBody], _FuncMeta, Vis, Env) ->
  % Function definition variant with assignment to an anonymous function on the right.
  {Path, Args, Result, Where, DefEnv, BodyEnv} = expand_func_def_head(FuncHead, Env),
  Body = expand_func_def_body(FuncBody, BodyEnv),

  Func = aero_core:c_func([], Args, Result, Where, Body),
  {aero_core:c_def_func([], Path, Vis, Func), DefEnv};
expand_func_def(_, FuncMeta, _, _) ->
  throw({expand_error, {func_def_invalid, FuncMeta}}).

expand_func_def_head(FuncHead, Env) ->
  expand_func_def_head(FuncHead, [], Env).

expand_func_def_head({expand, _, {op, _, '_where_'}, [FuncHeadLeft, Clause]}, Wheres, Env) ->
  {WhereTypes, HeadEnv} = aero_expand_type:expand_where_clauses(Wheres, Env),

  expand_func_def_head(FuncHeadLeft, [Clause | WhereTypes], HeadEnv);
expand_func_def_head({expand, FuncHeadMeta, {op, _, Arrow}, [{args, _, LeftArrowArgs}, Result]},
                     Where,
                     Env) when Arrow =:= '_->_'; Arrow =:= '_->>_' ->
  % TODO: check when pure.
  case LeftArrowArgs of
    [{expand, _, {op, _, '_(_)'}, [{ident, _, _} = Ident, {args, _, Args}]}] ->
      check_existing_def(Env, Ident),
      {DefEnv, Path} = aero_env:register_def(Env, Ident),

      {CoreArgs, ResultEnv} =
        lists:foldl(fun(Arg, {ArgAcc, EnvAcc}) ->
          case Arg of
            {tag, _, {ident, _, _} = ArgIdent, Type} ->
              {ArgEnv, ArgVar} = aero_env:register_var(EnvAcc, ArgIdent),
              {ArgType, NewEnv} = aero_expand_type:expand_type(Type, ArgEnv),

              {[{ArgVar, ArgType} | ArgAcc], NewEnv};
            _ ->
              throw({expand_error, {func_def_arg_invalid, aero_ast:meta(Arg)}})
          end
        end, {[], aero_env:reset_counter(Env)}, Args),
      {ResultType, BodyEnv} = aero_expand_type:expand_type(Result, ResultEnv),

      {Path, lists:reverse(CoreArgs), ResultType, Where, DefEnv, BodyEnv};
    _ ->
      throw({expand_error, {func_def_head_invalid, FuncHeadMeta}})
  end;
expand_func_def_head(FuncHead, _, _Env) ->
  throw({expand_error, {func_def_head_invalid, aero_ast:meta(FuncHead)}}).

expand_func_def_body({block, _, _} = Block, Env) ->
  lift(expand_expr(Block, Env), Env);
expand_func_def_body(FuncBody, _Env) ->
  throw({expand_error, {func_def_body_invalid, aero_ast:meta(FuncBody)}}).

expand_const_def([{expand, _, {op, _, '_=_'}, [ConstLeft, ConstExpr]}], ConstMeta, Vis, Env) ->
  case ConstLeft of
    {tag, _, {ident, _, _} = Ident, TagType} ->
      check_existing_def(Env, Ident),
      {DefEnv, Path} = aero_env:register_def(Env, Ident),
      {Type, BodyEnv} = aero_expand_type:expand_type(TagType, aero_env:reset_counter(Env)),
      Expr = lift(expand_expr(ConstExpr, BodyEnv), BodyEnv),

      {aero_core:c_def_const([], Path, Vis, Type, Expr), DefEnv};
    {ident, _, _} ->
      throw({expand_error, {const_def_missing_type, ConstMeta}});
    _ ->
      throw({expand_error, {const_def_invalid, ConstMeta}})
  end;
expand_const_def(_, ConstMeta, _, _) ->
  throw({expand_error, {const_def_invalid, ConstMeta}}).

%% -----------------------------------------------------------------------------
%% Expression Expanding
%% -----------------------------------------------------------------------------

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
            {CorePat, CoreEnv} = expand_pat(Pat, Env),
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
            {CorePat, PatEnv} = expand_pat(Pat, Env),
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
%% Pattern Expanding
%% -----------------------------------------------------------------------------

%% Patterns.
expand_pat(Ast, Env) ->
  % Patterns store the variables bound during this pattern to prevent them from
  % being repeating.
  {Pat, PatEnv} = expand_pat_outer(Ast, Env),
  {Pat, aero_env:clear_pat_vars(PatEnv)}.

%% Args pattern.
expand_pat_outer({args, _, Args}, Env) ->
  {PatArgs, PatEnv} =
    lists:foldl(fun(Arg, {Acc, AccEnv}) ->
      {PatArg, NewEnv} = expand_pat_inner(Arg, AccEnv),
      {[PatArg | Acc], NewEnv}
    end, {[], Env}, Args),

  {aero_core:c_pat_args([], lists:reverse(PatArgs)), PatEnv};

expand_pat_outer(Pat, Env) ->
  expand_pat_inner(Pat, Env).

%% Pattern literals.
expand_pat_inner({ident, _, Bool}, Env) when Bool =:= true; Bool =:= false ->
  {aero_core:c_pat_bool([], Bool), Env};
expand_pat_inner({int_lit, _, Integer}, Env) ->
  {aero_core:c_pat_int([], Integer), Env};
expand_pat_inner({float_lit, _, Float}, Env) ->
  {aero_core:c_pat_float([], Float), Env};
expand_pat_inner({atom_lit, _, Atom}, Env) ->
  {aero_core:c_pat_atom([], Atom), Env};
expand_pat_inner({str_lit, _, String}, Env) ->
  {aero_core:c_pat_str([], String), Env};

%% Unit pattern.
expand_pat_inner({expand, _, {op, _, '(_)'}, [{args, _, []}]}, Env) ->
  {aero_core:c_pat_unit([]), Env};

%% Tuple pattern.
expand_pat_inner({expand, _, {op, _, '(_)'}, [{args, _, Args}]}, Env) when length(Args) > 1 ->
  {Elems, PatEnv} =
    lists:foldl(fun(Arg, {Acc, AccEnv}) ->
      {Elem, NewEnv} = expand_pat_inner(Arg, AccEnv),
      {[Elem | Acc], NewEnv}
    end, {[], Env}, Args),

  {aero_core:c_pat_tuple([], lists:reverse(Elems)), PatEnv};

%% Cons and nil pattern.
expand_pat_inner({expand, _, {op, _, '_::_'}, [Head, Tail]}, Env) ->
  {HeadPat, HeadEnv} = expand_pat_inner(Head, Env),
  {TailPat, TailEnv} = expand_pat_inner(Tail, HeadEnv),

  {aero_core:c_pat_cons([], HeadPat, TailPat), TailEnv};
expand_pat_inner({ident, _, nil}, Env) ->
  {aero_core:c_pat_nil([]), Env};

%% Dictionary pattern.
expand_pat_inner({expand, _, {op, _, '#{_}'}, [{args, _, Args}]}, Env) ->
  {Pairs, PatEnv} =
    lists:foldl(fun(Arg, {Acc, AccEnv}) ->
      case Arg of
        {expand, _, {op, _, '_=>_'}, [Key, Value]} ->
          {KeyPat, KeyEnv} = expand_pat_inner(Key, AccEnv),
          {ValuePat, ValueEnv} = expand_pat_inner(Value, KeyEnv),

          {[{KeyPat, ValuePat} | Acc], ValueEnv};

        % We can have a tag in a dictionary pattern for #{ atom: pat } syntax.
        % Needing to corece the left side into an atom.
        {tag, _, {ident, _, Key}, Value} ->
          KeyPat = aero_core:c_pat_atom([], Key),
          {ValuePat, ValueEnv} = expand_pat_inner(Value, AccEnv),

          {[{KeyPat, ValuePat} | Acc], ValueEnv}
      end
    end, {[], Env}, Args),

  {aero_core:c_pat_dict([], lists:reverse(Pairs)), PatEnv};

%% Pattern variable.
expand_pat_inner({ident, _, _} = Ident, Env) ->
  check_existing_pat_var(Env, Ident),
  {NewEnv, PatVar} = aero_env:register_pat_var(Env, Ident),
  {PatVar, NewEnv};

%% Wildcard.
expand_pat_inner({blank, _}, Env) ->
  {aero_env:wildcard_pat_var(Env), Env};

%% Constructor patterns.
expand_pat_inner({expand, Meta, {op, _, '_(_)'},
                                [{expand, _, {op, _, '#_'}, [Path]}, {args, _, Args}]},
                 Env) ->
  case Path of
    {ident, _, list} ->
      lists:foldr(fun(Arg, {Acc, AccEnv}) ->
        {Head, NewEnv} = expand_pat_inner(Arg, AccEnv),
        {aero_core:c_pat_cons([], Head, Acc), NewEnv}
      end, {aero_core:c_pat_nil([]), Env}, Args);
    _ ->
      throw({expand_error, {pat_constructor_invalid, Meta}})
  end;

%% Anything else...
expand_pat_inner(Pat, _Env) ->
  throw({expand_error, {pat_invalid, aero_ast:meta(Pat)}}).

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

check_existing_def(Env, Ident) ->
  case aero_env:lookup_def(Env, Ident) of
    undefined -> ok;
    Def       -> throw({expand_error, {def_exists, aero_ast:meta(Ident), Def}})
  end.

check_existing_pat_var(Env, Ident) ->
  case aero_env:lookup_pat_var(Env, Ident) of
    undefined -> ok;
    PatVar    -> throw({expand_error, {pat_var_exists, aero_ast:meta(Ident), PatVar}})
  end.

erl_call(Mod, Func, Args) ->
  Callee = aero_core:c_path([erl_path], [aero_core:c_var([], Mod), aero_core:c_var([], Func)]),
  aero_core:c_call([], Callee, Args).

lift({c_block, Meta, Exprs}, Env) ->
  aero_core:c_block(Meta, [lift(Expr, Env) || Expr <- Exprs]);
lift({c_tuple, Meta, Exprs}, Env) ->
  {Lets, NewExprs} =
    lists:foldl(fun(Expr, {AccLets, AccExprs}) ->
      LiftedExpr = lift(Expr, Env),
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
lift({c_cons, Meta, Head, Tail}, Env) ->
  LiftedHead = lift(Head, Env),
  LiftedTail = lift(Tail, Env),
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
lift({c_dict, Meta, Pairs}, Env) ->
  {Lets, NewPairs} =
    lists:foldl(fun({Key, Value}, {AccLets, AccPairs}) ->
      LiftedKey = lift(Key, Env),
      LiftedValue = lift(Value, Env),
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
lift({c_func, Meta, Args, Result, Where, Body}, Env) ->
  aero_core:c_func(Meta, Args, Result, Where, lift(Body, Env));
lift({Call, Meta, Path, Args}, Env) when Call =:= c_call; Call =:= c_apply ->
  {Lets, NewArgs} =
    lists:foldl(fun(Arg, {AccLets, AccArgs}) ->
      LiftedArg = lift(Arg, Env),
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
lift({c_let, Meta, Var, Type, Expr}, Env) ->
  aero_core:c_let(Meta, Var, Type, lift(Expr, Env));
lift({c_letrec, Meta, Var, Type, Func}, Env) ->
  aero_core:c_letrec(Meta, Var, Type, lift(Func, Env));
lift({c_match, Meta, Expr, Cases}, Env) ->
  LiftedExpr = lift(Expr, Env),
  LiftedCases = lists:map(fun({Pat, Body}) -> {Pat, lift(Body, Env)} end, Cases),
  case is_simple(LiftedExpr) of
    true ->
      aero_core:c_match(Meta, LiftedExpr, LiftedCases);
    false ->
      Var = aero_env:tmp_var(Env),
      Let = aero_core:c_let([], Var, aero_env:inferred_type_var(Env), LiftedExpr),

      aero_core:c_block(Meta, [Let, aero_core:c_match(Meta, Var, LiftedCases)])
  end;
lift(Expr, _Env) ->
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
is_literal({c_atom, _, _})  -> true;
is_literal({c_str, _, _})   -> true;
is_literal({c_nil, _})      -> true;
is_literal({c_unit, _})     -> true;
is_literal(_)               -> false.

is_var({c_var, _, _})  -> true;
is_var({c_path, _, _}) -> true;
is_var(_)              -> false.

is_when_exhaustive([]) ->
  false;
is_when_exhaustive(Clauses) ->
  case lists:last(Clauses) of
    {{c_bool, _, true}, _} -> true;
    _                      -> false
  end.
