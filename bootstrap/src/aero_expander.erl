%%% Converts the Aero AST down to Core Aero.
%%%
%%% This format is made to resemble Core Erlang with variables renamed and
%%% macros expanded giving a simple core language.
%%%
%%% For now we don't really have macros, so they're just manually expanded here.

-module(aero_expander).

-export([expand/2]).
-export_type([c_pkg/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Top-level of Core Aero, represents a whole package.
-type c_pkg() :: {c_pkg, meta(), atom(), [c_mod()]}.

-type c_mod()       :: {c_mod, meta(), c_path(), c_mod_attrs(), [c_def()]}.
-type c_mod_attrs() :: [{c_atom(), c_expr()}].

%% Definitions.
-type c_def() :: {c_def_func, meta(), c_path(), c_vis(), c_func()}
               | {c_def_const, meta(), c_path(), c_vis(), c_type_inner(), c_expr()}.

%% Definition visibility.
-type c_vis() :: c_vis_pub
               | c_vis_priv.

%% Any expression in Core Aero.
-type c_expr() :: c_block()
                | c_bool()
                | c_int()
                | c_float()
                | c_atom()
                | c_str()
                | c_unit()
                | c_tuple()
                | c_cons()
                | c_nil()
                | c_dict()
                | c_func()
                | c_call()
                | c_apply()
                | c_var()
                | c_path()
                | c_let()
                | c_letrec()
                | c_match()
                | c_args().

%% A group of expressions with the last giving the value of the group.
-type c_block() :: {c_block, meta(), [c_expr()]}.

%% Literals.
-type c_bool()  :: {c_bool, meta(), boolean()}.
-type c_int()   :: {c_int, meta(), integer()}.
-type c_float() :: {c_float, meta(), float()}.
-type c_atom()  :: {c_atom, meta(), atom()}.
-type c_str()   :: {c_str, meta(), binary()}.

%% Unit value.
-type c_unit() :: {c_unit, meta()}.

%% Tuple.
-type c_tuple() :: {c_tuple, meta(), [c_expr()]}.

%% Cons and nil.
-type c_cons() :: {c_cons, meta(), c_expr(), c_cons() | c_nil()}.
-type c_nil()  :: {c_nil, meta()}.

%% Dictionary.
-type c_dict() :: {c_dict, meta(), [c_dict_pair()]}.

-type c_dict_pair() :: {c_expr(), c_expr()}.

%% A function expression.
-type c_func() :: {c_func, meta(), c_func_args(), c_func_result(), c_func_where(), c_func_body()}.

-type c_func_args()   :: [{c_var(), c_type_inner()}].
-type c_func_result() :: c_type_inner().
-type c_func_where()  :: c_type_where().
-type c_func_body()   :: c_expr().

%% A call to a named function.
-type c_call() :: {c_call, meta(), c_path(), c_call_args()}.

-type c_call_args() :: [c_expr()].

%% A call to an anonymous function.
-type c_apply() :: {c_apply, meta(), c_var(), c_apply_args()}.

-type c_apply_args() :: [c_expr()].

%% Variables.
-type c_var() :: {c_var, meta(), atom()}.

%% Paths.
-type c_path() :: {c_path, [], [c_var()]}.

%% Let and letrec expressions.
-type c_let()    :: {c_let, meta(), c_var(), c_type(), c_expr()}.
-type c_letrec() :: {c_letrec, meta(), c_var(), c_type(), c_func()}.

%% Match.
-type c_match() :: {c_match, meta(), c_expr(), c_match_cases()}.

-type c_match_cases() :: [{c_pat(), c_expr()}].

%% A list of expressions from function arguments.
-type c_args() :: {c_args, meta(), [c_expr()]}.

%% Patterns that may or may not match.
-type c_pat() :: c_pat_outer()
               | c_pat_inner().

-type c_pat_outer() :: {c_pat_args, meta(), c_pat_inner()}.

-type c_pat_inner() :: {c_pat_bool, meta(), boolean()}
                     | {c_pat_int, meta(), integer()}
                     | {c_pat_float, meta(), float()}
                     | {c_pat_atom, meta(), atom()}
                     | {c_pat_str, meta(), binary()}
                     | {c_pat_unit, meta()}
                     | {c_pat_tuple, meta(), [c_pat()]}
                     | {c_pat_cons, meta(), c_pat(), [c_pat()]}
                     | {c_pat_nil, meta()}
                     | {c_pat_dict, meta(), [{c_pat(), c_pat()}]}
                     | {c_pat_var, meta(), atom()}.

%% Any type in Core Aero.
-type c_type() :: {c_type, meta(), c_type_inner(), c_type_where()}.

-type c_type_inner() :: c_type_bool
                      | c_type_int
                      | c_type_float
                      | c_type_atom
                      | c_type_str
                      | c_type_bytes
                      | c_type_bits
                      | c_type_ref
                      | c_type_unit
                      | {c_type_tuple, [c_type_inner()]}
                      | {c_type_list, c_type_inner()}
                      | {c_type_dict, c_type_inner(), c_type_inner()}
                      | {c_type_func, [c_type_inner()], c_type_inner()}
                      | {c_type_uniq, c_type_inner()} 
                      | {c_type_dyn, c_type_inner()}
                      | c_type_wld
                      | c_type_never
                      | {c_type_mbox, c_type_inner()}
                      | {c_type_addr, c_type_inner()}
                      | {c_type_param, atom()}
                      | {c_type_tag, atom()}
                      | {c_type_struct, c_path(), [c_type_inner()]}
                      | {c_type_proto, c_path(), [c_type_inner()]}
                      | {c_type_union, [c_type_inner()]}
                      | {c_type_inter, [c_type_inner()]}.
-type c_type_where() :: [{c_type_inner(), c_type_inner()}].

-type meta() :: [term()].

-spec expand(aero_ast:ast(), aero_context:context()) -> {ok, c_pkg()} | {error, term()}.
expand(Source, Context) ->
  try expand_source(Source, Context) of
    Package -> {ok, Package}
  catch
    throw:{expand_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Definition Expanding
%% -----------------------------------------------------------------------------

expand_source({source, _, SourceArgs}, Context) ->
  PkgName = aero_session:pkg(),
  ModName =
    case aero_session:pkg() of
      aero -> binary_to_atom(filename:basename(aero_context:filename(Context), ".aero"), utf8);
      Pkg  -> Pkg
    end,

  {Defs, InnerModules} =
    lists:foldl(fun(SourceArg, {Defs, InnerModules}) ->
      {[expand_def(SourceArg, Context) | Defs], InnerModules}
    end, {[], []}, SourceArgs),

  ModulePath = {c_path, [], [{c_var, [], ModName}]},
  Module = {c_mod, [], ModulePath, [], lists:reverse(Defs)},

  {c_pkg, [], PkgName, [Module | lists:reverse(InnerModules)]};
expand_source(_, _) ->
  throw({expand_error, no_source}).

%% Private definitions.
expand_def({expand, Meta, {ident, _, func}, Args}, _Context) ->
  expand_func_def(Args, Meta, c_vis_priv);
expand_def({expand, Meta, {ident, _, const}, Args}, _Context) ->
  expand_const_def(Args, Meta, c_vis_priv);

%% Public definitions.
expand_def({expand, Meta, {ident, _, pub}, PubArgs}, _Context) ->
  case PubArgs of
    [{expand, _, {ident, _, func}, Args}] ->
      expand_func_def(Args, Meta, c_vis_pub);
    [{expand, _, {ident, _, const}, Args}] ->
      expand_const_def(Args, Meta, c_vis_pub);
    _ ->
      throw({expand_error, {pub_invalid, aero_ast:meta(Meta)}})
  end;

%% Anything else...
expand_def(Def, _Context) ->
  throw({expand_error, {def_invalid, aero_ast:meta(Def)}}).

expand_func_def([{expand, _, {op, _, '_=_'}, [FuncHead, FuncBody]}], FuncMeta, Vis) ->
  % Function definition variant with assignment to an anonymous function on the right.
  Where = [],
  case FuncHead of
    {tag, _, {ident, _, Name}, {expand, _, {op, _, Arrow}, [{args, _, LeftArrowArgs}, Result]}}
        when Arrow =:= '_->_'; Arrow =:= '_->>_' ->
      % For function head.
      Path = {c_path, [], [{c_var, [], Name}]},
      ArgTypes = [expand_type_inner(Arg) || Arg <- LeftArrowArgs],
      ResultType = expand_type_inner(Result),

      % Expanding body and ensuring it gives a function.
      Env = new_env(),
      case expand_expr(FuncBody, Env) of
        {c_func, _, ExprArgs, _, _, ExprBody} when length(ExprArgs) =:= length(ArgTypes) ->
          ExprVars = [element(1, Arg) || Arg <- ExprArgs],
          NewArgs = lists:zip(ExprVars, ArgTypes),

          Func = {c_func, [], NewArgs, ResultType, Where, lift(ExprBody, Env)},
          {c_def_func, [], Path, Vis, Func};
        {c_func, _, _, _, _, _} ->
          throw({expand_error, {func_def_eq_arity_mismatch, FuncMeta}});
        _ ->
          throw({expand_error, {func_def_eq_body_invalid, aero_ast:meta(FuncBody)}})
      end;
    _ ->
      throw({expand_error, {func_def_eq_head_invalid, aero_ast:meta(FuncHead)}})
  end;
expand_func_def([FuncHead, FuncBody], _FuncMeta, Vis) ->
  % Function definition variant with assignment to an anonymous function on the right.
  {Path, Args, Result, Where, Env} = expand_func_def_head(FuncHead, new_env()),
  Body = expand_func_def_body(FuncBody, Env),

  Func = {c_func, [], Args, Result, Where, Body},
  {c_def_func, [], Path, Vis, Func};
expand_func_def(_, FuncMeta, _) ->
  throw({expand_error, {func_def_invalid, FuncMeta}}).

expand_func_def_head(FuncHead, Env) ->
  expand_func_def_head(FuncHead, [], Env).

expand_func_def_head({expand, _, {op, _, '_where_'}, [FuncHeadLeft, Clause]}, Where, Env) ->
  expand_func_def_head(FuncHeadLeft, [Clause | expand_type_where(Where)], Env);
expand_func_def_head({expand, FuncHeadMeta, {op, _, Arrow}, [{args, _, LeftArrowArgs}, Result]},
                     Where,
                     Env) when Arrow =:= '_->_'; Arrow =:= '_->>_'->
  % TODO: check when pure.
  case LeftArrowArgs of
    [{expand, _, {op, _, '_(_)'}, [{ident, _, Name}, {args, _, Args}]}] ->
      Path = {c_path, [], [{c_var, [], Name}]},
      {CoreArgs, BodyEnv} =
        lists:foldl(fun(Arg, {ArgAcc, EnvAcc}) ->
          case Arg of
            {tag, _, {ident, _, _} = Ident, Type} ->
              {NewEnv, ArgVar} = register_var(EnvAcc, Ident),
              ArgType = expand_type_inner(Type),

              {[{ArgVar, ArgType} | ArgAcc], NewEnv};
            _ ->
              throw({expand_error, {func_def_arg_invalid, aero_ast:meta(Arg)}})
          end
        end, {[], Env}, Args),
      ResultType = expand_type_inner(Result),

      {Path, lists:reverse(CoreArgs), ResultType, Where, BodyEnv};
    _ ->
      throw({expand_error, {func_def_head_invalid, FuncHeadMeta}})
  end;
expand_func_def_head(FuncHead, _, _Env) ->
  throw({expand_error, {func_def_head_invalid, aero_ast:meta(FuncHead)}}).

expand_func_def_body({block, _, _} = Block, Env) ->
  lift(expand_expr(Block, Env), Env);
expand_func_def_body(FuncBody, _Env) ->
  throw({expand_error, {func_def_body_invalid, aero_ast:meta(FuncBody)}}).

expand_const_def([{expand, _, {op, _, '_=_'}, [{tag, _, {ident, _, Name}, TagType}, ConstExpr]}],
                 _ConstMeta,
                 Vis) ->
  Path = {c_path, [], [{c_var, [], Name}]},
  Type = expand_type_inner(TagType),
  Expr = expand_expr(ConstExpr, new_env()),

  {c_def_const, [], Path, Vis, Type, Expr};
expand_const_def(_, ConstMeta, _) ->
  throw({expand_error, {const_def_invalid, ConstMeta}}).

%% -----------------------------------------------------------------------------
%% Expression Expanding
%% -----------------------------------------------------------------------------

%% Blocks.
expand_expr({block, _, []}, _Env) ->
  {c_unit, []};
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
      Vars = [register_tmp(Env) || _ <- lists:seq(1, Arity)],
      FuncVars = [{Var, inferred_type()} || Var <- Vars],
      Match = {c_match, [], {c_args, [], Vars}, FuncCases},

      {c_func, [], FuncVars, inferred_type(), [], Match}
  end;
expand_expr({block, _, BlockExprs}, Env) ->
  {Exprs, _} =
    lists:foldl(fun(BlockExpr, {ExprAcc, EnvAcc}) ->
      case BlockExpr of
        {expand, _, {op, _, '_=_'}, [Ident, RightExpr]} ->
          {NewEnv, Var} = register_var(EnvAcc, Ident),
          LetExpr = {c_let, [], Var, inferred_type(), expand_expr(RightExpr, Env)},

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
      {c_block, [], lists:reverse(lists:map(fun(Expr) ->
        case Expr of
          {c_let, _, _, _, _} ->
            Expr;
          _ ->
            Var = element(2, register_var(Env, {ident, [], '_'})),
            {c_let, [], Var, inferred_type(), Expr}
        end
      end, tl(Exprs))) ++ [hd(Exprs)]}
  end;

%% Literals.
expand_expr({ident, _, Bool}, _Env) when Bool =:= true; Bool =:= false ->
  {c_bool, [], Bool};
expand_expr({int_lit, _, Integer}, _Env) ->
  {c_int, [], Integer};
expand_expr({float_lit, _, Float}, _Env) ->
  {c_float, [], Float};
expand_expr({atom_lit, _, Atom}, _Env) ->
  {c_atom, [], Atom};
expand_expr({str_lit, _, String}, _Env) ->
  {c_str, [], String};

%% Unit value.
expand_expr({expand, _, {op, _, '(_)'}, [{args, _, []}]}, _Env) ->
  {c_unit, []};

%% Tuples.
expand_expr({expand, _, {op, _, '(_)'}, [{args, _, Args}]}, Env) when length(Args) > 1 ->
  {c_tuple, [], [expand_expr(Arg, Env) || Arg <- Args]};

%% Cons and nil.
expand_expr({expand, _, {op, _, '_::_'}, [Head, Tail]}, Env) ->
  {c_cons, [], expand_expr(Head, Env), expand_expr(Tail, Env)};
expand_expr({ident, _, nil}, _Env) ->
  {c_nil, []};

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
          {{c_atom, [], Key}, expand_expr(Value, Env)}
      end
    end, Args),
  {c_dict, [], Pairs};

%% Anonymous functions.
expand_expr({expand, _, {op, _, Arrow}, [{args, _, Args}, Body]}, Env) when Arrow =:= '_->_';
                                                                            Arrow =:= '_->>_' ->
  % If the function is in the form `name(args) -> body`, then extract the name
  % for a recusive anonymous function.
  {FuncArgs, {HeadEnv, Var}} =
    case Args of
      [{expand, _, {op, _, '_(_)'}, [FuncIdent, {args, _, InnerArgs}]}] ->
        {InnerArgs, register_var(Env, FuncIdent)};
      _ ->
        {Args, {Env, none}}
    end,
  % Not allowing any types to be used in anonymous functions.
  {CoreArgs, BodyEnv} =
    lists:foldl(fun(Arg, {ArgAcc, EnvAcc}) ->
      case Arg of
        {ident, _, _} = Ident ->
          {NewEnv, ArgVar} = register_var(EnvAcc, Ident),
          ArgType = inferred_type(),

          {[{ArgVar, ArgType} | ArgAcc], NewEnv};
        _ ->
          throw({expand_error, {func_arg_invalid, aero_ast:meta(Arg)}})
      end
    end, {[], HeadEnv}, FuncArgs),
  CoreBody = expand_expr(Body, BodyEnv),
  Func = {c_func, [], lists:reverse(CoreArgs), inferred_type(), [], CoreBody},

  % If recursive then wrap it into a block with a letrec.
  case Var of
    none -> Func;
    _    -> {c_block, [], [{c_letrec, [], Var, inferred_type(), Func}, Var]}
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
            {c_cons, [], expand_expr(Arg, Env), Acc}
          end, {c_nil, []}, Args);
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
        {c_var, _, _} -> {c_apply, [], CoreCallee, CoreArgs};
        _             -> {c_call, [], CoreCallee, CoreArgs}
      end
  end;

%% Variables.
expand_expr({ident, _, Name} = Ident, Env) ->
  case lookup_var(Env, Ident) of
    undefined -> {c_path, [], [{c_var, [], Name}]};
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
  LeftVar = register_tmp(Env),
  RightVar = register_tmp(Env),

  LeftExpr = expand_expr(Left, Env),
  RightExpr = expand_expr(Right, Env),

  IsLeftInt = erl_call(erlang, is_integer, [LeftVar]),
  IsRightInt = erl_call(erlang, is_integer, [RightVar]),

  IntDiv = erl_call(erlang, 'div', [LeftVar, RightVar]),
  FloatDiv = erl_call(erlang, '/', [LeftVar, RightVar]),

  {c_block, [], [
    {c_let, [], LeftVar, inferred_type(), LeftExpr},
    {c_let, [], RightVar, inferred_type(), RightExpr},
    {c_match, [], IsLeftInt, [
      {{c_pat_bool, [], true}, {c_match, [], IsRightInt, [
        {{c_pat_bool, [], true}, IntDiv},
        {register_pat_wildcard(Env), FloatDiv}
      ]}},
      {register_pat_wildcard(Env), FloatDiv}
    ]}
  ]};
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
    {{c_pat_bool, [], true}, expand_expr(Right, Env)},
    {register_pat_wildcard(Env), {c_bool, [], false}}
  ],
  {c_match, [], expand_expr(Left, Env), Cases};

expand_expr({expand, _, {op, _, '_or_'}, [Left, Right]}, Env) ->
  % Manually short-circuit with `match`.
  Cases = [
    {{c_pat_bool, [], true}, {c_bool, [], true}},
    {register_pat_wildcard(Env), expand_expr(Right, Env)}
  ],
  {c_match, [], expand_expr(Left, Env), Cases};

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
          Exprs  -> {c_tuple, [], [expand_expr(Expr, Env) || Expr <- Exprs]}
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

      {c_match, [], CoreExpr, Cases};
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
        {c_match, [], Cond, [{{c_pat_bool, [], true}, Expr}, {register_pat_wildcard(Env), Inner}]}
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
        {{c_pat_bool, [], true}, expand_expr(Then, Env)},
        {register_pat_wildcard(Env), expand_expr(Else, Env)}
      ],
      {c_match, [], expand_expr(Cond, Env), Cases};

    % No else block, so wrapping with optional.
    {block, _, _} = Then ->
      Cases = [
        {{c_pat_bool, [], true}, {c_tuple, [], [{c_atom, [], some}, expand_expr(Then, Env)]}},
        {register_pat_wildcard(Env), {c_atom, [], none}}
      ],
      {c_match, [], expand_expr(Cond, Env), Cases};

    _ ->
      throw({expand_error, {if_invalid, Meta}})
  end;

expand_expr({expand, Meta, {ident, _, 'if'}, _}, _Env) ->
  throw({expand_error, {if_invalid, Meta}});

%% Logs.
expand_expr({expand, _, {ident, _, log}, [Message]}, Env) ->
  Args = [
    {c_atom, [], standard_io},
    {c_cons, [], expand_expr(Message, Env), {c_cons, [], {c_int, [], $\n}, {c_nil, []}}}
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
  {Pat, clear_pat_vars(PatEnv)}.

%% Args pattern.
expand_pat_outer({args, _, Args}, Env) ->
  {PatArgs, PatEnv} =
    lists:foldl(fun(Arg, {Acc, AccEnv}) ->
      {PatArg, NewEnv} = expand_pat_inner(Arg, AccEnv),
      {[PatArg | Acc], NewEnv}
    end, {[], Env}, Args),

  {{c_pat_args, [], lists:reverse(PatArgs)}, PatEnv};

expand_pat_outer(Pat, Env) ->
  expand_pat_inner(Pat, Env).

%% Pattern literals.
expand_pat_inner({ident, _, Bool}, Env) when Bool =:= true; Bool =:= false ->
  {{c_pat_bool, [], Bool}, Env};
expand_pat_inner({int_lit, _, Integer}, Env) ->
  {{c_pat_int, [], Integer}, Env};
expand_pat_inner({float_lit, _, Float}, Env) ->
  {{c_pat_float, [], Float}, Env};
expand_pat_inner({atom_lit, _, Atom}, Env) ->
  {{c_pat_atom, [], Atom}, Env};
expand_pat_inner({str_lit, _, String}, Env) ->
  {{c_pat_str, [], String}, Env};

%% Unit pattern.
expand_pat_inner({expand, _, {op, _, '(_)'}, [{args, _, []}]}, Env) ->
  {{c_pat_unit, []}, Env};

%% Tuple pattern.
expand_pat_inner({expand, _, {op, _, '(_)'}, [{args, _, Args}]}, Env) when length(Args) > 1 ->
  {Elems, PatEnv} =
    lists:foldl(fun(Arg, {Acc, AccEnv}) ->
      {Elem, NewEnv} = expand_pat_inner(Arg, AccEnv),
      {[Elem | Acc], NewEnv}
    end, {[], Env}, Args),

  {{c_pat_tuple, [], lists:reverse(Elems)}, PatEnv};

%% Cons and nil pattern.
expand_pat_inner({expand, _, {op, _, '_::_'}, [Head, Tail]}, Env) ->
  {HeadPat, HeadEnv} = expand_pat_inner(Head, Env),
  {TailPat, TailEnv} = expand_pat_inner(Tail, HeadEnv),

  {{c_pat_cons, [], HeadPat, TailPat}, TailEnv};
expand_pat_inner({ident, _, nil}, Env) ->
  {{c_pat_nil, []}, Env};

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
          KeyPat = {c_pat_atom, [], Key},
          {ValuePat, ValueEnv} = expand_pat_inner(Value, AccEnv),

          {[{KeyPat, ValuePat} | Acc], ValueEnv}
      end
    end, {[], Env}, Args),

  {{c_pat_dict, [], lists:reverse(Pairs)}, PatEnv};

%% Pattern variable.
expand_pat_inner({ident, _, _} = Ident, Env) ->
  {NewEnv, PatVar} = register_pat_var(Env, Ident),
  {PatVar, NewEnv};

%% Wildcard.
expand_pat_inner({blank, _}, Env) ->
  {register_pat_wildcard(Env), Env};

%% Constructor patterns.
expand_pat_inner({expand, Meta, {op, _, '_(_)'},
                                [{expand, _, {op, _, '#_'}, [Path]}, {args, _, Args}]},
                 Env) ->
  case Path of
    {ident, _, list} ->
      lists:foldr(fun(Arg, {Acc, AccEnv}) ->
        {Head, NewEnv} = expand_pat_inner(Arg, AccEnv),
        {{c_pat_cons, [], Head, Acc}, NewEnv}
      end, {{c_pat_nil, []}, Env}, Args);
    _ ->
      throw({expand_error, {pat_constructor_invalid, Meta}})
  end;

%% Anything else...
expand_pat_inner(Pat, _Env) ->
  throw({expand_error, {pat_invalid, aero_ast:meta(Pat)}}).

%% -----------------------------------------------------------------------------
%% Type Expanding
%% -----------------------------------------------------------------------------

%% Builtins.
expand_type_inner({ident, _, bool}) ->
  c_type_bool;
expand_type_inner({ident, _, int}) ->
  c_type_int;
expand_type_inner({ident, _, float}) ->
  c_type_float;
expand_type_inner({ident, _, atom}) ->
  c_type_atom;
expand_type_inner({ident, _, str}) ->
  c_type_str;
expand_type_inner({ident, _, bytes}) ->
  c_type_bytes;
expand_type_inner({ident, _, bits}) ->
  c_type_bits;
expand_type_inner({ident, _, ref}) ->
  c_type_ref;

%% Unit type.
expand_type_inner({expand, _, {op, _, '(_)'}, [{args, _, []}]}) ->
  c_type_unit;

%% Collections.
expand_type_inner({expand, _, {op, _, '(_)'}, [{args, _, Args}]}) when length(Args) > 1 ->
  {c_type_tuple, lists:map(fun expand_type_inner/1, Args)};
expand_type_inner({expand, _, {ident, _, list}, [T]}) ->
  {c_type_list, expand_type_inner(T)};
expand_type_inner({expand, _, {ident, _, dict}, [K, V]}) ->
  {c_type_dict, expand_type_inner(K), expand_type_inner(V)};

%% Functions.
expand_type_inner({expand, _, {op, _, Arrow}, [{args, _, Args}, Result]})
    when Arrow =:= '_->_'; Arrow =:= '_->>_' ->
  {c_type_func, lists:map(fun expand_type_inner/1, Args), expand_type_inner(Result)};

%% Concurrent primitives.
expand_type_inner({ident, _, wld}) ->
  c_type_wld;
expand_type_inner({ident, _, never}) ->
  c_type_never;
expand_type_inner({expand, _, {ident, _, mbox}, [T]}) ->
  {c_type_mbox, expand_type_inner(T)};
expand_type_inner({expand, _, {ident, _, addr}, [T]}) ->
  {c_type_addr, expand_type_inner(T)};

%% Type parameters.
expand_type_inner({type_param, _, TParam}) ->
  {c_type_param, TParam};

%% Option and Result type macros.
expand_type_inner({expand, _, {op, _, '_?'}, [Type]}) ->
  SomeInner =
    case expand_type_inner(Type) of
      {c_type_tuple, InnerTypes} -> InnerTypes;
      InnerType                  -> [InnerType]
    end,
  Some = {c_type_tuple, [{c_type_tag, some} | SomeInner]},
  None = {c_type_tag, none},

  {c_type_union, [Some, None]};
expand_type_inner({expand, _, {op, _, '_!'}, [Type]}) ->
  OkInner =
    case expand_type_inner(Type) of
      {c_type_tuple, InnerTypes} -> InnerTypes;
      InnerType                  -> [InnerType]
    end,
  Ok = {c_type_tuple, [{c_type_tag, ok} | OkInner]},
  ErrorInner = {c_type_proto, {c_path, [], [{c_var, [], aero_std}, {c_var, [], 'Error'}]}, []},
  Error = {c_type_tuple, [{c_type_tag, error}, ErrorInner]},

  {c_type_union, [Ok, Error]};

%% Anything else...
expand_type_inner(Type) ->
  throw({expand_error, {type_invalid, aero_ast:meta(Type)}}).

%% Where clauses.
%% TODO: implement.
expand_type_where(Where) ->
  throw({expand_error, {type_where_invalid, aero_ast:meta(Where)}}).

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

%% Variable environment.
-record(env, {vars, pat_vars, counter}).

%% Empty expression environment.
new_env() ->
  #env{vars = [], pat_vars = [], counter = counters:new(1, [])}.

%% Get a variable by its name.
lookup_var(Env, {ident, _, IdentName}) ->
  case proplists:get_value(IdentName, Env#env.vars) of
    undefined -> undefined;
    Var       -> Var
  end.

%% Create a fresh variable name from an identifier and save it to the env.
register_var(Env, {ident, _, IdentName}) ->
  counters:add(Env#env.counter, 1, 1),
  Num = counters:get(Env#env.counter, 1),

  Var = {c_var, [], list_to_atom(atom_to_list(IdentName) ++ "_" ++ integer_to_list(Num))},
  {Env#env{vars = [{IdentName, Var} | Env#env.vars]}, Var}.

register_tmp(Env) ->
  {_, Var} = register_var(Env, {ident, [], ''}),
  Var.

%% Create a fresh pattern variable.
register_pat_var(Env, {ident, Meta, IdentName} = Ident) ->
  case proplists:is_defined(IdentName, Env#env.pat_vars) of
    true ->
      throw({expand_error, {pat_var_duplicate, Meta}});
    false ->
      {VarEnv, {c_var, VarMeta, VarName} = Var} = register_var(Env, Ident),
      PatVar = {c_pat_var, VarMeta, VarName},
      {VarEnv#env{pat_vars = [{IdentName, Var} | VarEnv#env.pat_vars]}, PatVar}
  end.

register_pat_wildcard(Env) ->
  {_, PatVar} = register_pat_var(Env, {ident, [], ''}),
  PatVar.

%% Remove pattern variables for once patterns are done parsing.
clear_pat_vars(Env) ->
  Env#env{pat_vars = []}.

%% Type without any bounds.
inferred_type() ->
  {c_type_param, '_'}.

erl_call(Mod, Func, Args) ->
  Callee = {c_path, [erl_path], [{c_var, [], Mod}, {c_var, [], Func}]},
  {c_call, [], Callee, Args}.

lift({c_block, Meta, Exprs}, Env) ->
  {c_block, Meta, [lift(Expr, Env) || Expr <- Exprs]};
lift({c_tuple, Meta, Exprs}, Env) ->
  {Lets, NewExprs} =
    lists:foldl(fun(Expr, {AccLets, AccExprs}) ->
      LiftedExpr = lift(Expr, Env),
      case is_simple(LiftedExpr) of
        true ->
          {AccLets, [LiftedExpr | AccExprs]};
        false ->
          Var = register_tmp(Env),
          Let = {c_let, [], Var, inferred_type(), LiftedExpr},

          {[Let | AccLets], [Var | AccExprs]}
      end
    end, {[], []}, Exprs),
  case Lets of
    [] -> {c_tuple, Meta, lists:reverse(NewExprs)};
    _  -> {c_block, Meta, lists:reverse([{c_tuple, Meta, lists:reverse(NewExprs)} | Lets])}
  end;
lift({c_cons, Meta, Head, Tail}, Env) ->
  LiftedHead = lift(Head, Env),
  LiftedTail = lift(Tail, Env),
  case {is_simple(LiftedHead), is_simple(LiftedTail)} of
    % Both simple.
    {true, true} ->
      {c_cons, Meta, LiftedHead, LiftedTail};

    % Only head is simple.
    {true, false} ->
      Var = register_tmp(Env),
      Let = {c_let, [], Var, inferred_type(), LiftedTail},

      {c_block, Meta, [Let, {c_cons, Meta, LiftedHead, Var}]};

    % Only tail is simple.
    {false, true} ->
      Var = register_tmp(Env),
      Let = {c_let, [], Var, inferred_type(), LiftedHead},

      {c_block, Meta, [Let, {c_cons, Meta, Var, LiftedTail}]};

    % Neither are simple.
    {false, false} ->
      HeadVar = register_tmp(Env),
      TailVar = register_tmp(Env),
      HeadLet = {c_let, [], HeadVar, inferred_type(), LiftedHead},
      TailLet = {c_let, [], TailVar, inferred_type(), LiftedTail},

      {c_block, Meta, [HeadLet, TailLet, {c_cons, Meta, HeadVar, TailVar}]}
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
          Var = register_tmp(Env),
          Let = {c_let, [], Var, inferred_type(), LiftedValue},

          {[Let | AccLets], [{LiftedKey, Var} | AccPairs]};

        % Only value is simple.
        {false, true} ->
          Var = register_tmp(Env),
          Let = {c_let, [], Var, inferred_type(), LiftedKey},

          {[Let | AccLets], [{Var, LiftedValue} | AccPairs]};

        % Neither are simple.
        {false, false} ->
          KeyVar = register_tmp(Env),
          ValueVar = register_tmp(Env),
          KeyLet = {c_let, [], KeyVar, inferred_type(), LiftedKey},
          ValueLet = {c_let, [], ValueVar, inferred_type(), LiftedValue},

          {[KeyLet, ValueLet | AccLets], [{KeyVar, ValueVar} | AccPairs]}
      end
    end, {[], []}, Pairs),
  case Lets of
    [] -> {c_dict, Meta, lists:reverse(NewPairs)};
    _  -> {c_block, Meta, lists:reverse([{c_dict, Meta, lists:reverse(NewPairs)} | Lets])}
  end;
lift({c_func, Meta, Args, Result, Where, Body}, Env) ->
  {c_func, Meta, Args, Result, Where, lift(Body, Env)};
lift({Call, Meta, Path, Args}, Env) when Call =:= c_call; Call =:= c_apply ->
  {Lets, NewArgs} =
    lists:foldl(fun(Arg, {AccLets, AccArgs}) ->
      LiftedArg = lift(Arg, Env),
      case is_simple(LiftedArg) of
        true ->
          {AccLets, [LiftedArg | AccArgs]};
        false ->
          Var = register_tmp(Env),
          Let = {c_let, [], Var, inferred_type(), LiftedArg},

          {[Let | AccLets], [Var | AccArgs]}
      end
    end, {[], []}, Args),
  case Lets of
    [] -> {Call, Meta, Path, lists:reverse(NewArgs)};
    _  -> {c_block, Meta, lists:reverse([{Call, Meta, Path, lists:reverse(NewArgs)} | Lets])}
  end;
lift({c_let, Meta, Var, Type, Expr}, Env) ->
  {c_let, Meta, Var, Type, lift(Expr, Env)};
lift({c_letrec, Meta, Var, Type, Func}, Env) ->
  {c_letrec, Meta, Var, Type, lift(Func, Env)};
lift({c_match, Meta, Expr, Cases}, Env) ->
  LiftedExpr = lift(Expr, Env),
  LiftedCases = lists:map(fun({Pat, Body}) -> {Pat, lift(Body, Env)} end, Cases),
  case is_simple(LiftedExpr) of
    true ->
      {c_match, Meta, LiftedExpr, LiftedCases};
    false ->
      Var = register_tmp(Env),
      Let = {c_let, [], Var, inferred_type(), LiftedExpr},

      {c_block, Meta, [Let, {c_match, Meta, Var, LiftedCases}]}
  end;
lift(Expr, _Env) ->
  Expr.

%% Expressions without any nested expressions to evaluate inside.
is_simple({c_tuple, _, Exprs}) ->
  lists:all(fun is_literal/1, Exprs);
is_simple({c_cons, _, Head, Tail}) ->
  is_simple(Head) andalso is_literal(Tail);
is_simple({c_dict, _, Pairs}) ->
  lists:all(fun(K, V) -> is_literal(K) andalso is_literal(V) end, Pairs);
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
