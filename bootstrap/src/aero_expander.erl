%%% Converts the Aero AST down to Core Aero.
%%%
%%% This format is made to resemble Core Erlang, with variables renamed,
%%% macros expanded, and with type information checked, giving a simple core
%%% language.
%%%
%%% For now we don't really have macros, so they're just manually expanded here.

-module(aero_expander).

-export([expand/2]).

-export_type([c_any/0, c_module/0, c_vis/0, c_expr/0, c_block/0]).
-export_type([c_literal/0, c_bool_lit/0, c_int_lit/0, c_float_lit/0, c_atom_lit/0, c_str_lit/0]).
-export_type([c_func/0, c_var/0, c_type/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Any Core Aero syntax element.
-type c_any() :: c_module()
               | c_expr()
               | c_type().

%% Top-level of Core Aero, represents a whole module.
-type c_module() :: {c_module, meta(), c_var(), c_module_attrs(), c_module_defs()}.

-type c_module_attrs() :: [{c_atom_lit(), c_literal()}].
-type c_module_defs()  :: [{c_var(), c_vis(), c_func()}].

% Definition visibility.
-type c_vis() :: c_vis_pub
               | c_vis_priv.

%% Any expression, excludes non-expression parts of Core Aero.
-type c_expr() :: c_block()
                | c_literal()
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
                | c_letrec().

%% A group of expressions with the last giving the value of the group.
-type c_block() :: {c_block, meta(), [c_expr()]}.

%% A constant literal value at compile time.
-type c_literal() :: c_bool_lit()
                   | c_int_lit()
                   | c_float_lit()
                   | c_atom_lit()
                   | c_str_lit().

%% Literals.
-type c_bool_lit()  :: {c_bool_lit, meta(), atom()}.
-type c_int_lit()   :: {c_int_lit, meta(), integer()}.
-type c_float_lit() :: {c_float_lit, meta(), float()}.
-type c_atom_lit()  :: {c_atom_lit, meta(), atom()}.
-type c_str_lit()   :: {c_str_lit, meta(), binary()}.

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
-type c_apply() :: {c_call, meta(), c_var(), c_apply_args()}.

-type c_apply_args() :: [c_expr()].

%% Variables.
-type c_var() :: {c_var, meta(), atom()}.

%% Paths.
-type c_path() :: {c_path, [], [c_var()]}.

%% Let and letrec expressions.
-type c_let()    :: {c_let, meta(), c_var(), c_type(), c_expr()}.
-type c_letrec() :: {c_letrec, meta(), c_var(), c_type(), c_func()}.

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

-spec expand(aero_parser:ast(), aero_context:context()) -> {ok, [c_module()]} | {error, term()}.
expand(Source, Context) ->
  Filename = aero_context:filename(Context),
  ModuleName = binary_to_atom(filename:basename(Filename, ".aero"), utf8),
  try expand_source(Source, ModuleName) of
    Modules -> {ok, Modules}
  catch
    throw:{expand_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Definition Expanding
%% -----------------------------------------------------------------------------

expand_source({source, Meta, SourceArgs}, ModuleName) ->
  expand_mod_def([{ident, [], ModuleName}, {block, Meta, SourceArgs}], []);
expand_source(_, _) ->
  throw({expand_error, no_source}).

expand_mod_def([{ident, _IdentMeta, Ident}, {block, _BlockMeta, BlockArgs}], _ModuleMeta) ->
  Meta = [],
  Attrs = [],
  {Defs, InnerModules} =
    lists:foldl(fun(BlockArg, {Defs, InnerModules}) ->
      case BlockArg of
        % Public functions.
        {expand, FuncMeta, {ident, _, pub}, [{expand, _, {ident, _, func}, FuncArgs}]} ->
          Def = expand_func_def(FuncArgs, FuncMeta, c_vis_pub),
          {[Def | Defs], InnerModules};

        % Private functions.
        {expand, FuncMeta, {ident, _, func}, FuncArgs} ->
          Def = expand_func_def(FuncArgs, FuncMeta, c_vis_priv),
          {[Def | Defs], InnerModules}
      end
    end, {[], []}, BlockArgs),

  Module = {c_module, Meta, {c_path, [], [{c_var, [], Ident}]}, Attrs, lists:reverse(Defs)},
  [Module | lists:reverse(InnerModules)];
expand_mod_def(_, ModuleMeta) ->
  throw({expand_error, {mod_def_invalid, ModuleMeta}}).

expand_func_def([FuncHead, FuncBody], _FuncMeta, Vis) ->
  Meta = [],
  {Name, Args, Result, Where, Env} = expand_func_def_head(FuncHead, new_env()),
  Body = expand_func_def_body(FuncBody, Env),

  Func = {c_func, Meta, Args, Result, Where, Body},
  {Name, Vis, Func};
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
              throw({expand_error, {func_def_arg_invalid, get_meta(Arg)}})
          end
        end, {[], Env}, Args),
      ResultType = expand_type_inner(Result),

      {Path, lists:reverse(CoreArgs), ResultType, Where, BodyEnv};
    _ ->
      throw({expand_error, {func_def_head_invalid, FuncHeadMeta}})
  end;
expand_func_def_head(FuncHead, _, _Env) ->
  throw({expand_error, {func_def_head_invalid, get_meta(FuncHead)}}).

expand_func_def_body({block, _, _} = Block, Env) ->
  expand_expr(Block, Env);
expand_func_def_body(FuncBody, _Env) ->
  throw({expand_error, {func_def_body_invalid, get_meta(FuncBody)}}).

%% -----------------------------------------------------------------------------
%% Expression Expanding
%% -----------------------------------------------------------------------------

%% Blocks.
expand_expr({block, _, []}, _Env) ->
  {c_unit, []};
expand_expr({block, _, BlockExprs}, Env) ->
  {Exprs, _} =
    lists:foldl(fun(BlockExpr, {ExprAcc, EnvAcc}) ->
      case BlockExpr of
        {expand, _, {op, _, '_=_'}, [Ident, RightExpr]} ->
          {NewEnv, Var} = register_var(EnvAcc, Ident),
          LetExpr = {c_let, [], Var, {c_type_param, '_'}, expand_expr(RightExpr, Env)},

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
            {c_let, [], Var, {c_type_param, '_'}, Expr}
        end
      end, tl(Exprs))) ++ [hd(Exprs)]}
  end;

%% Literals.
expand_expr({ident, _, Bool}, _Env) when Bool =:= true; Bool =:= false ->
  {c_bool_lit, [], Bool};
expand_expr({integer_lit, _, Integer}, _Env) ->
  {c_int_lit, [], Integer};
expand_expr({float_lit, _, Float}, _Env) ->
  {c_float_lit, [], Float};
expand_expr({atom_lit, _, Atom}, _Env) ->
  {c_atom_lit, [], Atom};
expand_expr({string_lit, _, String}, _Env) ->
  {c_str_lit, [], String};

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
          {{c_atom_lit, [], Key}, expand_expr(Value, Env)}
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
          ArgType = {c_type_param, '_'},

          {[{ArgVar, ArgType} | ArgAcc], NewEnv};
        _ ->
          throw({expand_error, {func_arg_invalid, get_meta(Arg)}})
      end
    end, {[], HeadEnv}, FuncArgs),
  CoreBody = expand_expr(Body, BodyEnv),
  Func = {c_func, [], lists:reverse(CoreArgs), {c_type_param, '_'}, [], CoreBody},

  % If recursive then wrap it into a block with a letrec.
  case Var of
    none -> Func;
    _    -> {c_block, [], [{c_letrec, [], Var, {c_type_param, '_'}, Func}, Var]}
  end;

%% Function calls.
expand_expr({expand, Meta, {op, _, '_(_)'}, [Callee, {args, _, Args}]}, Env) ->
  case Callee of
    % Constructors.
    {expand, _, {op, _, '#_'}, [Path]} ->
      case Path of
        {ident, _, ref} when length(Args) =:= 0 ->
          % TODO: call into a type-checkable Aero function instead.
          {c_call, [], {c_path, [], [{c_var, [], erlang}, {c_var, [], make_ref}]}, []};
        {ident, _, list} ->
          lists:foldr(fun(Arg, Acc) ->
            {c_cons, [], expand_expr(Arg, Env), Acc}
          end, {c_nil, []}, Args);
        {ident, _, mbox} when length(Args) =:= 0 ->
          % TODO: call into a type-checkable Aero function instead.
          {c_call, [], {c_path, [], [{c_var, [], erlang}, {c_var, [], make_ref}]}, []};
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

%% Logs.
expand_expr({expand, _, {ident, _, log}, [Message]}, Env) ->
  % TODO: call into a type-checkable Aero function instead.
  Callee = {c_path, [], [{c_var, [], io}, {c_var, [], put_chars}]},
  Args = [
    {c_atom_lit, [], standard_io},
    {c_cons, [], expand_expr(Message, Env), {c_cons, [], {c_int_lit, [], $\n}, {c_nil, []}}}
  ],

  {c_call, [], Callee, Args};

%% Anything else...
expand_expr(Expr, _Env) ->
  throw({expand_error, {expr_invalid, get_meta(Expr)}}).

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
expand_type_inner({expand, _, {op, _, '_->_'}, [{args, _, Args}, Result]}) ->
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
  throw({expand_error, {type_invalid, get_meta(Type)}}).

%% Where clauses.
%% TODO: implement.
expand_type_where(Where) ->
  throw({expand_error, {type_where_invalid, get_meta(Where)}}).

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

%% Variable environment.
-record(env, {vars, counter}).

%% Empty expression environment.
new_env() ->
  #env{vars = [], counter = counters:new(1, [])}.

%% Get a variable by its name.
lookup_var(Env, {ident, _, IdentName}) ->
  case proplists:get_value(IdentName, Env#env.vars) of
    undefined -> undefined;
    VarName   -> {c_var, [], VarName}
  end.

%% Create a fresh variable name from an identifier and save it to the env.
register_var(Env, {ident, _, IdentName}) ->
  counters:add(Env#env.counter, 1, 1),
  Num = counters:get(Env#env.counter, 1),

  VarName = list_to_atom(atom_to_list(IdentName) ++ "_" ++ integer_to_list(Num)),
  {Env#env{vars = [{IdentName, VarName} | Env#env.vars]}, {c_var, [], VarName}}.

get_meta({source, Meta, _})          -> Meta;
get_meta({integer_lit, Meta, _})     -> Meta;
get_meta({float_lit, Meta, _})       -> Meta;
get_meta({atom_lit, Meta, _})        -> Meta;
get_meta({string_lit, Meta, _})      -> Meta;
get_meta({ident, Meta, _})           -> Meta;
get_meta({type_param, Meta, _})      -> Meta;
get_meta({blank, Meta})              -> Meta;
get_meta({op, Meta, _})              -> Meta;
get_meta({block, Meta, _})           -> Meta;
get_meta({expand, Meta, _, _})       -> Meta;
get_meta({args, Meta, _})            -> Meta;
get_meta({tag, Meta, _, _})          -> Meta;
get_meta({attribute, Meta, _, _})    -> Meta;
get_meta({inner_attribute, Meta, _}) -> Meta.
