%%% Handles expansion for types.

-module(aero_expand_type).

-export([expand_type/2, expand_types/2, expand_where_clause/2, expand_where_clauses/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Expand a type.
-spec expand_type(aero_ast:t(), aero_env:t()) -> {aero_core:c_type(), aero_env:t()}.

%% Builtins.
expand_type({ident, _, bool}, Env) ->
  {aero_core:c_type_bool([]), Env};
expand_type({ident, _, int}, Env) ->
  {aero_core:c_type_int([]), Env};
expand_type({ident, _, float}, Env) ->
  {aero_core:c_type_float([]), Env};
expand_type({ident, _, atom}, Env) ->
  {aero_core:c_type_sym([]), Env};
expand_type({ident, _, str}, Env) ->
  {aero_core:c_type_str([]), Env};
expand_type({ident, _, bytes}, Env) ->
  {aero_core:c_type_bytes([]), Env};
expand_type({ident, _, bits}, Env) ->
  {aero_core:c_type_bits([]), Env};
expand_type({ident, _, ref}, Env) ->
  {aero_core:c_type_ref([]), Env};

%% Void type.
expand_type({expand, _, {op, _, '(_)'}, [{args, _, []}]}, Env) ->
  {aero_core:c_type_void([]), Env};

%% Collections.
expand_type({expand, _, {op, _, '(_)'}, [{args, _, Args}]}, Env) when length(Args) > 1 ->
  {Types, TypeEnv} = expand_types(Args, Env),

  {aero_core:c_type_tuple([], Types), TypeEnv};
expand_type({expand, _, {ident, _, list}, [T]}, Env) ->
  {Type, TypeEnv} = expand_type(T, Env),

  {aero_core:c_type_list([], Type), TypeEnv};
expand_type({expand, _, {ident, _, dict}, [K, V]}, Env) ->
  {[KeyType, KeyValue], TypeEnv} = expand_types([K, V], Env),

  {aero_core:c_type_dict([], KeyType, KeyValue), TypeEnv};

%% Functions.
expand_type({expand, _, {op, _, Arrow}, [{args, _, Args}, Result]}, Env) when Arrow =:= '_->_';
                                                                              Arrow =:= '_->>_' ->
  {ArgTypes, ResultEnv} = expand_types(Args, Env),
  {ResultType, TypeEnv} = expand_type(Result, ResultEnv),

  {aero_core:c_type_func([], ArgTypes, ResultType), TypeEnv};

%% Top and bottom types.
expand_type({ident, _, any}, Env) ->
  {aero_core:c_type_any([]), Env};
expand_type({ident, _, never}, Env) ->
  {aero_core:c_type_never([]), Env};

%% Concurrent primitives.
expand_type({ident, _, wld}, Env) ->
  {aero_core:c_type_wld([]), Env};
expand_type({expand, _, {ident, _, mbox}, [T]}, Env) ->
  {Type, TypeEnv} = expand_type(T, Env),

  {aero_core:c_type_mbox([], Type), TypeEnv};
expand_type({expand, _, {ident, _, addr}, [T]}, Env) ->
  {Type, TypeEnv} = expand_type(T, Env),

  {aero_core:c_type_addr([], Type), TypeEnv};

%% Type parameters.
expand_type({type_param, _, _} = TypeParam, Env) ->
  {TypeEnv, Type} = aero_env:register_type_var(Env, TypeParam),
  {Type, TypeEnv};

%% Option and Result type macros.
expand_type({expand, _, {op, _, '_?'}, [T]}, Env) ->
  {Type, TypeEnv} = expand_type(T, Env),
  SomeInner =
    case Type of
      {c_type_tuple, _, InnerTypes} -> InnerTypes;
      InnerType                     -> [InnerType]
    end,
  Some = aero_core:c_type_tuple([], [aero_core:c_type_tag([], some) | SomeInner]),
  None = aero_core:c_type_tag([], none),

  {aero_core:c_type_union([], [Some, None]), TypeEnv};
expand_type({expand, _, {op, _, '_!'}, [T]}, Env) ->
  {Type, TypeEnv} = expand_type(T, Env),
  OkInner =
    case Type of
      {c_type_tuple, _, InnerTypes} -> InnerTypes;
      InnerType                     -> [InnerType]
    end,
  Ok = aero_core:c_type_tuple([], [aero_core:c_type_tag([], ok) | OkInner]),
  ErrorInner = aero_core:c_type_proto([],
    aero_core:c_type_path([], [
      aero_core:c_type_var([], aero_std),
      aero_core:c_type_var([], 'Error')
    ]),
    []
  ),
  Error = aero_core:c_type_tuple([], [aero_core:c_type_tag([], error), ErrorInner]),

  {aero_core:c_type_union([], [Ok, Error]), TypeEnv};

%% Anything else...
expand_type(Type, _) ->
  throw({expand_error, {type_invalid, aero_ast:meta(Type)}}).

%% Expand a list of types.
-spec expand_types([aero_ast:t()], aero_env:t()) -> {[aero_core:c_type()], aero_env:t()}.
expand_types(Args, Env) ->
  {Types, TypeEnv} =
    lists:foldl(fun(Arg, {TypeAcc, EnvAcc}) ->
      {Type, NewEnv} = expand_type(Arg, EnvAcc),
      {[Type | TypeAcc], NewEnv}
    end, {[], Env}, Args),

  {lists:reverse(Types), TypeEnv}.

%% Expand a where clause.
-spec expand_where_clause(aero_ast:t(), aero_env:t()) ->
        {{aero_core:c_type(), aero_core:c_type()}, aero_env:t()}.
expand_where_clause(Where, _Env) ->
  %% TODO: implement.
  throw({expand_error, {type_where_invalid, aero_ast:meta(Where)}}).

%% Expand a list of where clauses.
-dialyzer({no_return, expand_where_clauses/2}).
-spec expand_where_clauses([aero_ast:t()], aero_env:t()) ->
        {[{aero_core:c_type(), aero_core:c_type()}], aero_env:t()}.
expand_where_clauses(Wheres, Env) ->
  {Wheres, WhereEnv} =
    lists:foldl(fun(Arg, {WhereAcc, EnvAcc}) ->
      {Where, NewEnv} = expand_where_clause(Arg, EnvAcc),
      {[Where | WhereAcc], NewEnv}
    end, {[], Env}, Wheres),

  {lists:reverse(Wheres), WhereEnv}.
