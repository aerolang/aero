%%% Handles expansion for patterns.

-module(aero_expand_pat).

-export([expand_pat/2]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Expand a pattern.
-spec expand_pat(aero_ast:t(), aero_env:t()) -> {aero_core:c_pat(), aero_env:t()}.
expand_pat(Ast, Env) ->
  % Patterns store the variables bound during this pattern to prevent them from
  % being repeating, but all the pattern vars are cleared.
  {Pat, PatEnv} = expand_pat_outer(Ast, Env),
  {Pat, aero_env:clear_pat_vars(PatEnv)}.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

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
  case aero_env:lookup_pat_var(Env, Ident) of
    undefined ->
      {NewEnv, PatVar} = aero_env:register_pat_var(Env, Ident),
      {PatVar, NewEnv};
    PatVar ->
      throw({expand_error, {pat_var_exists, aero_ast:meta(Ident), PatVar}})
  end;

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
