%%% Generates Core Erlang code from the Aero AST.

-module(aero_codegen).

-export([generate/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec generate(aero_expander:c_module()) -> {ok, binary()} | {error, term()}.
generate(CoreModule) ->
  CerlModule = gen_module(CoreModule),
  case compile:noenv_forms(CerlModule, [from_core, return_errors]) of
    {ok, _, Binary} ->
      {c_module, _, {c_path, _, [{c_var, _, Name}]}, _, _} = CoreModule,
      {ok, Name, Binary};
    {error, Errors, _} ->
      {error, {internal_cerl_compile, Errors}}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

%% Definitions.

gen_module({c_module, _, {c_path, _, [{c_var, _, Name}]}, Attrs, Defs}) ->
  CerlName = cerl:c_atom(Name),

  CerlAttrs =
    lists:map(fun(_Attr) ->
      % TODO: Implement.
      throw(unimplemented)
    end, Attrs),

  {CerlExports, CerlDefs} =
    lists:foldl(fun(Def, {AccExports, AccDefs}) ->
      case gen_def(Def) of
        {no_export, CerlDef}          -> {AccExports, [CerlDef | AccDefs]};
        {export, CerlExport, CerlDef} -> {[CerlExport | AccExports], [CerlDef | AccDefs]}
      end
    end, {[], []}, Defs),

  cerl:c_module(CerlName, lists:reverse(CerlExports), CerlAttrs, lists:reverse(CerlDefs)).

gen_def({c_def_func, _, {c_path, _, [{c_var, _, FuncName}]}, Vis, Func}) ->
  FName = cerl:c_fname(FuncName, arity(Func)),
  Def = {FName, gen_expr(Func)},
  case Vis of
    c_vis_pub  -> {export, FName, Def};
    c_vis_priv -> {no_export, Def}
  end;

gen_def({c_def_const, _, {c_path, _, [{c_var, _, FuncName}]}, Vis, _Type, Expr}) ->
  FName = cerl:c_fname(FuncName, 0),
  Def = {FName, cerl:c_fun([], gen_expr(Expr))},
  case Vis of
    c_vis_pub  -> {export, FName, Def};
    c_vis_priv -> {no_export, Def}
  end.

%% Expressions.

gen_expr({c_block, _, [{c_letrec, _, {c_var, _, FuncName} = Var, _Type, Func}, Var]}) ->
  % Recursive functions are handled specially: the name of the recursive
  % function needs to also be bound inside a recursive function in Core
  % Erlang, e.g. binding `Name` to `'Name/1'`.
  CerlFuncName = cerl:c_fname(FuncName, arity(Func)),
  CerlFunc = gen_expr(Func),
  CerlFuncNew = cerl:c_fun(
    cerl:fun_vars(CerlFunc),
    cerl:c_let([cerl:c_var(FuncName)], CerlFunc, cerl:fun_body(CerlFunc))
  ),
  cerl:c_letrec([{CerlFuncName, CerlFuncNew}], CerlFuncName);
gen_expr({c_block, _, Exprs}) ->
  RevExprs = lists:reverse(Exprs),

  % Erlang "let"s go backwards, so we expand the inner parts first.
  lists:foldr(fun({c_let, _, Left, _Type, Right}, Inner) ->
    cerl:c_let([gen_expr(Left)], gen_expr(Right), Inner)
  end, gen_expr(hd(RevExprs)), tl(RevExprs));

gen_expr({c_bool_lit, _, Bool}) ->
  cerl:abstract(Bool);
gen_expr({c_int_lit, _, Integer}) ->
  cerl:abstract(Integer);
gen_expr({c_float_lit, _, Float}) ->
  cerl:abstract(Float);
gen_expr({c_atom_lit, _, Atom}) ->
  cerl:abstract(Atom);
gen_expr({c_str_lit, _, String}) ->
  cerl:abstract(String);

gen_expr({c_unit, _}) ->
  cerl:abstract(ok);

gen_expr({c_tuple, _, Exprs}) ->
  cerl:c_tuple([gen_expr(Expr) || Expr <- Exprs]);

gen_expr({c_cons, _, Head, Tail}) ->
  cerl:c_cons(gen_expr(Head), gen_expr(Tail));
gen_expr({c_nil, _}) ->
  cerl:c_nil();

gen_expr({c_dict, _, Pairs}) ->
  CerlPairs =
    lists:map(fun({Key, Value}) ->
      cerl:c_map_pair(gen_expr(Key), gen_expr(Value))
    end, Pairs),
  cerl:c_map(CerlPairs);

gen_expr({c_func, _, Args, _Result, _Where, Body}) ->
  CerlArgs = [cerl:c_var(Name) || {{c_var, _, Name}, _} <- Args],
  CerlBody = gen_expr(Body),

  cerl:c_fun(CerlArgs, CerlBody);

gen_expr({c_call, _, {c_path, _, [{c_var, _, Module}, {c_var, _, Function}]}, Args}) ->
  CerlModule = cerl:abstract(Module),
  CerlFunction = cerl:abstract(Function),
  CerlArgs = [gen_expr(Arg) || Arg <- Args],

  cerl:c_call(CerlModule, CerlFunction, CerlArgs);
gen_expr({c_call, _, {c_path, _, [{c_var, _, Function}]}, Args}) ->
  CerlFunction = cerl:c_fname(Function, length(Args)),
  CerlArgs = [gen_expr(Arg) || Arg <- Args],

  cerl:c_apply(CerlFunction, CerlArgs);
gen_expr({c_apply, _, {c_var, _, Function}, Args}) ->
  CerlFunction = cerl:c_var(Function),
  CerlArgs = [gen_expr(Arg) || Arg <- Args],

  cerl:c_apply(CerlFunction, CerlArgs);

gen_expr({c_var, _, VarName}) ->
  cerl:c_var(VarName);

% Occurs when a let is at the end of a block, binding is not used.
gen_expr({c_let, _, _Left, _Type, Right}) ->
  gen_expr(Right);

gen_expr({c_match, _, Expr, Cases}) ->
  CerlClauses =
    lists:map(fun
      ({{c_pat_args, _, Pats}, Body}) ->
        cerl:c_clause([gen_pat(Pat) || Pat <- Pats], gen_expr(Body));
      ({Pat, Body}) ->
        cerl:c_clause([gen_pat(Pat)], gen_expr(Body))
    end, Cases),
  case Expr of
    {c_args, _, Args} ->
      cerl:c_case(cerl:c_values([gen_expr(Arg) || Arg <- Args]), CerlClauses);
    _ ->
      cerl:c_case(gen_expr(Expr), CerlClauses)
  end.

%% Patterns.

gen_pat({c_pat_bool, _, Bool}) ->
  cerl:abstract(Bool);
gen_pat({c_pat_int, _, Integer}) ->
  cerl:abstract(Integer);
gen_pat({c_pat_float, _, Float}) ->
  cerl:abstract(Float);
gen_pat({c_pat_atom, _, Atom}) ->
  cerl:abstract(Atom);
gen_pat({c_pat_str, _, String}) ->
  cerl:abstract(String);

gen_pat({c_pat_unit, _}) ->
  cerl:abstract(ok);

gen_pat({c_pat_tuple, _, Exprs}) ->
  cerl:c_tuple([gen_pat(Expr) || Expr <- Exprs]);

gen_pat({c_pat_cons, _, Head, Tail}) ->
  cerl:c_cons(gen_pat(Head), gen_pat(Tail));
gen_pat({c_pat_nil, _}) ->
  cerl:c_nil();

gen_pat({c_pat_dict, _, Pairs}) ->
  CerlPairs =
    lists:map(fun({Key, Value}) ->
      cerl:c_map_pair(gen_pat(Key), gen_pat(Value))
    end, Pairs),
  cerl:c_map(CerlPairs);

gen_pat({c_pat_var, _, VarName}) ->
  cerl:c_var(VarName).

%% Utilities.

arity({c_func, _, Args, _, _, _}) ->
  length(Args).
