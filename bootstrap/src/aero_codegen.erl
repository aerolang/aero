%%% Generates Core Erlang code from the Aero AST.

-module(aero_codegen).

-export([generate/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-spec generate(aero_expander:c_module()) -> {ok, binary()} | {error, term()}.
generate(CoreModule) ->
  CerlMod = gen_module(CoreModule),
  case compile:noenv_forms(CerlMod, [from_core, return_errors]) of
    {ok, _, Binary} ->
      Name = element(3, CoreModule),
      {ok, Name, Binary};
    {error, Errors, _} ->
      {error, {internal_cerl_compile, Errors}}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

gen_module({c_module, _, Name, Exports, Attrs, Defs}) ->
  CerlName = cerl:c_atom(Name),

  CerlExports = lists:map(fun(Export) ->
    Func = proplists:get_value(Export, Defs),
    cerl:c_var({Export, arity(Func)})
  end, Exports),

  CerlAttrs = lists:map(fun(_Attr) ->
    % TODO: Implement.
    throw(unimplemented)
  end, Attrs),

  CerlDefs = lists:map(fun(Def) ->
    {FuncName, Func} = Def,
    CerlVar = cerl:c_var({FuncName, arity(Func)}),
    CerlFunc = gen_func(Func),
    {CerlVar, CerlFunc}
  end, Defs),

  cerl:ann_c_module([], CerlName, CerlExports, CerlAttrs, CerlDefs).

gen_func({c_func, _, Args, _Ret, _Where, Body}) ->
  CerlArgs = [cerl:c_var(Name) || {Name, _} <- Args],
  CerlBody = gen_expr(Body),
  cerl:ann_c_fun([], CerlArgs, CerlBody).

gen_expr({c_integer_lit, _, Integer}) ->
  cerl:ann_c_int([], Integer);
gen_expr({c_float_lit, _, Float}) ->
  cerl:ann_c_float([], Float);
gen_expr({c_atom_lit, _, Atom}) ->
  cerl:ann_c_atom([], Atom);
gen_expr({c_string_lit, _, _String}) ->
  % TODO: Implement.
  throw(unimplemented).

arity({c_func, _, Args, _, _, _}) ->
  length(Args).
