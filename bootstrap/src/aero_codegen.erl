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
  cerl:add_ann([], cerl:abstract(Integer));
gen_expr({c_float_lit, _, Float}) ->
  cerl:add_ann([], cerl:abstract(Float));
gen_expr({c_atom_lit, _, Atom}) ->
  cerl:add_ann([], cerl:abstract(Atom));
gen_expr({c_string_lit, _, String}) ->
  cerl:add_ann([], cerl:abstract(String));

gen_expr({c_cons, _, Head, Tail}) ->
  cerl:ann_c_cons([], gen_expr(Head), gen_expr(Tail));
gen_expr({c_nil, _}) ->
  cerl:ann_c_nil([]);

gen_expr({c_unit, _}) ->
  cerl:add_ann([], cerl:abstract(ok));

gen_expr({c_call, _, Module, Function, Args}) ->
  CerlModule = gen_expr(Module),
  CerlFunction = gen_expr(Function),
  CerlArgs = [gen_expr(Arg) || Arg <- Args],

  cerl:ann_c_call([], CerlModule, CerlFunction, CerlArgs).

arity({c_func, _, Args, _, _, _}) ->
  length(Args).
