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
      {c_module, _, {c_var, _, Name}, _, _} = CoreModule,
      {ok, Name, Binary};
    {error, Errors, _} ->
      {error, {internal_cerl_compile, Errors}}
  end.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

gen_module({c_module, _, {c_var, [], Name}, Attrs, Defs}) ->
  CerlName = cerl:c_atom(Name),

  CerlAttrs =
    lists:map(fun(_Attr) ->
      % TODO: Implement.
      throw(unimplemented)
    end, Attrs),

  {CerlExports, CerlDefs} =
    lists:foldl(fun(Def, {AccExports, AccDefs}) ->
      case Def of
        {{c_var, _, FuncName}, Vis, {c_func, _, _, _, _, _} = Func} ->
          CerlDef = {cerl:c_var({FuncName, arity(Func)}), gen_func(Func)},
          case Vis of
            c_vis_pub ->
              CerlExport = cerl:c_var({FuncName, arity(Func)}),
              {[CerlExport | AccExports], [CerlDef | AccDefs]};
            c_vis_priv ->
              {AccExports, [CerlDef | AccDefs]}
          end
      end
    end, {[], []}, Defs),

  cerl:ann_c_module([], CerlName, lists:reverse(CerlExports), CerlAttrs, lists:reverse(CerlDefs)).

gen_func({c_func, _, Args, _Ret, _Where, Body}) ->
  CerlArgs = [cerl:c_var(Name) || {Name, _} <- Args],
  CerlBody = gen_expr(Body),

  cerl:ann_c_fun([], CerlArgs, CerlBody).

gen_expr({c_bool_lit, _, Bool}) ->
  cerl:add_ann([], cerl:abstract(Bool));
gen_expr({c_int_lit, _, Integer}) ->
  cerl:add_ann([], cerl:abstract(Integer));
gen_expr({c_float_lit, _, Float}) ->
  cerl:add_ann([], cerl:abstract(Float));
gen_expr({c_atom_lit, _, Atom}) ->
  cerl:add_ann([], cerl:abstract(Atom));
gen_expr({c_str_lit, _, String}) ->
  cerl:add_ann([], cerl:abstract(String));

gen_expr({c_cons, _, Head, Tail}) ->
  cerl:ann_c_cons([], gen_expr(Head), gen_expr(Tail));
gen_expr({c_nil, _}) ->
  cerl:ann_c_nil([]);

gen_expr({c_unit, _}) ->
  cerl:add_ann([], cerl:abstract(ok));

gen_expr({c_call, _, {c_callee_remote, {c_var, _, Module}, {c_var, _, Function}}, Args}) ->
  CerlModule = cerl:add_ann([], cerl:abstract(Module)),
  CerlFunction = cerl:add_ann([], cerl:abstract(Function)),
  CerlArgs = [gen_expr(Arg) || Arg <- Args],

  cerl:ann_c_call([], CerlModule, CerlFunction, CerlArgs).

arity({c_func, _, Args, _, _, _}) ->
  length(Args).
