%%% Aero to Elixir AST conversions.

-module(aero_transform).

-export([transform/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Convert the Aero AST into the Elixir AST.
transform({source, _Meta, Exprs}) ->
  % Source of an Aero file calls the `mod` macro. A require call to the Aero
  % kernel implemented in Elixir is inserted at the beginning of the source and
  % the Elixir kernel (besides the special forms) are unimported.
  NoExKernel = {
    import,
    [{context, 'Elixir'}],
    [{'__aliases__', [{alias, false}], ['Kernel']}, [{only, []}]]
  },
  KernelReq = {
    require,
    [{context, 'Elixir'}],
    [{'__aliases__', [{alias, false}], ['Aero', 'Kernel']}]
  },
  Module = ex_macro_call(mod, ['__source__', ex_block(lists:map(fun transform/1, Exprs))]),
  ex_block([NoExKernel, KernelReq, Module]);
transform({integer_lit, _Meta, Integer}) ->
  Integer;
transform({float_lit, _Meta, Float}) ->
  Float;
transform({atom_lit, _Meta, Atom}) ->
  Atom;
transform({string_lit, _Meta, String}) ->
  String;
transform({ident, _Meta, Ident}) ->
  Safe = safe_ident(Ident),
  case zero_arg_macro(Safe) of
    true ->
      % Zero arg macros names are converted to macro calls of no arguments.
      ex_macro_call(Safe, []);
    false ->
      {safe_ident(Ident), [], 'Elixir'}
  end;
transform({op, _Meta, Op}) ->
  safe_ident(Op);
transform({block, _Meta, Exprs}) ->
  ex_block(lists:map(fun transform/1, Exprs));
transform({expand, _Meta, Macro, Exprs}) ->
  ex_macro_call(transform(Macro), lists:map(fun transform/1, Exprs));
transform({args, _Meta, Exprs}) ->
  ex_macro_call('__args__', lists:map(fun transform/1, Exprs));
transform({tag, _Meta, Left, Right}) ->
  ex_macro_call('__tag__', [transform(Left), transform(Right)]);
transform({attribute, _Meta, Left, Right}) ->
  ex_macro_call('__attr__', [transform(Left), transform(Right)]);
transform({inner_attribute, _Meta, Expr}) ->
  ex_macro_call('__inner_attr__', [transform(Expr)]).

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

ex_block(Exprs) ->
  {'__block__', [], Exprs}.

ex_macro_call(Macro, Args) ->
  Callee = {
    '.',
    [],
    [{'__aliases__', [{alias, false}], ['Aero', 'Kernel']}, Macro]
  },
  {Callee, [], Args}.

%% Convert idents that are keywords in Elixir.
safe_ident(true) -> true_;
safe_ident(false) -> false_;
safe_ident(nil) -> nil_;
safe_ident('when') -> when_;
safe_ident('and') -> and_;
safe_ident('or') -> or_;
safe_ident(in) -> in_;
safe_ident(fn) -> fn_;
safe_ident(do) -> do_;
safe_ident('end') -> end_;
safe_ident('catch') -> catch_;
safe_ident(rescue) -> rescue_;
safe_ident('after') -> after_;
safe_ident(else) -> else_;
safe_ident('_') -> '__';
safe_ident(Other) -> Other.

%% Identifiers which are converted to macro calls.
zero_arg_macro(true_) -> true;
zero_arg_macro(false_) -> true;
zero_arg_macro(nil_) -> true;
zero_arg_macro(_) -> false.
