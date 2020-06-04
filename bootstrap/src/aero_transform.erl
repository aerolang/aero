%%% Aero to Elixir AST conversions.

-module(aero_transform).

-export([transform/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Convert the Aero AST into the Elixir AST.
transform({source, _Meta, Exprs}) ->
  % Source of an Aero file calls Elixir's defmodule.
  Block = {'__block__', [], lists:map(fun transform/1, Exprs)},
  ExMeta = [{context, 'Elixir'}, {import, 'Elixir.Kernel'}],
  {defmodule, ExMeta, ['_source', [{do, Block}]]};
transform({expand, _Meta, Macro, Args}) ->
  MacroName =
    case Macro of
      {ident, _, _} = Ident -> transform(Ident);
      {op, _, OpName} -> atom_to_binary(OpName, utf8)
    end,
  Callee = {'.', [], ['_aero_kernel', MacroName]},
  {Callee, [], lists:map(fun transform/1, Args)};
transform({ident, _Meta, Ident}) ->
  case Ident of
    true -> true;
    false -> false;
    nil -> [];
    Other -> {safe_ident(Other), [], 'Elixir'}
  end;
transform({atom_lit, _Meta, Atom}) ->
  Atom;
transform({string_lit, _Meta, String}) ->
  String;
transform({block, _Meta, Exprs}) ->
  {'__block__', [], lists:map(fun transform/1, Exprs)}.

%% -----------------------------------------------------------------------------
%% Helper Functions
%% -----------------------------------------------------------------------------

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
