%%% Handles building the AST from tokens in the lexer.
%%%
%%% The parser here uses the Pratt parser, which is an extension of a
%%% traditional recursive descent parser, except that it keeps track of operator
%%% precedence to parse more complicated and nested expressions.

-module(aero_parser).

-export([parse/1, format_parse_error/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Parse tokens into the Aero AST.
parse([]) ->
  {error, no_tokens};
parse(Tokens) ->
  try source(Tokens) of
    {[{eof, _}], Expr} -> {ok, Expr}
  catch
    throw:{parse_error, Reason} -> {error, Reason}
  end.

%% Format a parsing error for displaying.
format_parse_error(Reason) -> format_error(Reason).

%% -----------------------------------------------------------------------------
%% Parsing
%% -----------------------------------------------------------------------------

-define(BLOCK(T), element(1, T) =:= '{').
-define(SIMPLE(T), element(1, T) =:= ident;
                   element(1, T) =:= atom_lit;
                   element(1, T) =:= string_lit).
-define(SUBEXPR(T), ?BLOCK(T); ?SIMPLE(T)).

%% Continuously parse expressions in the source.
source(Tokens) -> source(Tokens, []).

source([{eof, _}] = Eof, Exprs) ->
  Source = {source, [], lists:reverse(Exprs)},
  {Eof, Source};
source([{newline, _} | Tail], Exprs) ->
  source(Tail, Exprs);
source(Tokens, Exprs) ->
  {Tokens2, Expr} = expr_bp(Tokens),
  source(Tokens2, [Expr | Exprs]).

%% Parse expressions in a block until it closes.
block([T | _] = Tokens) ->
  {Tokens2, Exprs} = block_tail(expect(Tokens, '{'), []),
  Block = {block, get_meta(T), Exprs},
  {Tokens2, Block}.

block_tail([{'}', _} | Tail], Exprs) ->
  {Tail, lists:reverse(Exprs)};
block_tail([{newline, _} | Tail], Exprs) ->
  block_tail(Tail, Exprs);
block_tail(Tokens, Exprs) ->
  {Tokens2, Expr} = expr_bp(Tokens),
  block_tail(Tokens2, [Expr | Exprs]).

%% Parse a general expression with precedence (binding power).
%%
%% A prefix expression is parsed first, followed by any number of postfix and
%% infix expressions.
expr_bp(Tokens) -> expr_bp(Tokens, 1).

expr_bp(Tokens, MinBP) ->
  {Tokens2, LeftExpr} = expr_prefix(Tokens),
  expr_postfix_infix(Tokens2, LeftExpr, MinBP).

%% Parse expressions that don't require the previous.
expr_prefix([T | _] = Tokens) when ?BLOCK(T) ->
  block(Tokens);
expr_prefix([T | Tail]) when ?SIMPLE(T) ->
  Expr = token_to_ast(T),
  case Tail of
    [Next | _] when ?SUBEXPR(Next) -> macro_call(Tail, Expr);
    _ -> {Tail, Expr}
  end;
expr_prefix([{Next, _, _} | _]) ->
  throw({parse_error, {unexpected_token, Next}});
expr_prefix([{Next, _} | _]) ->
  throw({parse_error, {unexpected_token, Next}}).

%% Parse postfix and infix expressions as long as there remain tokens to parse
%% and the operators meet the required binding power.
expr_postfix_infix([{eof, _}] = Eof, LeftExpr, _MinBP) ->
  % End of parsing. Bubbling up the EOF to break recursion.
  {Eof, LeftExpr};
expr_postfix_infix([{Op, _} = T | Tail] = Tokens, LeftExpr, MinBP) ->
  case {postfix_binding_power(Op), infix_binding_power(Op)} of
    {0, 0} ->
      throw({parse_error, {unexpected_token, Op}});
    {LeftBP, _} when LeftBP >= MinBP ->
      {Tokens2, Expr} = expr_postfix(Tail, T, LeftExpr),
      expr_postfix_infix(Tokens2, Expr, MinBP);
    {_, {LeftBP, RightBP}} when LeftBP >= MinBP ->
      {Tokens2, RightExpr} = expr_bp(allow(Tail, newline), RightBP),
      {Tokens3, Expr} = expr_infix(Tokens2, T, LeftExpr, RightExpr, RightBP),
      expr_postfix_infix(Tokens3, Expr, MinBP);
    _ ->
      % No operators with high enough binding power.
      {Tokens, LeftExpr}
  end;
expr_postfix_infix([{Next, _, _} | _], _LeftExpr, _MinBP) ->
  throw({parse_error, {unexpected_token, Next}}).

%% Parse expressions that need the previous expression.
expr_postfix(_Tokens, _Op, _LeftExpr) ->
  throw(unimplemented).

%% Parse expressions that need both the previous and next expressions.
expr_infix(Tokens, {'=', _}, LeftExpr, RightExpr, _RightBP) ->
  Expr = {op_call, get_meta(LeftExpr), [
    {op, bind},
    {left, LeftExpr},
    {right, RightExpr}
  ]},
  {Tokens, Expr}.

%% Expressions found inside general expressions.
subexpr([T | _] = Tokens) when ?BLOCK(T) ->
  block(Tokens);
subexpr([T | Tail]) when ?SIMPLE(T) ->
  {Tail, token_to_ast(T)};
subexpr([{Next, _, _} | _]) ->
  throw({parse_error, {unexpected_token, Next}});
subexpr([{Next, _} | _]) ->
  throw({parse_error, {unexpected_token, Next}}).

%% Parse macro calls.
macro_call(Tokens, Ident) ->
  {Tokens2, Args} = macro_args(Tokens),
  Expr = {macro_call, get_meta(Ident), [
    {name, token_to_ast(Ident)},
    {args, Args}
  ]},
  {Tokens2, Expr}.

%% Macro arguments are comma-separated subexpressions, though commas are
%% not required before blocks.
macro_args(Tokens) -> macro_args(Tokens, []).

macro_args(Tokens, Args) ->
  {Tokens2, Arg} = subexpr(Tokens),
  Args2 = [{pos_arg, get_meta(Arg), Arg} | Args],
  case Tokens2 of
    [{',', _} | Tail] -> macro_args(allow(Tail, newline), Args2);
    [T | _] when ?BLOCK(T) -> macro_args(Tokens2, Args2);
    _ -> {Tokens2, lists:reverse(Args2)}
  end.

infix_binding_power(',') -> {30, 31};
infix_binding_power('=') -> {100, 99};
infix_binding_power(_) -> {0, 0}.

postfix_binding_power(_) -> 0.

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

%% Throw an error if a token is not found, otherwise, pop the token off.
expect([{Expected, _, _} | Tail], Expected) ->
  Tail;
expect([{Found, _, _} | _], Expected) ->
  throw({parse_error, {unexpected_token, Found, Expected}});
expect([{Expected, _} | Tail], Expected) ->
  Tail;
expect([{Found, _} | _], Expected) ->
  throw({parse_error, {unexpected_token, Found, Expected}}).

%% Pop the token off if it's there, but don't throw an error if it isn't.
allow([{Allowed, _, _} | Tail], Allowed) -> Tail;
allow([{Allowed, _} | Tail], Allowed) -> Tail;
allow(Tokens, _Allowed) -> Tokens.

%% Convert a token containing a value to an AST node.
token_to_ast({Category, _, Value} = Token) ->
  {Category, get_meta(Token), Value}.

%% Get a token's line number and put it in metadata.
get_meta({_, Line}) when is_integer(Line) ->
  [{line, Line}];

get_meta({_, Line, _}) when is_integer(Line) ->
  [{line, Line}];

get_meta({_, Meta, _}) when is_list(Meta) ->
  Meta.

%% -----------------------------------------------------------------------------
%% Error Formatting
%% -----------------------------------------------------------------------------

format_error(no_tokens) ->
  <<"no tokens provided to parser, at least EOF is required">>;
format_error({unexpected_token, Found}) ->
  format_message("unexpected token '~s'", [Found]);
format_error({unexpected_token, Found, Expected}) ->
  format_message("unexpected token '~s', expected '~s'", [Found, Expected]);
format_error(_) ->
  <<"unknown error">>.

format_message(Format, Terms) ->
  TermStrings = [io_lib:write(T) || T <- Terms],
  Message = lists:flatten(io_lib:fwrite(Format, TermStrings)),
  list_to_binary(Message).
