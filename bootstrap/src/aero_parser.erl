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
  {error, no_tokens, ?LINE};
parse(Tokens) ->
  try source(Tokens) of
    Expr -> {ok, Expr}
  catch
    throw:{parse_error, Reason, Line} -> {error, Reason, Line}
  end.

%% Format a parsing error for displaying.
format_parse_error(Reason) -> format_error(Reason).

%% -----------------------------------------------------------------------------
%% Parsing
%% -----------------------------------------------------------------------------

%% Continuously parse expressions in the source until EOF.
source(Tokens) ->
  {[{eof, _}], Exprs} = group(Tokens, eof, top),
  {source, [], Exprs}.

%% Parse newline-separated expressions until an end token.
group(Tokens, EndToken, Mode) ->
  group(Tokens, EndToken, Mode, []).

group([{Type, _, End} | _] = Tokens, {Type, End}, _Mode, Exprs) ->
  {Tokens, lists:reverse(Exprs)};
group([{End, _} | _] = Tokens, End, _Mode, Exprs) ->
  {Tokens, lists:reverse(Exprs)};
group([{newline, _} | Tail], EndToken, Mode, Exprs) ->
  group(Tail, EndToken, Mode, Exprs);
group(Tokens, EndToken, Mode, Exprs) ->
  {Tokens2, Expr} = expr(Tokens, Mode),
  % Expressions that end on anything except a newline or the end token are
  % ending unexpectedly.
  ExprEnded =
    case {Tokens2, EndToken} of
      {[{Type, _, End} | _], {Type, End}} -> true;
      {[{End, _} | _], End} -> true;
      {[{newline, _} | _], _} -> true;
      _ -> false
    end,
  case ExprEnded of
    true ->
      group(Tokens2, EndToken, Mode, [Expr | Exprs]);
    false ->
      throw({parse_error, {unexpected_token, hd(Tokens2)}, ?LINE})
  end.

%% Parse the remainder of a container after it's starting token.
%%
%% Containers wrap expressions inside and can also act as a prefix operator
%% an expression to the right.
%%
%% Four parsing types are allowed:
%%
%% - `group`: Read expressions inside container separated by newlines.
%% - `expr`: Read a single expression inside container.
%% - `exprs`: Read comma-separated expressions inside or let it be empty.
%% - `infix`: Read an expression inside and to the right of the container.
container(Tokens, EndToken, _BP, InnerMode, _OuterMode, group) ->
  {Tokens2, Exprs} = group(Tokens, EndToken, InnerMode),
  {expect(Tokens2, EndToken), Exprs};
container(Tokens, EndToken, _BP, InnerMode, _OuterMode, expr) ->
  {Tokens2, Expr} = expr(allow(Tokens, newline), InnerMode),
  {expect(allow(Tokens2, newline), EndToken), [Expr]};
container(Tokens, EndToken, BP, InnerMode, OuterMode, exprs) ->
  case {allow(Tokens, newline), EndToken} of
    {[{Type, _, End} | _] = Tokens2, {Type, End}} ->
      {expect(Tokens2, EndToken), []};
    {[{End, _} | _] = Tokens2, End} ->
      {expect(Tokens2, EndToken), []};
    {Tokens2, _} ->
      {Tokens3, Exprs} = container(Tokens2, EndToken, BP, InnerMode, OuterMode, expr),
      % Unwrapping inner tuples.
      case Exprs of
        [{tuple_lit, _, InnerExprs}] -> {Tokens3, InnerExprs};
        _ -> {Tokens3, Exprs}
      end
  end;
container(Tokens, EndToken, BP, InnerMode, OuterMode, infix) ->
  {Tokens2, InnerExpr} = container(Tokens, EndToken, BP, InnerMode, OuterMode, expr),
  {Tokens3, RightExpr} = expr(allow(Tokens2, newline), BP, OuterMode),
  % Right goes first since the container applies to it like a prefix operator.
  {allow(Tokens3, newline), [RightExpr | InnerExpr]}.

%% Parse an expression with precedence (binding power).
%%
%% A prefix expression is parsed first, followed by any number of postfix and
%% infix expressions.
%%
%% There are 5 expression modes:
%%
%% - `top`: Expressions as parsed from source and blocks.
%% - `sub`: Subexpressions inside parens and brackets.
%% - `arg`: Macro arguments.
%% - `tup`: Continuations of top-level expressions after a comma.
%% - `lit`: Continuations of subexpressions after a comma.
%%
%% Macros cannot start inside `tup` or `lit` expressions. Commas have higher
%% binding power in top-level expressions for implicit tuple literals.
expr(Tokens, Mode) -> expr(Tokens, 1, Mode).

expr([{op, _, Op} = T | Tail], MinBP, Mode) ->
  % First thing in parsing is to look for prefix operators. After we loop
  % through postfix and infix operators through the rest.
  NextMode = next_mode(Mode, prefix, Op),
  {NextBP, _} = op(NextMode, prefix, Op),
  {Tokens, LeftExpr} =
    case {op(Mode, prefix, Op), container_op(prefix, Op)} of
      {{BP, right}, nil} when BP >= MinBP ->
        % Only right precedence works with prefix operators. This is the typical
        % operator case when it's not a container.
        {Tokens2, RightExpr} = expr(Tail, NextBP, NextMode),
        Expr = expr_prefix(T, [RightExpr], Mode),
        {Tokens2, Expr};
      {{BP, right}, {EndOp, ContainerType, InnerMode}} when BP >= MinBP ->
        % Here, we handle containers.
        {Tokens2, InnerExprs} =
          container(Tail, {op, EndOp}, NextBP, InnerMode, NextMode, ContainerType),
        Expr = expr_prefix(T, InnerExprs, Mode),
        {Tokens2, Expr};
      _ ->
        throw({parse_error, {unexpected_token, T}, ?LINE})
    end,
  % The token insertions are for custom logic to implement macro calls.
  Inserted = expr_insert(LeftExpr, T, hd(Tokens), Mode),
  expr_postfix_infix(Inserted ++ Tokens, LeftExpr, MinBP, Mode);
expr([T | Tail], MinBP, Mode) ->
  % If not a prefix operator, parse a single token.
  LeftExpr = expr_single(T, Mode),
  Inserted = expr_insert(LeftExpr, T, hd(Tail), Mode),
  expr_postfix_infix(Inserted ++ Tail, LeftExpr, MinBP, Mode).

%% Parse postfix and infix expressions as long as there remain tokens to parse
%% and the operators meet the required binding power.
expr_postfix_infix([{op, _, Op} = T | Tail] = Tokens, LeftExpr, MinBP, Mode) ->
  case {op(Mode, postfix, Op), op(Mode, infix, Op), LeftExpr} of
    {{0, _}, {_, none}, {expand, _, {op, _, Op}, _}} ->
      % Error when nonassociative ops are found twice in a row, can only be for
      % infix ops.
      throw({parse_error, {nonassoc_op, T}, ?LINE});
    {{BP, left}, _, _} when BP > MinBP ->
      % Postfix is always left associative. We just parse a postfix op, and
      % continue in the loop.
      {Tokens2, Expr} =
        case container_op(postfix, Op) of
          nil ->
            % Since there's no expression to parse after, we don't need to check
            % for the next mode or binding power.
            {Tail, expr_postfix(T, [LeftExpr], Mode)};
          {EndOp, ContainerType, InnerMode} ->
            NextMode = next_mode(Mode, prefix, Op),
            {NextBP, _} = op(NextMode, prefix, Op),
            {Tokens3, InnerExprs} =
              container(Tail, {op, EndOp}, NextBP, InnerMode, NextMode, ContainerType),
            {Tokens3, expr_postfix(T, [LeftExpr | InnerExprs], Mode)}
        end,
      % Postfix has insertions as well for macro calls.
      Inserted = expr_insert(LeftExpr, T, hd(Tail), Mode),
      expr_postfix_infix(Inserted ++ Tokens2, Expr, MinBP, Mode);
    {_, {BP, Assoc}, _} when Assoc =/= right, BP > MinBP; Assoc =:= right, BP >= MinBP ->
      % Right associative operators can also equal the minimum binding power,
      % this lets the right expression to keep being evalulated. For infix, the
      % next expression is parsed with an optional newline preceding it, and the
      % loop is continued.
      NextMode = next_mode(Mode, infix, Op),
      {NextBP, _} = op(NextMode, infix, Op),
      {Tokens2, RightExpr} = expr(allow(Tail, newline), NextBP, NextMode),
      Expr = expr_infix(T, [LeftExpr, RightExpr], Mode),
      expr_postfix_infix(Tokens2, Expr, MinBP, Mode);
    _ ->
      % No operators with high enough binding power.
      {Tokens, LeftExpr}
  end;
expr_postfix_infix([{eof, _}] = Tokens, LeftExpr, _MinBP, _Mode) ->
  {Tokens, LeftExpr};
expr_postfix_infix([{newline, _} | _] = Tokens, LeftExpr, _MinBP, _Mode) ->
  {Tokens, LeftExpr};
expr_postfix_infix([T | _], _LeftExpr, _MinBP, _Mode) ->
  throw({parse_error, {unexpected_token, T}, ?LINE}).

%% Magic for inserting tokens after prefix and postfix expressions.
%%
%% The previous expression can be used and token that started it can be used,
%% along with the next token and mode. Infix doesn't use this because and infix
%% expression isn't ever finished while in `expr_postfix_infix/4`.
%%
%% This allows for macros to work using inserted operators.
expr_insert(_LeftExpr, _T, {eof, _}, _Mode) ->
  [];
expr_insert(_LeftExpr, _T, {newline, _}, _Mode) ->
  [];
expr_insert({ident, _, _}, {ident, _, _} = T, {op, _, Op}, Mode) when Op =/= '(', Op =/= '[' ->
  %% Checking for both the expression and token to be `ident` to prevent
  %% `ident_paren` and `ident_brack` from using this rule.
  case {op(Mode, postfix, Op), op(Mode, infix, Op)} of
    {{0, _}, {0, _}} -> [{op, get_meta(T), ' '}];
    _ -> []
  end;
expr_insert({ident, _, _}, {ident, _, _} = T, _Next, _Mode) ->
  [{op, get_meta(T), ' '}];
expr_insert({block, _, _}, _T, {op, _, '{'}, arg) ->
  [];
expr_insert(_LeftExpr, _T, {op, _, '{'} = Next, arg) ->
  [{op, get_meta(Next), ' '}];
expr_insert(_LeftExpr, _T, _Next, _Mode) ->
  [].

%% Simple, one token expressions.
expr_single({ident, _, Ident} = T, _Mode) ->
  {ident, get_meta(T), Ident};
expr_single({ident_paren, _, Ident} = T, _Mode) ->
  {ident, get_meta(T), Ident};
expr_single({ident_brack, _, Ident} = T, _Mode) ->
  {ident, get_meta(T), Ident};
expr_single({quote_ident, _, Ident} = T, _Mode) ->
  {quote_ident, get_meta(T), Ident};
expr_single({atom_lit, _, Atom} = T, _Mode) ->
  {atom_lit, get_meta(T), Atom};
expr_single({string_lit, _, String} = T, _Mode) ->
  {string_lit, get_meta(T), String};
expr_single({integer_lit, _, Integer} = T, _Mode) ->
  {integer_lit, get_meta(T), Integer};
expr_single({float_lit, _, Float} = T, _Mode) ->
  {float_lit, get_meta(T), Float}.

%% Build operator expressions that don't require the previous expression.
expr_prefix({op, _, '('} = T, [], _Mode) ->
  {unit, get_meta(T)};
expr_prefix({op, _, '('}, [InnerExpr], _Mode) ->
  InnerExpr;
expr_prefix({op, _, '('} = T, InnerExprs, _Mode) ->
  {tuple_lit, get_meta(T), InnerExprs};
expr_prefix({op, _, '#('} = T, InnerExprs, _Mode) ->
  {record_lit, get_meta(T), InnerExprs};
expr_prefix({op, _, '{'} = T, InnerExprs, _Mode) ->
  {block, get_meta(T), InnerExprs};
expr_prefix({op, _, '#{'} = T, InnerExprs, _Mode) ->
  {map_lit, get_meta(T), InnerExprs};
expr_prefix({op, _, '['} = T, InnerExprs, _Mode) ->
  {array_lit, get_meta(T), InnerExprs};
expr_prefix({op, _, '#['} = T, [RightExpr | InnerExprs], _Mode) ->
  {attribute, get_meta(T), InnerExprs, RightExpr};
expr_prefix({op, _, '#!['} = T, InnerExprs, _Mode) ->
  {inner_attribute, get_meta(T), InnerExprs};
expr_prefix({op, _, '<<'} = T, InnerExprs, _Mode) ->
  {bits_lit, get_meta(T), InnerExprs};
expr_prefix({op, _, Op} = T, Exprs, _Mode) ->
  % Normal prefix operator case.
  % To distinguish a prefix operator, an underscore is appended.
  OpBinary = atom_to_binary(Op, utf8),
  PrefixOp = binary_to_atom(<<OpBinary/binary, "_">>, utf8),
  {expand, get_meta(T), {op, get_meta(T), PrefixOp}, Exprs}.

%% Build operator expressions that need the previous expression.
expr_postfix({op, _, '('} = T, Exprs, Mode) ->
  expr_postfix({op, get_meta(T), '()'}, Exprs, Mode);
expr_postfix({op, _, '['} = T, Exprs, Mode) ->
  expr_postfix({op, get_meta(T), '[]'}, Exprs, Mode);
expr_postfix({op, _, '.{'} = T, [LeftExpr, InnerExpr], Mode) ->
  expr_postfix({op, get_meta(T), '.{}'}, [LeftExpr, {block, get_meta(T), InnerExpr}], Mode);
expr_postfix({op, _, Op} = T, Exprs, _Mode) ->
  % Normal postfix operator case.
  % To distinguish a postfix operator, an underscore is prepended.
  OpBinary = atom_to_binary(Op, utf8),
  PostfixOp = binary_to_atom(<<"_", OpBinary/binary>>, utf8),
  {expand, get_meta(hd(Exprs)), {op, get_meta(T), PostfixOp}, Exprs}.

%% Parse expressions that need both the previous and next expressions.
expr_infix({op, _, ':'}, [LeftExpr, RightExpr], _Mode) ->
  {tag, get_meta(LeftExpr), LeftExpr, RightExpr};
expr_infix({op, _, ','}, [LeftExpr, RightExpr], arg) ->
  % `,` in args is handled as a macro arg separator.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ','}, [LeftExpr, RightExpr], _Mode) ->
  % `,` anywhere else is a tuple separator.
  tuple(LeftExpr, RightExpr);
expr_infix({op, _, '$'} = T, [{tuple_lit, _, _} = _LeftExpr, _RightExpr], top) ->
  throw({parse_error, {expected_token, {op, ','}, T}, ?LINE});
expr_infix({op, _, '$'}, [LeftExpr, RightExpr], top) ->
  % `$` allows a macro to start anywhere in `top` as long as it's not
  % directly after a tuple in a top-level expression.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, '$'}, [LeftExpr, RightExpr], sub) ->
  % `$` will work when a subexpression starts.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, '$'}, [LeftExpr, RightExpr], arg) ->
  % `$` will work anywhere in macro args.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, '$'} = T, [_LeftExpr, _RightExpr], _Mode) ->
  throw({parse_error, {expected_token, {op, ','}, T}, ?LINE});
expr_infix({op, _, ' '}, [{tuple_lit, _, _} = _LeftExpr, _RightExpr], top) ->
  throw({parse_error, {expected_token, {op, ','}}, ?LINE});
expr_infix({op, _, ' '}, [LeftExpr, RightExpr], top) ->
  % The synthetic ` ` operator acts as a `$` to start macros.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, RightExpr], sub) ->
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, {block, _, _} = RightExpr], arg) ->
  % A block to the right inside args makes a ` ` behave as a `,`.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, {expand_args, _, [{block, _, _} | _]} = RightExpr], arg) ->
  % Also behave as a `,` when the block to the right is inside args already.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, RightExpr], arg) ->
  % Otherwise, a ` ` in macro args acts as a `$`.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [_LeftExpr, _RightExpr], _Mode) ->
  throw({parse_error, {expected_token, {op, ','}}, ?LINE});
expr_infix({op, _, _} = T, Exprs, _Mode) ->
  % Normal infix operator case.
  {expand, get_meta(hd(Exprs)), T, Exprs}.

%% Build a tuple. Tuples to the right are flattened.
tuple(LeftExpr, RightExpr) ->
  {tuple_lit, get_meta(LeftExpr), [LeftExpr | plain_tuple(RightExpr)]}.

%% Build a macro call with args on the right.
macro_call(LeftExpr, {expand_args, _, RightExprs}) ->
  {expand, get_meta(LeftExpr), LeftExpr, RightExprs};
macro_call(LeftExpr, RightExpr) ->
  {expand, get_meta(LeftExpr), LeftExpr, [RightExpr]}.

%% Build macro args. Args to the right are flattened.
macro_args(LeftExpr, {expand_args, _, RightExprs}) ->
  {expand_args, get_meta(LeftExpr), [LeftExpr | RightExprs]};
macro_args(LeftExpr, RightExpr) ->
  {expand_args, get_meta(LeftExpr), [LeftExpr, RightExpr]}.

%% Turn a tuple or single item into a plain list.
plain_tuple({tuple_lit, _, Exprs}) -> Exprs;
plain_tuple([{tuple_lit, _, Exprs}]) -> Exprs;
plain_tuple([]) -> [];
plain_tuple([Expr]) -> [Expr];
plain_tuple(Expr) -> [Expr].

%% Configure operators with binding power and associativity.
%%
%% Commas make a special case in binding power on whether or not they are in a
%% top-level expression, subexpression, or macro args. This allows the parens to
%% be optional around tuples when not inside any parentheses or in macro args.
%% Commas aren't necessarily operators truly either, since they aren't returned
%% as an expand node. `$` is also not a true operator, as it forces the term to
%% the left to be a macro and is turned into an expand node with the expression
%% to the left as the operator. ` ` is a synthetic operator which is also for
%% for use in macro calls, and not included in the final AST. The op of
%% containers is used to easily have expressions end on them.
op(_,   prefix,  '(')    -> {250, right};  % 250  prefix   right  ()
op(_,   prefix,  '#(')   -> {250, right};  %                      #()
op(_,   prefix,  '{')    -> {250, right};  %                      {}
op(_,   prefix,  '#{')   -> {250, right};  %                      #{}
op(_,   prefix,  '[')    -> {250, right};  %                      []
op(_,   prefix,  '#[')   -> {250, right};  %                      #[]
op(_,   prefix,  '#![')  -> {250, right};  %                      #![]
op(_,   prefix,  '<<')   -> {250, right};  %                      <<>>
op(_,   infix,   '.')    -> {240, left};   % 240  infix    left   .
op(_,   infix,   '?.')   -> {240, left};   %                      ?.
op(_,   infix,   '!.')   -> {240, left};   %                      !.
op(_,   postfix, '?')    -> {230, left};   % 230  postfix  left   ?
op(_,   postfix, '!')    -> {230, left};   %                      !
op(_,   postfix, '?!')   -> {230, left};   %                      ?!
op(_,   postfix, '(')    -> {230, left};   %                      ()
op(_,   postfix, '[')    -> {230, left};   %                      []
op(_,   postfix, '.{')   -> {230, left};   %                      .{}
op(_,   prefix,  '+')    -> {220, right};  % 220  prefix   right  +
op(_,   prefix,  '-')    -> {220, right};  %                      -
op(_,   prefix,  '!')    -> {220, right};  %                      !
op(_,   prefix,  '~~~')  -> {220, right};  %                      ~~~
op(_,   prefix,  '&')    -> {220, right};  %                      &
op(_,   prefix,  '^')    -> {220, right};  %                      ^
op(_,   prefix,  '*')    -> {220, right};  %                      *
op(_,   prefix,  '**')   -> {220, right};  %                      **
op(_,   infix,   '..')   -> {210, none};   % 210  prefix   none   ..
op(_,   infix,   '..<')  -> {210, none};   %                      ..<
op(_,   infix,   '...')  -> {210, none};   %                      ...
op(_,   infix,   '...<') -> {210, none};   %                      ...<
op(_,   infix,   as)     -> {200, left};   % 200  infix    left   as
op(_,   infix,   '*')    -> {190, left};   % 190  infix    left   *
op(_,   infix,   '/')    -> {190, left};   %                      /
op(_,   infix,   '%')    -> {190, left};   %                      %
op(_,   infix,   '+')    -> {180, left};   % 180  infix    left   +
op(_,   infix,   '-')    -> {180, left};   %                      -
op(_,   infix,   '<<<')  -> {170, left};   % 170  infix    left   <<<
op(_,   infix,   '>>>')  -> {170, left};   %                      >>>
op(_,   infix,   '&&&')  -> {160, left};   % 160  infix    left   &&&
op(_,   infix,   '^^^')  -> {150, left};   % 150  infix    left   ^^^
op(_,   infix,   '|||')  -> {140, left};   % 140  infix    left   |||
op(_,   infix,   '::')   -> {135, right};  % 135  infix    right  ::
op(_,   infix,   '\\\\') -> {130, left};   % 130  infix    left   \\
op(_,   infix,   '??')   -> {130, left};   %                      ??
op(_,   infix,   '!!')   -> {130, left};   %                      !!
op(_,   infix,   '++')   -> {120, left};   % 120  infix    left   ++
op(_,   infix,   in)     -> {110, none};   % 110  infix    none   in
op(_,   infix,   '!in')  -> {110, none};   %                      !in
op(_,   infix,   '==')   -> {100,  left};  % 100  infix    none   ==
op(_,   infix,   '!=')   -> {100,  left};  %                      !=
op(_,   infix,   '<')    -> {100,  left};  %                      <
op(_,   infix,   '>')    -> {100,  left};  %                      >
op(_,   infix,   '<=')   -> {100,  left};  %                      <=
op(_,   infix,   '>=')   -> {100,  left};  %                      >=
op(_,   infix,   '<=>')  -> {100,  left};  %                      <=>
op(_,   infix,   '&&')   -> {90,  left};   %  90  infix    left   &&
op(_,   infix,   '||')   -> {80,  left};   %  80  infix    left   ||
op(top, infix,   ':')    -> {75,  none};   %  75  infix    none   top-level :
op(tup, infix,   ':')    -> {75,  none};   %                      tuple :
op(top, infix,   ',')    -> {70,  right};  %  70  infix    right  top-level ,
op(tup, infix,   ',')    -> {70,  right};  %                      tuple ,
op(top, infix,   '$')    -> {70,  right};  %                      top-level $
op(tup, infix,   '$')    -> {70,  right};  %                      tuple $
op(top, infix,   ' ')    -> {70,  right};  %                      (synthetic)
op(tup, infix,   ' ')    -> {70,  right};  %                      (synthetic)
op(_,   infix,   '&')    -> {60,  left};   %  60  infix    left   &
op(_,   infix,   '|')    -> {50,  left};   %  50  infix    left   |
op(_,   infix,   '=>')   -> {40,  right};  %  40  infix    right  =>
op(_,   infix,   '=')    -> {30,  right};  %  30  infix    right  =
op(_,   infix,   '<-')   -> {30,  right};  %                      <-
op(_,   infix,   '->')   -> {20,  right};  %  20  infix    right  ->
op(_,   infix,   ':')    -> {15,  none};   %  15  infix    none   :
op(_,   infix,   ',')    -> {10,  right};  %  10  infix    right  ,
op(_,   infix,   '$')    -> {10,  right};  %                      $
op(_,   infix,   ' ')    -> {10,  right};  %                      (synthetic)
op(_,   postfix, ')')    -> {-1,  right};  %  -1  End of containers
op(_,   postfix, '}')    -> {-1,  right};  %      (can't stand alone)
op(_,   postfix, ']')    -> {-1,  right};  %
op(_,   postfix, '>>')   -> {-1,  right};  %
op(_,   prefix,  _)      -> {0,   none};
op(_,   postfix, _)      -> {0,   none};
op(_,   infix,   _)      -> {0,   none}.

%% Configure what happens to the expr mode after using an operator.
%%
%% Macro call separators `$` and ` ` cause the expression to go into a macro arg
%% parsing mode until the expression ends. This drops the binding power of a
%% comma in the middle of an expression. If a tuple starts, then we set the mode
%% to not allows macros to start for the rest of the expression.
next_mode(top, infix, ',') -> tup;
next_mode(top, infix, '$') -> arg;
next_mode(top, infix, ' ') -> arg;
next_mode(sub, infix, ',') -> lit;
next_mode(sub, infix, '$') -> arg;
next_mode(sub, infix, ' ') -> arg;
next_mode(Prev, _, _) -> Prev. 

%% Map operators to containers.
%%
%% Gives the closing operator and it's parsing behavior. The use in infix
%% expressions isn't allowed.
container_op(prefix,  '(')   -> {')',  exprs, sub};  % ()
container_op(postfix, '(')   -> {')',  exprs, sub};  % ()
container_op(prefix,  '{')   -> {'}',  group, top};  % {}
container_op(prefix,  '[')   -> {']',  exprs, sub};  % []
container_op(postfix, '[')   -> {']',  exprs, sub};  % []
container_op(prefix,  '#(')  -> {')',  exprs, sub};  % #()
container_op(prefix,  '#{')  -> {'}',  exprs, sub};  % #{}
container_op(postfix, '.{')  -> {'}',  group, sub};  % .{}
container_op(prefix,  '#[')  -> {']',  infix, sub};  % #[]
container_op(prefix,  '#![') -> {']',  expr,  sub};  % #![]
container_op(prefix,  '<<')  -> {'>>', exprs, sub};  % <<>>
container_op(prefix,  _)     -> nil;
container_op(postfix, _)     -> nil.

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

%% Throw an error if a token is not found, otherwise, pop the token off.
expect([{Type, _, Expected} | Tail], {Type, Expected}) -> Tail;
expect([{Expected, _, _} | Tail], Expected) -> Tail;
expect([{Expected, _} | Tail], Expected) -> Tail;
expect([Found | _], Expected) -> throw({parse_error, {expected_token, Expected, Found}, ?LINE}).

%% Pop the token off if it's there, but don't throw an error if it isn't.
allow([{Type, _, Allowed} | Tail], {Type, Allowed}) -> Tail;
allow([{Allowed, _, _} | Tail], Allowed) -> Tail;
allow([{Allowed, _} | Tail], Allowed) -> Tail;
allow(Tokens, _Allowed) -> Tokens.

%% Get a token's or expression's line number and put it in metadata.
get_meta({_, Line, _}) when is_integer(Line) -> [{line, Line}];
get_meta({_, Line}) when is_integer(Line) -> [{line, Line}];
get_meta({_, Meta, _, _}) when is_list(Meta) -> Meta;
get_meta({_, Meta, _}) when is_list(Meta) -> Meta;
get_meta({_, Meta}) when is_list(Meta) -> Meta.

%% -----------------------------------------------------------------------------
%% Error Formatting
%% -----------------------------------------------------------------------------

format_error(no_tokens) ->
  <<"no tokens provided to parser, at least EOF is required">>;
format_error({unexpected_token, Found}) ->
  format_message("unexpected token '~p'", [Found]);
format_error({expected_token, Expected, Found}) ->
  format_message("expected token '~p', found token '~p'", [Expected, Found]);
format_error({expected_token, Expected}) ->
  format_message("expected token '~p'", [Expected]);
format_error({nonassoc_op, Op}) ->
  format_message("operator token '~p' is not associative", [Op]);
format_error(_) ->
  <<"unknown error">>.

format_message(Format, Terms) ->
  TermStrings = [io_lib:write(T) || T <- Terms],
  Message = lists:flatten(io_lib:fwrite(Format, TermStrings)),
  list_to_binary(Message).
