%%% Handles building the AST from tokens in the lexer.
%%%
%%% The parser here uses the Pratt parser, which is an extension of a
%%% traditional recursive descent parser, except that it keeps track of operator
%%% precedence to parse more complicated and nested expressions.

-module(aero_parser).

-export([parse/1, format_parse_error/1]).
-export_type([expr/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

-type expr() :: {source, meta(), [expr()]}
              | {integer_lit, meta(), integer()}
              | {float_lit, meta(), float()}
              | {atom_lit, meta(), atom()}
              | {string_lit, meta(), binary()}
              | {ident, meta(), atom()}
              | {op, meta(), atom()}
              | {block, meta(), [expr()]}
              | {expand, meta(), expr(), [expr()]}
              | {args, meta(), [expr()]}
              | {tag, meta(), expr(), expr()}
              | {attribute, meta(), expr(), expr()}
              | {inner_attribute, meta(), expr()}.

-type meta() :: [term()].

%% Parse tokens into the Aero AST.
-spec parse(binary()) -> {ok, expr()} | {error, term(), integer()}.
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
  process_tuples({source, [], Exprs}).

%% Parse newline-separated expressions until an end token.
group(Tokens, EndToken, Mode) ->
  group(Tokens, EndToken, Mode, []).

group([{Type, _, End} | _] = Tokens, {Type, End}, _Mode, Exprs) ->
  {Tokens, lists:reverse(Exprs)};
group([{End, _} | _] = Tokens, End, _Mode, Exprs) ->
  {Tokens, lists:reverse(Exprs)};
group([{newline, _} | Tail], EndToken, Mode, Exprs) ->
  group(Tail, EndToken, Mode, Exprs);
group([{space, _} | Tail], EndToken, Mode, Exprs) ->
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
  {Tokens3, T} = expect(Tokens2, EndToken),
  {Tokens3, Exprs, T};
container(Tokens, EndToken, _BP, InnerMode, _OuterMode, expr) ->
  {Tokens2, Expr} = expr(trim_whitespace(Tokens), InnerMode),
  {Tokens3, T} = expect(trim_whitespace(Tokens2), EndToken),
  {Tokens3, [Expr], T};
container(Tokens, EndToken, BP, InnerMode, OuterMode, exprs) ->
  case {trim_whitespace(Tokens), EndToken} of
    {[{Type, _, End} | _] = Tokens2, {Type, End}} ->
      {Tokens3, T} = expect(Tokens2, EndToken),
      {Tokens3, [], T};
    {[{End, _} | _] = Tokens2, End} ->
      {Tokens3, T} = expect(Tokens2, EndToken),
      {Tokens3, [], T};
    {Tokens2, _} ->
      {Tokens3, Exprs, T} = container(Tokens2, EndToken, BP, InnerMode, OuterMode, expr),
      case Exprs of
        [{op_args, _, InnerExprs}] -> {Tokens3, InnerExprs, T};
        _ -> {Tokens3, Exprs, T}
      end
  end;
container(Tokens, EndToken, BP, InnerMode, OuterMode, infix) ->
  {Tokens2, InnerExpr, T} = container(Tokens, EndToken, BP, InnerMode, OuterMode, expr),
  {Tokens3, RightExpr} = expr(trim_whitespace(Tokens2), BP, OuterMode),
  % Right goes first since the container applies to it like a prefix operator.
  {trim_whitespace(Tokens3), [RightExpr | InnerExpr], T}.

%% Parse an expression with precedence (binding power).
%%
%% A prefix expression is parsed first, followed by any number of postfix and
%% infix expressions.
%%
%% There are 5 expression modes:
%%
%% - `top`: Expressions at the top level of source and blocks.
%% - `con`: Expressions at the top level of containers.
%% - `arg`: Subexpressions inside macro arguments.
%% - `tup`: Subexpressions inside tuples at the top level.
%% - `sub`: Subexpressions inside other expressions.
%%
%% Macros cannot start inside `tup` or `sub` expressions. Commas have higher
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
        {Tokens2, InnerExprs, EndT} =
          container(Tail, {op, EndOp}, NextBP, InnerMode, NextMode, ContainerType),
        Expr = expr_prefix(T, EndT, InnerExprs, Mode),
        {Tokens2, Expr};
      _ ->
        % Some operators like `if` can be used in a prefix way. They are sent
        % back to try again.
        case op_to_tokens(T) of
          nil -> throw({parse_error, {unexpected_token, T}, ?LINE});
          OpTokens -> expr(OpTokens ++ Tail, MinBP, Mode)
        end
    end,
  expr_postfix_infix(Tokens, LeftExpr, MinBP, Mode);
expr([{space, _} | Tail], MinBP, Mode) ->
  % Getting rid of space at the beginning of an expression.
  expr(Tail, MinBP, Mode);
expr([T | Tail], MinBP, Mode) ->
  % If not a prefix operator, parse a single token.
  LeftExpr = expr_single(T, Mode),
  expr_postfix_infix(Tail, LeftExpr, MinBP, Mode).

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
            {Tokens3, InnerExprs, EndT} =
              container(Tail, {op, EndOp}, NextBP, InnerMode, NextMode, ContainerType),
            {Tokens3, expr_postfix(T, EndT, [LeftExpr | InnerExprs], Mode)}
        end,
      expr_postfix_infix(Tokens2, Expr, MinBP, Mode);
    {_, {BP, Assoc}, _} when Assoc =/= right, BP > MinBP; Assoc =:= right, BP >= MinBP ->
      % Right associative operators can also equal the minimum binding power,
      % this lets the right expression to keep being evalulated. For infix, the
      % next expression is parsed with an optional newline preceding it, and the
      % loop is continued.
      NextMode = next_mode(Mode, infix, Op),
      {NextBP, _} = op(NextMode, infix, Op),
      {Tokens2, RightExpr} = expr(trim_whitespace(Tail), NextBP, NextMode),
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
expr_postfix_infix([{space, Meta} | Tail] = Tokens, LeftExpr, MinBP, Mode) ->
  % When a space falls after an expression, check if we should convert this to
  % a space operator, or cause the expression to return. These space operator
  % insertions make macro calls work.
  case space_action(Mode, LeftExpr, hd(Tail)) of
    space_op -> expr_postfix_infix([{op, Meta, ' '} | Tail], LeftExpr, MinBP, Mode);
    continue -> expr_postfix_infix(Tail, LeftExpr, MinBP, Mode);
    break -> {Tokens, LeftExpr}
  end;
expr_postfix_infix([T | _], _LeftExpr, _MinBP, _Mode) ->
  throw({parse_error, {unexpected_token, T}, ?LINE}).

%% Simple, one token expressions.
expr_single({ident, _, Ident} = T, _Mode) ->
  {ident, get_meta(T), Ident};
expr_single({atom_lit, _, Atom} = T, _Mode) ->
  {atom_lit, get_meta(T), Atom};
expr_single({string_lit, _, String} = T, _Mode) ->
  {string_lit, get_meta(T), String};
expr_single({integer_lit, _, Integer} = T, _Mode) ->
  {integer_lit, get_meta(T), Integer};
expr_single({float_lit, _, Float} = T, _Mode) ->
  {float_lit, get_meta(T), Float}.

%% Build operator expressions that don't require the previous expression.
expr_prefix({op, _, Op} = T, Exprs, _Mode) ->
  % Normal prefix operator case.
  % To distinguish a prefix operator, an underscore is appended.
  OpBinary = atom_to_binary(Op, utf8),
  PrefixOp = binary_to_atom(<<OpBinary/binary, "_">>, utf8),
  {expand, get_meta(T), {op, get_meta(T), PrefixOp}, Exprs}.

expr_prefix({op, _, '('} = T, {op, _, ')'}, [{expand, _, {op, _, '()'}, InnerExprs}], _Mode) ->
  % Prevents unwrapping on the left side of `->`.
  {explicit_tuple, get_meta(T), InnerExprs};
expr_prefix({op, _, '('}, {op, _, ')'}, [InnerExpr], _Mode) ->
  % One item in parens gives back the inside without wrapping.
  InnerExpr;
expr_prefix({op, _, '{'} = T, {op, _, '}'}, InnerExprs, _Mode) ->
  {block, get_meta(T), InnerExprs};
expr_prefix({op, _, '#['} = T, {op, _, ']'}, [RightExpr, {op_args, _, InnerExprs}], _Mode) ->
  {attribute, get_meta(T), {args, get_meta(T), InnerExprs}, RightExpr};
expr_prefix({op, _, '#['} = T, {op, _, ']'}, [RightExpr, InnerExprs], _Mode) ->
  {attribute, get_meta(T), {args, get_meta(T), InnerExprs}, RightExpr};
expr_prefix({op, _, '#!['} = T, {op, _, ']'}, [{op_args, _, InnerExprs}], _Mode) ->
  {inner_attribute, get_meta(T), {args, get_meta(T), InnerExprs}};
expr_prefix({op, _, '#!['} = T, {op, _, ']'}, InnerExprs, _Mode) ->
  {inner_attribute, get_meta(T), {args, get_meta(T), InnerExprs}};
expr_prefix({op, _, LeftOp} = T, {op, _, RightOp}, Exprs, _Mode) ->
  LeftOpBinary = atom_to_binary(LeftOp, utf8),
  RightOpBinary = atom_to_binary(RightOp, utf8),
  ContainerOp = binary_to_atom(<<LeftOpBinary/binary, RightOpBinary/binary>>, utf8),
  {expand, get_meta(T), {op, get_meta(T), ContainerOp}, Exprs}.

%% Build operator expressions that need the previous expression.
expr_postfix({op, _, Op} = T, Exprs, _Mode) ->
  % Normal postfix operator case.
  % To distinguish a postfix operator, an underscore is prepended.
  OpBinary = atom_to_binary(Op, utf8),
  PostfixOp = binary_to_atom(<<"_", OpBinary/binary>>, utf8),
  {expand, get_meta(hd(Exprs)), {op, get_meta(T), PostfixOp}, Exprs}.

expr_postfix({op, _, '('} = T, {op, _, ')'}, [LeftExpr | InnerExprs], Mode) ->
  expr_postfix({op, get_meta(T), '()'}, [LeftExpr, {args, get_meta(T), InnerExprs}], Mode);
expr_postfix({op, _, '['} = T, {op, _, ']'}, [LeftExpr | InnerExprs], Mode) ->
  expr_postfix({op, get_meta(T), '[]'}, [LeftExpr, {args, get_meta(T), InnerExprs}], Mode).

%% Parse expressions that need both the previous and next expressions.
expr_infix({op, _, '->'} = T, [{expand, Meta, {op, _, '()'}, LeftExprs}, RightExpr], Mode)
    when Mode =/= top ->
  % `->`, except at the top level, unwraps regular tuples and turns them back
  % to `op_args`. This is transformed again in the general `->` case.
  expr_infix(T, [{op_args, Meta, LeftExprs}, RightExpr], Mode);
expr_infix({op, _, '->'} = T, [LeftExpr, RightExpr], _Mode) ->
  {expand, get_meta(LeftExpr), {op, get_meta(T), '->'}, [arrow_args(LeftExpr), RightExpr]};
expr_infix({op, _, ':'}, [LeftExpr, RightExpr], _Mode) ->
  {tag, get_meta(LeftExpr), LeftExpr, RightExpr};
expr_infix({op, _, ','}, [LeftExpr, RightExpr], arg) ->
  % `,` in `arg` mode is handled as a macro arg separator.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ','}, [LeftExpr, RightExpr], _Mode) ->
  % `,` anywhere else makes it be normal args. Most args are converted to tuples
  % in a pass later on.
  op_args(LeftExpr, RightExpr);
expr_infix({op, _, '$'}, [LeftExpr, RightExpr], _Mode) ->
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, RightExpr], top) ->
  % When ` ` appears at the top, it starts a macro.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, RightExpr], con) ->
  % ` ` also starts a macro in containers.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [{expand, _, _, _} = LeftExpr, RightExpr], arg) ->
  % The left expression can be a nested expand as an argument before a block.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, {block, _, _} = RightExpr], arg) ->
  % ` ` before a block in macro args acts as a `,`.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, {expand_args, _, [{block, _, _} | _]} = RightExpr], arg) ->
  % Also behave as a `,` when the block to the right is inside args already.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, RightExpr], arg) ->
  % Otherwise, in `arg` mode, a ` ` will start a new macro.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, _, Op} = T, Exprs, _Mode) ->
  % Normal infix operator case.
  {expand, get_meta(hd(Exprs)), {op, get_meta(T), Op}, Exprs}.

%% Build the args on the left side of `->` for `top` expressions.
%%
%% When `=` and `<-` and used, their `op_args` are converted to `args` nodes
%% which won't be translated to implicit tuples. This stops recursing after any
%% elements are hit except these two.
arrow_args({expand, _, {op, _, OpName} = Op, Exprs} = Expr) when OpName =:= '=';
                                                                 OpName =:= '<-' ->
  {expand, get_meta(Expr), Op, lists:map(fun arrow_args/1, Exprs)};
arrow_args({op_args, _, Exprs} = T) ->
  {args, get_meta(T), Exprs};
arrow_args(Expr) ->
  {args, get_meta(Expr), [Expr]}.

%% Build operator args. Args to the right are flattened.
op_args(LeftExpr, RightExpr) ->
  {op_args, get_meta(LeftExpr), [LeftExpr | plain_op_args(RightExpr)]}.

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

%% Turn op args or a single item into a plain list.
plain_op_args({op_args, _, Exprs}) -> Exprs;
plain_op_args([{op_args, _, Exprs}]) -> Exprs;
plain_op_args([]) -> [];
plain_op_args([Expr]) -> [Expr];
plain_op_args(Expr) -> [Expr].

%% Replace explicit tuples with regular tuples and create implicit tuples.
process_tuples({explicit_tuple, Meta, Exprs}) ->
  {expand, Meta, {op, Meta, '()'}, lists:map(fun process_tuples/1, Exprs)};
process_tuples({op_args, Meta, Exprs}) ->
  {expand, Meta, {op, Meta, '()'}, lists:map(fun process_tuples/1, Exprs)};
process_tuples({Type, Meta, Expr1, Exprs}) when is_list(Exprs) ->
  {Type, Meta, process_tuples(Expr1), lists:map(fun process_tuples/1, Exprs)};
process_tuples({Type, Meta, Expr1, Expr2}) ->
  {Type, Meta, process_tuples(Expr1), process_tuples(Expr2)};
process_tuples({Type, Meta, Exprs}) when is_list(Exprs) ->
  {Type, Meta, lists:map(fun process_tuples/1, Exprs)};
process_tuples({Type, Meta, Expr}) ->
  {Type, Meta, process_tuples(Expr)};
process_tuples({Type, Meta}) ->
  {Type, Meta};
process_tuples(Other) ->
  Other.

%% Configure operators with binding power and associativity.
%%
%% Commas make a special case in binding power on whether or not they are in a
%% top-level expression, subexpression, or macro args. This allows the parens to
%% be optional around tuples when not inside any parentheses or in macro args.
%% Commas aren't necessarily operators truly either, since they aren't returned
%% as an expand node. `$` is also not a true operator, as it forces the term to
%% the left to be a macro and is turned into an expand node with the expression
%% to the left as the operator. ` ` is a synthetic operator which is also for
%% for use in macro calls, and not included in the final AST. The op at the end
%% of containers is used to easily have expressions end on them.
op(_,   prefix,  '(')    -> {250, right};  % 250  prefix   right  ()
op(_,   prefix,  '#(')   -> {250, right};  %                      #()
op(_,   prefix,  '{')    -> {250, right};  %                      {}
op(_,   prefix,  '#{')   -> {250, right};  %                      #{}
op(_,   prefix,  '[')    -> {250, right};  %                      []
op(_,   prefix,  '<<')   -> {250, right};  %                      <<>>
op(_,   infix,   '.')    -> {240, left};   % 240  infix    left   .
op(_,   infix,   '?.')   -> {240, left};   %                      ?.
op(_,   infix,   '!.')   -> {240, left};   %                      !.
op(_,   postfix, '?')    -> {230, left};   % 230  postfix  left   ?
op(_,   postfix, '!')    -> {230, left};   %                      !
op(_,   postfix, '?!')   -> {230, left};   %                      ?!
op(_,   postfix, '(')    -> {230, left};   %                      ()
op(_,   postfix, '[')    -> {230, left};   %                      []
op(_,   postfix, '...')  -> {230, left};   %                      ...
op(_,   prefix,  '+')    -> {220, right};  % 220  prefix   right  +
op(_,   prefix,  '-')    -> {220, right};  %                      -
op(_,   prefix,  '!')    -> {220, right};  %                      !
op(_,   prefix,  '~~~')  -> {220, right};  %                      ~~~
op(_,   prefix,  '&')    -> {220, right};  %                      &
op(_,   prefix,  '^')    -> {220, right};  %                      ^
op(_,   prefix,  '*')    -> {220, right};  %                      *
op(_,   prefix,  '..')   -> {220, right};  %                      ..
op(_,   prefix,  '...')  -> {220, right};  %                      ...
op(_,   prefix,  '...<') -> {220, right};  %                      ...<
op(_,   infix,   '..')   -> {210, none};   % 210  prefix   none   ..
op(_,   infix,   '..<')  -> {210, none};   %                      ..<
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
op(_,   infix,   '==')   -> {100, left};   % 100  infix    left   ==
op(_,   infix,   '!=')   -> {100, left};   %                      !=
op(_,   infix,   '<')    -> {100, left};   %                      <
op(_,   infix,   '>')    -> {100, left};   %                      >
op(_,   infix,   '<=')   -> {100, left};   %                      <=
op(_,   infix,   '>=')   -> {100, left};   %                      >=
op(_,   infix,   '<=>')  -> {100, left};   %                      <=>
op(_,   infix,   '&&')   -> {90,  left};   %  90  infix    left   &&
op(_,   infix,   '||')   -> {80,  left};   %  80  infix    left   ||
op(top, infix,   ':')    -> {75,  right};  %  75  infix    right  top :
op(tup, infix,   ':')    -> {75,  right};  %                      tuple :
op(con, prefix,  '#[')   -> {70,  right};  %  70  prefix   right  con #[]
op(arg, prefix,  '#[')   -> {70,  right};  %                      arg #[]
op(sub, prefix,  '#[')   -> {70,  right};  %                      sub #[]
op(top, infix,   ',')    -> {65,  right};  %  65  infix    right  top ,
op(tup, infix,   ',')    -> {65,  right};  %                      tuple ,
op(top, infix,   '$')    -> {65,  right};  %                      top $
op(tup, infix,   '$')    -> {65,  right};  %                      tuple $
op(top, infix,   ' ')    -> {65,  right};  %                      top (space)
op(_,   infix,   '&')    -> {60,  left};   %  60  infix    left   &
op(_,   infix,   '|')    -> {50,  left};   %  50  infix    left   |
op(_,   infix,   '=')    -> {40,  right};  %  40  infix    right  =
op(_,   infix,   '<-')   -> {40,  right};  %                      <-
op(_,   infix,   '->')   -> {30,  right};  %  30  infix    right  ->
op(_,   infix,   '=>')   -> {25,  right};  %  25  infix    right  =>
op(_,   infix,   ':')    -> {20,  right};  %  20  infix    right  :
op(_,   infix,   ',')    -> {10,  right};  %  10  infix    right  ,
op(_,   infix,   '$')    -> {10,  right};  %                      $
op(_,   infix,   ' ')    -> {10,  right};  %                      (space)
op(_,   prefix,  '#[')   -> {5,   right};  %   5  prefix   right  #[]
op(_,   prefix,  '#![')  -> {5,   right};  %                      #![]
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
%% to not allows macros to start for the rest of the expression. Anything else,
%% and the next thing well be a subexpression. `=` and arrows at the top level
%% stay at the top level on the right side.
next_mode(top,  infix,  ',')  -> tup;   % ,
next_mode(tup,  infix,  ',')  -> tup;   %
next_mode(arg,  infix,  ',')  -> arg;   %
next_mode(_,    infix,  '$')  -> arg;   % $
next_mode(top,  infix,  ' ')  -> arg;   % (space)
next_mode(con,  infix,  ' ')  -> arg;   %
next_mode(arg,  infix,  ' ')  -> arg;   %
next_mode(top,  infix,  '=')  -> top;   % =
next_mode(top,  infix,  '<-') -> top;   % <-
next_mode(top,  infix,  '->') -> top;   % ->
next_mode(top,  infix,  '=>') -> top;   % =>
next_mode(Mode, prefix, '#[') -> Mode;  % #[]
next_mode(_,    prefix, _)    -> sub;
next_mode(_,    infix,  _)    -> sub.

%% Action to take place when a space is found in postfix/infix mode.
%%
%% The `space_op` action causes a space token to be translated into a space
%% operator, which is used for macro calls and macro args. Also enforces that
%% function calls and subscripts cannot have a space between, e.g. f() and x[].
space_action(top, {ident, _, _}, {op, _, '('}) -> space_op;  % x ()
space_action(con, {ident, _, _}, {op, _, '('}) -> space_op;  % (x ())
space_action(arg, {ident, _, _}, {op, _, '('}) -> space_op;  % x y ()
space_action(_,   _,             {op, _, '('}) -> break;     %
space_action(top, {ident, _, _}, {op, _, '['}) -> space_op;  % x []
space_action(con, {ident, _, _}, {op, _, '['}) -> space_op;  % (x [])
space_action(arg, {ident, _, _}, {op, _, '['}) -> space_op;  % x y []
space_action(_,   _,             {op, _, '['}) -> break;     %
space_action(top, {ident, _, _}, {op, _, '{'}) -> space_op;  % x { }
space_action(con, {ident, _, _}, {op, _, '{'}) -> space_op;  % (x { })
space_action(arg, {ident, _, _}, {op, _, '{'}) -> space_op;  % x y { }
space_action(arg, {block, _, _}, {op, _, '{'}) -> continue;  % x { } { }  (bad)
space_action(arg, _,             {op, _, '{'}) -> space_op;  % x 1 { }
space_action(sub, _,             {op, _, '{'}) -> break;     % x y == 1 { }
space_action(_,   _,             {op, _, '{'}) -> continue;  %
space_action(top, {ident, _, _}, {op, _, Op})  -> space_action_op(Op);
space_action(con, {ident, _, _}, {op, _, Op})  -> space_action_op(Op);
space_action(arg, {ident, _, _}, {op, _, Op})  -> space_action_op(Op);
space_action(top, {ident, _, _}, _)            -> space_op;  % x y
space_action(con, {ident, _, _}, _)            -> space_op;  % (x y)
space_action(arg, {ident, _, _}, _)            -> space_op;  % x y z
space_action(_,   _,             _)            -> continue.

space_action_op(Op) ->
  case {op(arg, postfix, Op), op(arg, infix, Op)} of
    {0, 0} -> space_op;
    _ -> continue
  end.

%% Map operators to containers.
%%
%% Gives the closing operator and it's parsing behavior. The use in infix
%% expressions isn't allowed.
container_op(prefix,  '(')   -> {')',  exprs, con};  % ()
container_op(postfix, '(')   -> {')',  exprs, con};  % ()
container_op(prefix,  '{')   -> {'}',  group, top};  % {}
container_op(prefix,  '[')   -> {']',  exprs, con};  % []
container_op(postfix, '[')   -> {']',  exprs, con};  % []
container_op(prefix,  '#(')  -> {')',  exprs, con};  % #()
container_op(prefix,  '#{')  -> {'}',  exprs, con};  % #{}
container_op(prefix,  '#[')  -> {']',  infix, con};  % #[]
container_op(prefix,  '#![') -> {']',  expr,  con};  % #![]
container_op(prefix,  '<<')  -> {'>>', exprs, con};  % <<>>
container_op(prefix,  _)     -> nil;
container_op(postfix, _)     -> nil.

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

%% Throw an error if a token is not found, otherwise, pop the token off.
expect([{Type, _, Expected} = T | Tail], {Type, Expected}) -> {Tail, T};
expect([{Expected, _, _} = T | Tail], Expected) -> {Tail, T};
expect([{Expected, _} = T | Tail], Expected) -> {Tail, T};
expect([Found | _], Expected) -> throw({parse_error, {expected_token, Expected, Found}, ?LINE}).

%% Pop off whitespace tokens if they come up.
trim_whitespace([{newline, _} | Tail]) -> trim_whitespace(Tail);
trim_whitespace([{space, _} | Tail]) -> trim_whitespace(Tail);
trim_whitespace(Tokens) -> Tokens.

%% Get a token's or expression's line number and put it in metadata.
get_meta({_, Line, _}) when is_integer(Line) -> [{line, Line}];
get_meta({_, Line}) when is_integer(Line) -> [{line, Line}];
get_meta({_, Meta, _, _}) when is_list(Meta) -> Meta;
get_meta({_, Meta, _}) when is_list(Meta) -> Meta;
get_meta({_, Meta}) when is_list(Meta) -> Meta.

%% Convert operators to tokens if possible.
op_to_tokens({op, Meta, Op}) when Op =:= in; Op =:= as ->
  % These infix operators are converted to idents when used in a prefix way.
  [{ident, Meta, Op}];
op_to_tokens({op, Meta, '!in'}) ->
  % `!in` is special since it looks like a negated `in`.
  [{op, Meta, '!'}, {ident, Meta, in}];
op_to_tokens(_T) ->
  nil.

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
