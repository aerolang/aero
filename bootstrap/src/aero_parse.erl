%%% Handles building the AST from tokens in the lexer.
%%%
%%% The parser here uses the Pratt parser, which is an extension of a
%%% traditional recursive descent parser, except that it keeps track of operator
%%% precedence to parse more complicated and nested expressions.

-module(aero_parse).

-export([parse/1]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Parse tokens into the Aero AST.
-spec parse([aero_token:token()]) -> {ok, aero_ast:source()} | {error, term()}.
parse([]) ->
  {error, no_tokens};
parse(Tokens) ->
  try source(Tokens) of
    Expr -> {ok, Expr}
  catch
    throw:{parse_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Parsing
%% -----------------------------------------------------------------------------

%% Continuously parse expressions in the source until EOF.
source(Tokens) ->
  {[{eof, EofMeta}], Exprs} = group(Tokens, eof, top),
  Meta = merge_meta([aero_token:meta(hd(Tokens)), EofMeta]),
  process_tuples({source, Meta, Exprs}).

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
      {[{End, _} | _], End}               -> true;
      {[{newline, _} | _], _}             -> true;
      _                                   -> false
    end,
  case ExprEnded of
    true ->
      group(Tokens2, EndToken, Mode, [Expr | Exprs]);
    false ->
      throw({parse_error, {unexpected_token, hd(Tokens2)}})
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
        [{expand, [op_args | _], _, [{args, _, InnerExprs}]}] -> {Tokens3, InnerExprs, T};
        _                                                     -> {Tokens3, Exprs, T}
      end
  end;
container(Tokens, EndToken, BP, InnerMode, OuterMode, infix) ->
  {Tokens2, InnerExpr, T} = container(Tokens, EndToken, BP, InnerMode, OuterMode, expr),
  {Tokens3, RightExpr} = expr(trim_whitespace(Tokens2), BP, OuterMode),
  % Right goes first since the container applies to it like a prefix operator.
  {Tokens3, [RightExpr, InnerExpr], T}.

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
      % Only right precedence works with prefix operators. This is the typical
      % operator case when it's not a container.
      {{BP, right}, nil} when BP >= MinBP ->
        {Tokens2, RightExpr} = expr(Tail, NextBP, NextMode),
        Expr = expr_prefix(T, [RightExpr], Mode),
        {Tokens2, Expr};

      % Here, we handle containers.
      {{BP, right}, {EndOp, ContainerType, InnerMode}} when BP >= MinBP ->
        {Tokens2, InnerExprs, EndT} =
          container(Tail, {op, EndOp}, NextBP, InnerMode, NextMode, ContainerType),
        Expr = expr_prefix(T, EndT, InnerExprs, Mode),
        {Tokens2, Expr};

      % Some operators like `if` can be used in a prefix way. They are sent back
      % to try again.
      _ ->
        case op_to_tokens(T) of
          nil -> throw({parse_error, {unexpected_token, T}});
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
    % Error when nonassociative ops are found twice in a row, can only be for
    % infix ops.
    {{0, _}, {_, none}, {expand, _, {op, _, Op}, _}} ->
      throw({parse_error, {nonassoc_op, T}});

    % Postfix is always left associative. We just parse a postfix op, and
    % continue in the loop.
    {{BP, left}, _, _} when BP > MinBP ->
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

    % Right associative operators can also equal the minimum binding power, this
    % lets the right expression to keep being evalulated. For infix, the next
    % expression is parsed with an optional newline preceding it, and the loop
    % is continued.
    {_, {BP, Assoc}, _} when Assoc =/= right, BP > MinBP; Assoc =:= right, BP >= MinBP ->
      NextMode = next_mode(Mode, infix, Op),
      {NextBP, _} = op(NextMode, infix, Op),
      {Tokens2, RightExpr} = expr(trim_whitespace(Tail), NextBP, NextMode),
      Expr = expr_infix(T, [LeftExpr, RightExpr], Mode),

      expr_postfix_infix(Tokens2, Expr, MinBP, Mode);

    % No operators with high enough binding power.
    _ ->
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
    break    -> {Tokens, LeftExpr}
  end;
expr_postfix_infix([T | _], _LeftExpr, _MinBP, _Mode) ->
  throw({parse_error, {unexpected_token, T}}).

%% Simple, one token expressions.
expr_single({ident, Meta, Ident}, _Mode) ->
  {ident, Meta, Ident};
expr_single({type_param, Meta, TypeParam}, _Mode) ->
  {type_param, Meta, TypeParam};
expr_single({blank, Meta}, _Mode) ->
  {blank, Meta};
expr_single({atom_lit, Meta, Atom}, _Mode) ->
  {atom_lit, Meta, Atom};
expr_single({str_lit, Meta, String}, _Mode) ->
  {str_lit, Meta, String};
expr_single({int_lit, Meta, Integer}, _Mode) ->
  {int_lit, Meta, Integer};
expr_single({float_lit, Meta, Float}, _Mode) ->
  {float_lit, Meta, Float}.

%% Build operator expressions that don't require the previous expression.
expr_prefix({op, OpMeta, Op}, Exprs, _Mode) ->
  % Normal prefix operator case.
  % To distinguish a prefix operator, an underscore is appended.
  OpBinary = atom_to_binary(Op, utf8),
  PrefixOp = binary_to_atom(<<OpBinary/binary, "_">>, utf8),
  Meta = merge_meta([OpMeta | aero_ast:metas(Exprs)]),

  {expand, Meta, {op, OpMeta, PrefixOp}, Exprs}.

expr_prefix({op, LeftMeta, '('},
            {op, RightMeta, ')'},
            [{expand, _, {op, _, '(_)'}, InnerExprs}],
            _Mode) ->
  % Prevents unwrapping on the left side of `->`.
  Meta = merge_meta([LeftMeta, RightMeta]),

  {expand, [explicit_tuple | Meta], {op, Meta, '(_)'}, InnerExprs};
expr_prefix({op, _, '('}, {op, _, ')'}, [InnerExpr], _Mode) ->
  % One item in parens gives back the inside without wrapping.
  InnerExpr;
expr_prefix({op, LeftMeta, '{'}, {op, RightMeta, '}'}, InnerExprs, _Mode) ->
  Meta = merge_meta([LeftMeta, RightMeta]),

  {block, Meta, InnerExprs};
expr_prefix({op, LeftMeta, '#['},
            {op, RightMeta, ']'},
            [RightExpr, {expand, [op_args | InnerMeta], _, [{args, _, InnerExprs}]}],
            _Mode) ->
  Meta = merge_meta([LeftMeta, RightMeta]),

  {attribute, Meta, {args, InnerMeta, InnerExprs}, RightExpr};
expr_prefix({op, LeftMeta, '#['}, {op, RightMeta, ']'}, [RightExpr, InnerExprs], _Mode) ->
  Meta = merge_meta([LeftMeta, RightMeta]),

  {attribute, Meta, {args, merge_meta(aero_ast:metas(InnerExprs)), InnerExprs}, RightExpr};
expr_prefix({op, LeftMeta, '#!['},
            {op, RightMeta, ']'},
            [{expand, [op_args | InnerMeta], _, [{args, _, InnerExprs}]}],
            _Mode) ->
  Meta = merge_meta([LeftMeta, RightMeta]),

  {inner_attribute, Meta, {args, InnerMeta, InnerExprs}};
expr_prefix({op, LeftMeta, '#!['}, {op, RightMeta, ']'}, InnerExprs, _Mode) ->
  Meta = merge_meta([LeftMeta, RightMeta]),

  {inner_attribute, Meta, {args, merge_meta(aero_ast:metas(InnerExprs)), InnerExprs}};
expr_prefix({op, LeftMeta, LeftOp}, {op, RightMeta, RightOp}, InnerExprs, _Mode) ->
  LeftOpBinary = atom_to_binary(LeftOp, utf8),
  RightOpBinary = atom_to_binary(RightOp, utf8),
  ContainerOp = binary_to_atom(<<LeftOpBinary/binary, "_", RightOpBinary/binary>>, utf8),
  Exprs = [{args, merge_meta(aero_ast:metas(InnerExprs)), InnerExprs}],
  Meta = merge_meta([LeftMeta, RightMeta]),

  {expand, Meta, {op, Meta, ContainerOp}, Exprs}.

%% Build operator expressions that need the previous expression.
expr_postfix({op, OpMeta, Op}, Exprs, _Mode) ->
  % Normal postfix operator case.
  % To distinguish a postfix operator, an underscore is prepended.
  OpBinary = atom_to_binary(Op, utf8),
  PostfixOp = binary_to_atom(<<"_", OpBinary/binary>>, utf8),
  Meta = merge_meta([OpMeta | aero_ast:metas(Exprs)]),

  {expand, Meta, {op, OpMeta, PostfixOp}, Exprs}.

expr_postfix({op, LeftMeta, LeftOp}, {op, RightMeta, RightOp}, [LeftExpr | InnerExprs], _Mode) ->
  LeftOpBinary = atom_to_binary(LeftOp, utf8),
  RightOpBinary = atom_to_binary(RightOp, utf8),
  ContainerOp = binary_to_atom(<<"_", LeftOpBinary/binary, "_", RightOpBinary/binary>>, utf8),
  Exprs = [LeftExpr, {args, merge_meta(aero_ast:metas(InnerExprs)), InnerExprs}],
  Meta = merge_meta([aero_ast:meta(LeftExpr), RightMeta]),

  {expand, Meta, {op, merge_meta([LeftMeta, RightMeta]), ContainerOp}, Exprs}.

%% Parse expressions that need both the previous and next expressions.
expr_infix({op, _, '->'} = T,
           [{expand, Meta, {op, _, '(_)'} = Op, LeftArgs}, RightExpr],
           Mode) when Mode =/= top, hd(Meta) =/= explicit_tuple, hd(Meta) =/= op_args ->
  % `->`, except at the top level, unwraps regular tuples and turns them back
  % to `op_args`. This is transformed again in the general `->` case.
  expr_infix(T, [{expand, [op_args | Meta], Op, LeftArgs}, RightExpr], Mode);
expr_infix({op, OpMeta, '->'}, [LeftExpr, RightExpr], _Mode) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr, RightExpr])),

  {expand, Meta, {op, OpMeta, '_->_'}, [arrow_args(LeftExpr), RightExpr]};
expr_infix({op, _, '->>'} = T,
           [{expand, Meta, {op, _, '(_)'} = Op, LeftArgs}, RightExpr],
           Mode) when Mode =/= top, hd(Meta) =/= explicit_tuple, hd(Meta) =/= op_args ->
  % Same special cases for `->>`.
  expr_infix(T, [{expand, [op_args | Meta], Op, LeftArgs}, RightExpr], Mode);
expr_infix({op, OpMeta, '->>'}, [LeftExpr, RightExpr], _Mode) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr, RightExpr])),

  {expand, Meta, {op, OpMeta, '_->>_'}, [arrow_args(LeftExpr), RightExpr]};
expr_infix({op, _, ':'}, [LeftExpr, RightExpr], _Mode) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr, RightExpr])),

  {tag, Meta, LeftExpr, RightExpr};
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
expr_infix({op, _, ' '}, [LeftExpr, {expand, _, _, [{block, _, _}, _]} = RightExpr], arg) ->
  % Also behave as a `,` when the block to the right is inside a binary op.
  macro_args(LeftExpr, RightExpr);
expr_infix({op, _, ' '}, [LeftExpr, RightExpr], arg) ->
  % Otherwise, in `arg` mode, a ` ` will start a new macro.
  macro_call(LeftExpr, RightExpr);
expr_infix({op, OpMeta, Op}, Exprs, _Mode) ->
  % Normal infix operator case.
  % To distinguish an infix operator, underscores are placed on both sides.
  OpBinary = atom_to_binary(Op, utf8),
  InfixOp = binary_to_atom(<<"_", OpBinary/binary, "_">>, utf8),
  Meta = merge_meta(aero_ast:metas(Exprs)),

  {expand, Meta, {op, OpMeta, InfixOp}, Exprs}.

%% Build the args on the left side of `->` for `top` expressions.
%%
%% When `@` is used, its `op_args` are converted to `args` nodes which won't be
%% translated to implicit tuples. This stops recursing after any elements are
%% hit except more `@`s.
%%
%% To allow for empty arguments in `->` in `top` expressions, tuples `()` are
%% converted to empty arguments and explicit tuples `(())` are left alone.
arrow_args({expand, Meta, {op, _, '@'} = Op, Exprs}) ->
  {expand, Meta, Op, lists:map(fun arrow_args/1, Exprs)};
arrow_args({expand, [explicit_tuple | Meta], _, _} = Expr) ->
  {args, Meta, [Expr]};
arrow_args({expand, [op_args | Meta], _, [{args, _, Exprs}]}) ->
  {args, Meta, Exprs};
arrow_args({expand, Meta, {op, _, '(_)'}, [{args, _, []}]}) ->
  {args, Meta, []};
arrow_args(Expr) ->
  {args, aero_ast:meta(Expr), [Expr]}.

%% Build operator args. Args to the right are flattened.
op_args(LeftExpr, RightExpr) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr, RightExpr])),
  Args = [LeftExpr | plain_op_args(RightExpr)],

  {expand, [op_args | Meta], {op, Meta, '(_)'}, [{args, Meta, Args}]}.

%% Build a macro call with args on the right.
macro_call(LeftExpr, {expand_args, _, RightExprs}) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr | RightExprs])),
  {expand, Meta, LeftExpr, RightExprs};
macro_call(LeftExpr, RightExpr) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr, RightExpr])),
  {expand, Meta, LeftExpr, [RightExpr]}.

%% Build macro args. Args to the right are flattened.
macro_args(LeftExpr, {expand_args, _, RightExprs}) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr | RightExprs])),
  {expand_args, Meta, [LeftExpr | RightExprs]};
macro_args(LeftExpr, RightExpr) ->
  Meta = merge_meta(aero_ast:metas([LeftExpr, RightExpr])),
  {expand_args, Meta, [LeftExpr, RightExpr]}.

%% Turn op args or a single item into a plain list.
plain_op_args({expand, [op_args | _],  _, [{args, _, Exprs}]})  -> Exprs;
plain_op_args([{expand, [op_args | _], _, [{args, _, Exprs}]}]) -> Exprs;
plain_op_args([])                                               -> [];
plain_op_args([Expr])                                           -> [Expr];
plain_op_args(Expr)                                             -> [Expr].

%% Replace explicit tuples with regular tuples and create implicit tuples.
process_tuples({expand, [explicit_tuple | Meta], Op, Exprs}) ->
  {expand, Meta, Op, lists:map(fun process_tuples/1, Exprs)};
process_tuples({expand, [op_args | Meta], Op, Exprs}) ->
  {expand, Meta, Op, lists:map(fun process_tuples/1, Exprs)};
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
op(_,   infix,   '.')    -> {240, left};   % 240  infix    left   .
op(_,   infix,   '?.')   -> {240, left};   %                      ?.
op(_,   infix,   '!.')   -> {240, left};   %                      !.
op(_,   prefix,  '#')    -> {235, right};  % 235  prefix   right  #
op(_,   postfix, '?')    -> {230, left};   % 230  postfix  left   ?
op(_,   postfix, '!')    -> {230, left};   %                      !
op(_,   postfix, '(')    -> {230, left};   %                      ()
op(_,   postfix, '[')    -> {230, left};   %                      []
op(_,   postfix, '...')  -> {230, left};   %                      ...
op(_,   prefix,  '+')    -> {220, right};  % 220  prefix   right  +
op(_,   prefix,  '-')    -> {220, right};  %                      -
op(_,   prefix,  '~~~')  -> {220, right};  %                      ~~~
op(_,   prefix,  '&')    -> {220, right};  %                      &
op(_,   prefix,  '^')    -> {220, right};  %                      ^
op(_,   prefix,  '*')    -> {220, right};  %                      *
op(_,   prefix,  '<<-')  -> {220, right};  %                      <<-
op(_,   prefix,  '..')   -> {220, right};  %                      ..
op(_,   prefix,  '...')  -> {220, right};  %                      ...
op(_,   prefix,  '...<') -> {220, right};  %                      ...<
op(_,   infix,   '..')   -> {210, none};   % 210  infix    none   ..
op(_,   infix,   '..<')  -> {210, none};   %                      ..<
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
op(_,   infix,   '::')   -> {130, right};  % 130  infix    right  ::
op(_,   infix,   '++')   -> {130, right};  %                      ++
op(_,   infix,   '??')   -> {120, left};   % 120  infix    left   ??
op(_,   infix,   '!!')   -> {120, left};   %                      !!
op(_,   infix,   '==')   -> {100, left};   % 100  infix    left   ==
op(_,   infix,   '<>')   -> {100, left};   %                      <>
op(_,   infix,   '<')    -> {100, left};   %                      <
op(_,   infix,   '>')    -> {100, left};   %                      >
op(_,   infix,   '<=')   -> {100, left};   %                      <=
op(_,   infix,   '>=')   -> {100, left};   %                      >=
op(_,   prefix,  'not')  -> {95,  right};  %  95  prefix   right  not
op(_,   infix,   'and')  -> {90,  left};   %  90  infix    left   and
op(_,   infix,   'or')   -> {85,  left};   %  85  infix    left   or
op(_,   infix,   '<<-')  -> {80,  right};  %  80  infix    right  <<-
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
op(_,   infix,   '|')    -> {55,  left};   %  55  infix    left   |
op(con, infix,   '@')    -> {50,  right};  %  50  infix    right  con @
op(arg, infix,   '@')    -> {50,  right};  %                      arg @
op(sub, infix,   '@')    -> {50,  right};  %                      sub @
op(con, infix,   '->')   -> {45,  right};  %  45  infix    right  con ->
op(arg, infix,   '->')   -> {45,  right};  %                      arg ->
op(sub, infix,   '->')   -> {45,  right};  %                      sub ->
op(con, infix,   '->>')  -> {45,  right};  %                      con ->>
op(arg, infix,   '->>')  -> {45,  right};  %                      arg ->>
op(sub, infix,   '->>')  -> {45,  right};  %                      sub ->>
op(_,   infix,   ':')    -> {40,  right};  %  40  infix    right  :
op(_,   infix,   '@')    -> {35,  right};  %  35  infix    right  @
op(_,   infix,   '->')   -> {30,  right};  %  30  infix    right  ->
op(_,   infix,   '->>')  -> {30,  right};  %                      ->>
op(_,   infix,   '=')    -> {25,  right};  %  25  infix    right  =
op(_,   infix,   '<-')   -> {25,  right};  %                      <-
op(_,   infix,   '=>')   -> {25,  right};  %                      =>
op(_,   infix,   'if')   -> {20,  right};  %  20  infix    right  if
op(_,   infix,   else)   -> {20,  right};  %                      else
op(_,   infix,   for)    -> {20,  right};  %                      for
op(_,   infix,   while)  -> {20,  right};  %                      while
op(_,   infix,   where)  -> {20,  right};  %                      where
op(_,   infix,   as)     -> {20,  right};  %                      as
op(_,   infix,   ',')    -> {10,  right};  %  10  infix    right  ,
op(_,   infix,   '$')    -> {10,  right};  %                      $
op(_,   infix,   ' ')    -> {10,  right};  %                      (space)
op(_,   prefix,  '#[')   -> {5,   right};  %   5  prefix   right  #[]
op(_,   prefix,  '#![')  -> {5,   right};  %                      #![]
op(_,   postfix, ')')    -> {-1,  right};  %  -1  End of containers
op(_,   postfix, '}')    -> {-1,  right};  %      (can't stand alone)
op(_,   postfix, ']')    -> {-1,  right};  %
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
next_mode(top,  infix,  ',')   -> tup;   % ,
next_mode(tup,  infix,  ',')   -> tup;   %
next_mode(arg,  infix,  ',')   -> arg;   %
next_mode(_,    infix,  '$')   -> arg;   % $
next_mode(top,  infix,  ' ')   -> arg;   % (space)
next_mode(con,  infix,  ' ')   -> arg;   %
next_mode(arg,  infix,  ' ')   -> arg;   %
next_mode(top,  infix,  '=')   -> top;   % =
next_mode(top,  infix,  '<-')  -> top;   % <-
next_mode(top,  infix,  '->')  -> top;   % ->
next_mode(top,  infix,  '->>') -> top;   % ->>
next_mode(top,  infix,  '=>')  -> top;   % =>
next_mode(Mode, prefix, '#[')  -> Mode;  % #[]
next_mode(_,    prefix, _)     -> sub;
next_mode(_,    infix,  _)     -> sub.

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
    {{0, _}, {0, _}} -> space_op;
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
container_op(prefix,  _)     -> nil;
container_op(postfix, _)     -> nil.

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

%% Throw an error if a token is not found, otherwise, pop the token off.
expect([{Type, _, Expected} = T | Tail], {Type, Expected}) ->
  {Tail, T};
expect([{Expected, _, _} = T | Tail], Expected) ->
  {Tail, T};
expect([{Expected, _} = T | Tail], Expected) ->
  {Tail, T};
expect([Found | _], Expected) ->
  throw({parse_error, {expected_token, Expected, Found}}).

%% Pop off whitespace tokens if they come up.
trim_whitespace([{newline, _} | Tail]) -> trim_whitespace(Tail);
trim_whitespace([{space, _} | Tail])   -> trim_whitespace(Tail);
trim_whitespace(Tokens)                -> Tokens.

%% Combine code spans together and use the first line/column.
merge_meta([]) ->
  [];
merge_meta(Metas) ->
  Line = proplists:get_value(line, hd(Metas)),
  Column = proplists:get_value(column, hd(Metas)),
  Span = proplists:get_value(span, hd(Metas)),

  merge_meta(tl(Metas), Line, Column, Span).

merge_meta([], Line, Column, Span) ->
  [{line, Line}, {column, Column}, {span, Span}];
merge_meta([Meta | Tail], Line, Column, Span) ->
  Line2 = proplists:get_value(line, Meta),
  Column2 = proplists:get_value(column, Meta),
  Span2 = proplists:get_value(span, Meta),

  case Line2 < Line orelse (Line2 =:= Line andalso Column2 < Column) of
    true  -> merge_meta(Tail, Line2, Column2, aero_span:merge(Span, Span2));
    false -> merge_meta(Tail, Line, Column, aero_span:merge(Span, Span2))
  end.

%% Convert operators to tokens if possible.
op_to_tokens({op, Meta, Op}) when Op =:= 'if'; Op =:= else; Op =:= for; Op =:= while ->
  % These infix operators are converted to idents when used in a prefix way.
  [{ident, Meta, Op}];
op_to_tokens(_T) ->
  nil.
