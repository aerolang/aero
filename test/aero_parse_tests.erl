-module(aero_parse_tests).

-include_lib("eunit/include/eunit.hrl").

scan(Source) ->
  Result = aero_scan:scan(Source),
  ?assertMatch({ok, _}, Result),

  {ok, Tokens} = Result,
  Tokens.

parse(Source) ->
  Tokens = scan(Source),
  Result = aero_parse:parse(Tokens),

  {ok, Ast} = Result,
  Ast.

empty_test_() ->
  [
    % Empty source.
    ?_assertMatch({source, _, []}, parse(<<"">>)),
    % Only a newline.
    ?_assertMatch({source, _, []}, parse(<<"\n">>))
  ].

expand_test_() ->
  [
    % Single expression.
    ?_assertMatch(
      {source, _, [
        {expand, _, {ident, _, log}, [{str_lit, _, <<"Hello, World!">>}]}
      ]},
      parse(<<"log \"Hello, World!\"">>)
    ),
    % Single block.
    ?_assertMatch(
      {source, _, [
        {expand, _, {ident, _, unsafe}, [{block, _, []}]}
      ]},
      parse(<<"unsafe { }">>)
    ),
    % Identifier and a block.
    ?_assertMatch(
      {source, _, [
        {expand, _, {ident, _, mod}, [{ident, _, empty}, {block, _, []}]}
      ]},
      parse(<<"mod empty { }">>)
    ),
    % Identifier and a block (explicit comma).
    ?_assertMatch(
      {source, _, [
        {expand, _, {ident, _, mod}, [{ident, _, empty}, {block, _, []}]}
      ]},
      parse(<<"mod empty, { }">>)
    ),
    % Two macros calls.
    ?_assertMatch(
      {source, _, [
        {expand, _, {ident, _, mod}, [{ident, _, empty}, {block, _, []}]},
        {expand, _, {ident, _, mod}, [{ident, _, another_empty}, {block, _, []}]}
      ]},
      parse(<<"
        mod empty { }
        mod another_empty { }
      ">>)
    ),
    % Three arguments.
    ?_assertMatch(
      {source, _, [
        {expand, _, {ident, _, a}, [{ident, _, b}, {ident, _, c}, {ident, _, d}]}
      ]},
      parse(<<"a b, c, d">>)
    )
  ].

block_test_() ->
  [
    % Block with three expressions.
    ?_assertMatch(
      {source, _, [
        {block, _, [{ident, _, one}, {ident, _, two}, {ident, _, three}]}
      ]},
      parse(<<"
        {
          one
          two; three
        }
      ">>)
    )
  ].
