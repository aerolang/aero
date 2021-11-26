-module(aero_scan_tests).

-include_lib("eunit/include/eunit.hrl").

scan(Source) ->
  Result = aero_scan:scan(Source),
  ?assertMatch({ok, _}, Result),

  {ok, Tokens} = Result,
  Tokens.

empty_test() ->
  ?assertMatch([{eof, _}], scan(<<"">>)).

whitespace_test_() ->
  [
    % Whitespace is classified as newlines and spaces.
    ?_assertMatch(
      [
        {newline, _}, {ident, _, t_1}, {space, _}, {ident, _, t_2}, {space, _}, {ident, _, t_3},
        {newline, _}, {ident, _, t_4}, {newline, _}, {ident, _, t_5}, {newline, _},
        {ident, _, t_6}, {space, _}, {eof, _}
      ],
      scan(<<"\r\n  t_1 t_2\tt_3 \nt_4\r\nt_5 \n \n t_6 ">>)
    ),
    % Semicolons act as newlines.
    ?_assertMatch(
      [
        {ident, _, t_1}, {newline, _}, {ident, _, t_2}, {newline, _}, {ident, _, t_3},
        {newline, _}, {ident, _, t_4}, {space, _}, {ident, _, t_5}, {newline, _}, {eof, _}
      ],
      scan(<<"t_1;t_2 ; t_3\n;t_4 t_5;;">>)
    ),
    % Newlines are tokenized after comments.
    ?_assertMatch(
      [
        {newline, _}, {ident, _, t_1}, {space, _}, {eof, _}
      ],
      scan(<<"// comment\nt_1// comment ">>)
    )
  ].

string_test_() ->
  [
    % Basic string.
    ?_assertMatch([{str_lit, _, <<"test">>}, {eof, _}], scan(<<"\"test\"">>))
  ].

identifier_test() ->
  ?assertMatch([
    {ident, _, test}, {space, _}, {ident, _, 'Test'}, {space, _}, {ident, _, '__TEST__'},
    {space, _}, {ident, _, 'test_1'}, {space, _}, {ident, _, 'Test1'}, {space, _},
    {ident, _, '__TEST_1__'}, {eof, _}
  ], scan(<<"test Test __TEST__ test_1 Test1 __TEST_1__">>)).

symbol_test_() ->
  [
    % Basic symbols.
    ?_assertMatch([
      {sym_lit, _, test}, {space, _}, {sym_lit, _, test_1}, {eof, _}
    ], scan(<<":test :test_1">>)),
    % Escaped symbols.
    ?_assertMatch([
      {sym_lit, _, 'Test'}, {space, _}, {sym_lit, _, '& ,#'}, {eof, _}
    ], scan(<<":\"Test\" :\"& ,#\"">>))
  ].
