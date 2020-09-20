defmodule Aero.LexerTest do
  use Aero.LexerCase

  lexer_test "empty string",
    source: "",
    lines: 1,
    tokens: [
      {:eof, [line: 1, column: 1, span: :aero_span.new(0, 0)]}
    ]

  lexer_test "whitespace is classified as newlines and spaces",
    source: "\r\n  t_1 t_2\tt_3 \nt_4\r\nt_5 \n \n t_6 ",
    lines: 6,
    tokens: [
      {:newline, [line: 1, column: 1,  span: :aero_span.new(0,  4)]},
      {:ident,   [line: 2, column: 3,  span: :aero_span.new(4,  7)], :t_1},
      {:space,   [line: 2, column: 6,  span: :aero_span.new(7,  8)]},
      {:ident,   [line: 2, column: 7,  span: :aero_span.new(8,  11)], :t_2},
      {:space,   [line: 2, column: 10, span: :aero_span.new(11, 12)]},
      {:ident,   [line: 2, column: 12, span: :aero_span.new(12, 15)], :t_3},
      {:newline, [line: 2, column: 15, span: :aero_span.new(15, 17)]},
      {:ident,   [line: 3, column: 1,  span: :aero_span.new(17, 20)], :t_4},
      {:newline, [line: 3, column: 4,  span: :aero_span.new(20, 22)]},
      {:ident,   [line: 4, column: 1,  span: :aero_span.new(22, 25)], :t_5},
      {:newline, [line: 4, column: 4,  span: :aero_span.new(25, 30)]},
      {:ident,   [line: 6, column: 2,  span: :aero_span.new(30, 33)], :t_6},
      {:space,   [line: 6, column: 5,  span: :aero_span.new(33, 34)]},
      {:eof,     [line: 6, column: 6,  span: :aero_span.new(34, 34)]}
    ]

  lexer_test "semicolons act as newlines",
    source: "t_1;t_2 ; t_3\n;t_4 t_5;;",
    lines: 2,
    tokens: [
      {:ident,   [line: 1, column: 1,  span: :aero_span.new(0,  3)], :t_1},
      {:newline, [line: 1, column: 4,  span: :aero_span.new(3,  4)]},
      {:ident,   [line: 1, column: 5,  span: :aero_span.new(4,  7)], :t_2},
      {:newline, [line: 1, column: 8,  span: :aero_span.new(7,  10)]},
      {:ident,   [line: 1, column: 11, span: :aero_span.new(10, 13)], :t_3},
      {:newline, [line: 1, column: 14, span: :aero_span.new(13, 15)]},
      {:ident,   [line: 2, column: 2,  span: :aero_span.new(15, 18)], :t_4},
      {:space,   [line: 2, column: 5,  span: :aero_span.new(18, 19)]},
      {:ident,   [line: 2, column: 6,  span: :aero_span.new(19, 22)], :t_5},
      {:newline, [line: 2, column: 9,  span: :aero_span.new(22, 24)]},
      {:eof,     [line: 2, column: 11, span: :aero_span.new(24, 24)]}
    ]

  lexer_test "newlines are tokenized after comments",
    source: "// comment\nt_1// comment ",
    lines: 2,
    tokens: [
      {:newline, [line: 1, column: 1,  span: :aero_span.new(0,  11)]},
      {:ident,   [line: 2, column: 1,  span: :aero_span.new(11, 14)], :t_1},
      {:space,   [line: 2, column: 4,  span: :aero_span.new(14, 25)]},
      {:eof,     [line: 2, column: 15, span: :aero_span.new(25, 25)]}
    ]

  lexer_test "basic string",
    source: "\"test\"",
    lines: 1,
    tokens: [
      {:string_lit, [line: 1, column: 1, span: :aero_span.new(0, 6)], "test"},
      {:eof,        [line: 1, column: 7, span: :aero_span.new(6, 6)]}
    ]

  lexer_test "identifiers",
    source: "test Test __TEST__ test_1 Test1 __TEST_1__",
    lines: 1,
    tokens: [
      {:ident, [line: 1, column: 1,  span: :aero_span.new(0,  4)], :test},
      {:space, [line: 1, column: 5,  span: :aero_span.new(4,  5)]},
      {:ident, [line: 1, column: 6,  span: :aero_span.new(5,  9)], :Test},
      {:space, [line: 1, column: 10, span: :aero_span.new(9,  10)]},
      {:ident, [line: 1, column: 11, span: :aero_span.new(10, 18)], :__TEST__},
      {:space, [line: 1, column: 19, span: :aero_span.new(18, 19)]},
      {:ident, [line: 1, column: 20, span: :aero_span.new(19, 25)], :test_1},
      {:space, [line: 1, column: 26, span: :aero_span.new(25, 26)]},
      {:ident, [line: 1, column: 27, span: :aero_span.new(26, 31)], :Test1},
      {:space, [line: 1, column: 32, span: :aero_span.new(31, 32)]},
      {:ident, [line: 1, column: 33, span: :aero_span.new(32, 42)], :__TEST_1__},
      {:eof,   [line: 1, column: 43, span: :aero_span.new(42, 42)]}
    ]

  lexer_test "basic atoms",
    source: ":test :test_1",
    lines: 1,
    tokens: [
      {:atom_lit, [line: 1, column: 1,  span: :aero_span.new(0,  5)], :test},
      {:space,    [line: 1, column: 6,  span: :aero_span.new(5,  6)]},
      {:atom_lit, [line: 1, column: 7,  span: :aero_span.new(6,  13)], :test_1},
      {:eof,      [line: 1, column: 14, span: :aero_span.new(13, 13)]}
    ]

  lexer_test "escaped atoms",
    source: ":\"Test\" :\"& ,#\"",
    lines: 1,
    tokens: [
      {:atom_lit, [line: 1, column: 1,  span: :aero_span.new(0,  7)], :Test},
      {:space,    [line: 1, column: 8,  span: :aero_span.new(7,  8)]},
      {:atom_lit, [line: 1, column: 9,  span: :aero_span.new(8,  15)], :"& ,#"},
      {:eof,      [line: 1, column: 16, span: :aero_span.new(15, 15)]}
    ]
end
