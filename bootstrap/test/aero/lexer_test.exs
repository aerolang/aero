defmodule Aero.LexerTest do
  use ExUnit.Case

  test "empty string" do
    source = ""

    {:ok, tokens, 1} = Aero.Lexer.tokenize source

    assert tokens === []
  end

  test "whitespace is classified as newlines and spaces" do
    source = "\r\n  t_1 t_2 \r t_3 \nt_4\r\nt_5 \n \n t_6 "

    {:ok, tokens, 6} = Aero.Lexer.tokenize source

    assert tokens === [
      {:newline, 1},
      {:ident, 2, :t_1},
      {:space, 2},
      {:ident, 2, :t_2},
      {:space, 2},
      {:ident, 2, :t_3},
      {:newline, 2},
      {:ident, 3, :t_4},
      {:newline, 3},
      {:ident, 4, :t_5},
      {:newline, 4},
      {:ident, 6, :t_6},
      {:space, 6}
    ]
  end

  test "semicolons act as newlines" do
    source = "t_1;t_2 ; t_3\n;t_4 t_5;;"

    {:ok, tokens, 2} = Aero.Lexer.tokenize source

    assert tokens === [
      {:ident, 1, :t_1},
      {:newline, 1},
      {:ident, 1, :t_2},
      {:newline, 1},
      {:ident, 1, :t_3},
      {:newline, 1},
      {:ident, 2, :t_4},
      {:space, 2},
      {:ident, 2, :t_5},
      {:newline, 2}
    ]
  end

  test "newlines are tokenized after comments" do
    source = "-- comment\nt_1-- comment "

    {:ok, tokens, 2} = Aero.Lexer.tokenize source

    assert tokens === [
      {:newline, 1},
      {:ident, 2, :t_1}
    ]
  end

  test "basic string" do
    source = "\"test\""

    {:ok, tokens, 1} = Aero.Lexer.tokenize source

    assert tokens === [
      {:string_lit, 1, "test"}
    ]
  end

  test "basic char" do
    source = "'a'"

    {:ok, tokens, 1} = Aero.Lexer.tokenize source

    assert tokens === [
      {:char_lit, 1, ?a}
    ]
  end

  test "identifiers" do
    source = "test Test __TEST__ test_1 Test1 __TEST_1__"

    {:ok, tokens, 1} = Aero.Lexer.tokenize source

    assert tokens === [
      {:ident, 1, :test},
      {:space, 1},
      {:ident, 1, :Test},
      {:space, 1},
      {:ident, 1, :__TEST__},
      {:space, 1},
      {:ident, 1, :test_1},
      {:space, 1},
      {:ident, 1, :Test1},
      {:space, 1},
      {:ident, 1, :__TEST_1__}
    ]
  end

  test "basic atoms" do
    source = ":test :test_1"

    {:ok, tokens, 1} = Aero.Lexer.tokenize source

    assert tokens === [
      {:atom_lit, 1, :test},
      {:space, 1},
      {:atom_lit, 1, :test_1}
    ]
  end

  test "escaped atoms" do
    source = ":\"Test\" :\"& ,#\""

    {:ok, tokens, 1} = Aero.Lexer.tokenize source

    assert tokens === [
      {:atom_lit, 1, :Test},
      {:space, 1},
      {:atom_lit, 1, :"& ,#"}
    ]
  end
end
