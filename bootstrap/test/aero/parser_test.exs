defmodule Aero.ParserTest do
  use ExUnit.Case

  test "no tokens" do
    tokens = []

    {:ok, ast} = Aero.Parser.parse tokens

    assert ast === {:source, [], []}
  end

  test "whitespace" do
    tokens = [{:space, 1}]

    {:ok, ast} = Aero.Parser.parse tokens

    assert ast === {:source, [], []}
  end

  test "hello world" do
    tokens = [
      {:ident, 1, :log},
      {:space, 1},
      {:string_lit, 1, "Hello, world!"},
      {:newline, 1}
    ]

    {:ok, ast} = Aero.Parser.parse tokens

    assert ast === {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :log},
        args: [
          {:pos_arg, [line: 1], {:string_lit, [line: 1], "Hello, world!"}}
        ]
      ]}
    ]}
  end
end
