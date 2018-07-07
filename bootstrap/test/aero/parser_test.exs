defmodule Aero.ParserTest do
  use ExUnit.Case

  test "no tokens" do
    tokens = []

    {:ok, ast} = Aero.Parser.parse tokens

    assert ast === {:source, [], []}
  end
end
