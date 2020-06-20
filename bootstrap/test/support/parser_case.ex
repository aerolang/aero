defmodule Aero.ParserCase do
  use ExUnit.CaseTemplate

  @doc """
  Create an Aero parser test case with source transforming to an ast.
  """
  defmacro parser_test(message, [source: source, ast: ast]) do
    quote do
      test unquote(message) do
        expected = unquote(ast)
        {:ok, tokens, _} = :aero_lexer.tokenize(unquote(source))
        assert {:ok, output} = :aero_parser.parse(tokens)
        assert output == expected
      end
    end
  end

  using do
    quote do
      import Aero.ParserCase, only: [parser_test: 2]
    end
  end
end
