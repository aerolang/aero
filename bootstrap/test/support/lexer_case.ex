defmodule Aero.LexerCase do
  use ExUnit.CaseTemplate

  @doc """
  Create an Aero lexer test case with source transforming to tokens.
  """
  defmacro lexer_test(message, [source: source, lines: lines, tokens: tokens]) do
    quote do
      test unquote(message) do
        expected = unquote(tokens)
        assert {:ok, output, count} = :aero_lexer.tokenize(unquote(source))
        assert output == expected
        assert count == unquote(lines)
      end
    end
  end

  using do
    quote do
      import Aero.LexerCase, only: [lexer_test: 2]
    end
  end
end
