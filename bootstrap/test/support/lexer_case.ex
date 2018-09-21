defmodule Aero.LexerCase do
  use ExUnit.CaseTemplate

  @doc """
  Create an Aero lexer test case with source transforming to tokens.
  """
  defmacro lexer_test(message, [source: source, lines: lines,
      tokens: tokens]) do
    source = Macro.escape source
    lines = Macro.escape lines
    tokens = Macro.escape tokens

    quote bind_quoted: [message: message, source: source, lines: lines,
        tokens: tokens] do
      test message do
        input = unquote(source)
        output = unquote(tokens)
        count = unquote(lines)
        {:ok, ^output, ^count} = Aero.Lexer.tokenize input
      end
    end
  end

  using do
    quote do
      import Aero.LexerCase, only: [lexer_test: 2]
    end
  end
end
