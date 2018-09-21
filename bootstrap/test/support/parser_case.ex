defmodule Aero.ParserCase do
  use ExUnit.CaseTemplate

  @doc """
  Create an Aero parser test case with tokens transforming to an ast.
  """
  defmacro parser_test(message, [tokens: tokens, ast: ast]) do
    tokens = Macro.escape tokens
    ast = Macro.escape ast

    quote bind_quoted: [message: message, tokens: tokens, ast: ast] do
      test message do
        input = unquote(tokens)
        output = unquote(ast)
        {:ok, ^output} = Aero.Parser.parse input
      end
    end
  end

  using do
    quote do
      import Aero.ParserCase, only: [parser_test: 2]
    end
  end
end
