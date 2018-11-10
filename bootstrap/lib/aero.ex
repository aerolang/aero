defmodule Aero do
  @moduledoc """
  Bootstrap compiler for the Aero programming language.
  """

  @doc """
  Compile source from a file.
  """
  def compile(filename) when is_binary(filename) do
    {:ok, content} = File.read filename
    {:ok, tokens, _} = Aero.Lexer.tokenize content
    {:ok, ast} = Aero.Parser.parse tokens

    output =
      ast
      |> Aero.Transform.transform()
      |> Aero.Kernel.add_kernel()
      |> Macro.to_string()
    {:ok, output}
  end
end
