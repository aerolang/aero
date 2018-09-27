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
    output = Aero.Builder.build ast
    {:ok, output}
  end
end
