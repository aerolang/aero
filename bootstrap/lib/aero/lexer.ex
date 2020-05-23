defmodule Aero.Lexer do
  def tokenize(source) when is_binary(source) do
    :aero_lexer.tokenize source
  end
end
