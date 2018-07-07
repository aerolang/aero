defmodule Aero.Parser do
  def parse(tokens) when is_list(tokens) do
    :aero_parser.parse tokens
  end
end
