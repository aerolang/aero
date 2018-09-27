defmodule Aero.CLI do
  def main(args \\ []) do
    filename = Enum.at args, 0
    Aero.compile filename
  end
end
