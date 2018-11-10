defmodule Aero.CLI do
  @options [
    output: :string
  ]

  @aliases [
    o: :output
  ]

  def main(args \\ []) do
    {parsed, argv} =
      OptionParser.parse! args, strict: @options, aliases: @aliases

    [input] = argv
    output = Keyword.fetch! parsed, :output

    {:ok, elixir_src} = Aero.compile input

    File.write! output, elixir_src
  end
end
