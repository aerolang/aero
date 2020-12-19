defmodule Aero.ScannerCase do
  use ExUnit.CaseTemplate

  @doc """
  Create an Aero scanner test case with source transforming to tokens.
  """
  defmacro scanner_case(message, [source: source, tokens: tokens]) do
    quote do
      test unquote(message) do
        expected = unquote(tokens)
        assert {:ok, output} = :aero_scan.scan(unquote(source))
        assert output == expected
      end
    end
  end

  using do
    quote do
      import Aero.ScannerCase, only: [scanner_case: 2]
    end
  end
end
