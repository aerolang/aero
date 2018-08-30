defmodule Aero.BuilderTest do
  use ExUnit.Case

  test "empty source" do
    ast = {:source, [], []}

    Aero.Builder.build ast
  end

  test "build hello world example" do
    ast = {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :log},
        args: [
          {:pos_arg, [line: 1], {:string_lit, [line: 1], "Hello, world!"}}
        ]
      ]}
    ]}

    output = ExUnit.CaptureIO.capture_io fn ->
      Aero.Builder.build ast
    end

    assert output === "Hello, world!\n"
  end
end
