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

  test "test if macro with true and false" do
    # source:
    #   if true {
    #     log "is true"
    #   }
    #
    #   if false {
    #     log "is false"
    #   }
    ast = {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :if},
        args: [
          {:pos_arg, [line: 1], {:ident, [line: 1], :true}},
          {:pos_arg, [line: 1], {:macro_call, [line: 2], [
            name: {:ident, [line: 2], :log},
            args: [
              {:pos_arg, [line: 2], {:string_lit, [line: 2], "is true"}}
            ]
          ]}}
        ]
      ]},
      {:macro_call, [line: 5], [
        name: {:ident, [line: 5], :if},
        args: [
          {:pos_arg, [line: 5], {:ident, [line: 5], :false}},
          {:pos_arg, [line: 5], {:macro_call, [line: 6], [
            name: {:ident, [line: 6], :log},
            args: [
              {:pos_arg, [line: 6], {:string_lit, [line: 6], "is false"}}
            ]
          ]}}
        ]
      ]}
    ]}

    output = ExUnit.CaptureIO.capture_io fn ->
      Aero.Builder.build ast
    end

    assert output === "is true\n"
  end
end
