defmodule Aero.ParserTest do
  use Aero.ParserCase

  parser_test "empty source",
    source: "",
    ast: {:source, [], []}

  parser_test "only a newline",
    source: "\n",
    ast: {:source, [], []}

  parser_test "hello world",
    source: "log \"Hello, World!\"",
    ast: {:source, [], [
      {:expand, [line: 1], {:ident, [line: 1], :log}, [
        {:string_lit, [line: 1], "Hello, World!"}
      ]}
    ]}

  parser_test "macro with a block",
    source: "z { }",
    ast: {:source, [], [
      {:expand, [line: 1], {:ident, [line: 1], :z}, [
        {:block, [line: 1], []}
      ]}
    ]}

  parser_test "block with three expressions",
    source: """
      {
        one
        two; three
      }
    """,
    ast: {:source, [], [
      {:block, [line: 1], [
        {:ident, [line: 2], :one},
        {:ident, [line: 3], :two},
        {:ident, [line: 3], :three}
      ]}
    ]}

  parser_test "empty module",
    source: "mod empty { }",
    ast: {:source, [], [
      {:expand, [line: 1], {:ident, [line: 1], :mod}, [
        {:ident, [line: 1], :empty},
        {:block, [line: 1], []}
      ]}
    ]}

  parser_test "two modules",
    source: """
      mod empty { }
      mod another_empty { }
    """,
    ast: {:source, [], [
      {:expand, [line: 1], {:ident, [line: 1], :mod}, [
        {:ident, [line: 1], :empty},
        {:block, [line: 1], []}
      ]},
      {:expand, [line: 2], {:ident, [line: 2], :mod}, [
        {:ident, [line: 2], :another_empty},
        {:block, [line: 2], []}
      ]}
    ]}

  parser_test "empty module with comma",
    source: "mod empty, { }",
    ast: {:source, [], [
      {:expand, [line: 1], {:ident, [line: 1], :mod}, [
        {:ident, [line: 1], :empty},
        {:block, [line: 1], []}
      ]}
    ]}

  parser_test "three argument macro",
    source: "a b, c, d",
    ast: {:source, [], [
      {:expand, [line: 1], {:ident, [line: 1], :a}, [
        {:ident, [line: 1], :b},
        {:ident, [line: 1], :c},
        {:ident, [line: 1], :d}
      ]}
    ]}
end
