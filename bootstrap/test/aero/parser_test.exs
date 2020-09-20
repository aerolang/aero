defmodule Aero.ParserTest do
  use Aero.ParserCase

  parser_test "empty source",
    source: "",
    ast: {:source, [line: 1, column: 1, span: :aero_span.new(0, 0)], []}

  parser_test "only a newline",
    source: "\n",
    ast: {:source, [line: 1, column: 1, span: :aero_span.new(0, 1)], []}

  parser_test "hello world",
    source: "log \"Hello, World!\"",
    ast: {
      :source,
      [line: 1, column: 1, span: :aero_span.new(0, 19)],
      [
        {
          :expand,
          [line: 1, column: 1, span: :aero_span.new(0, 19)],
          {:ident, [line: 1, column: 1, span: :aero_span.new(0, 3)], :log},
          [
            {:string_lit, [line: 1, column: 5, span: :aero_span.new(4, 19)], "Hello, World!"}
          ]
        }
      ]
    }

  parser_test "macro with a block",
    source: "z { }",
    ast: {
      :source,
      [line: 1, column: 1, span: :aero_span.new(0, 5)],
      [
        {
          :expand,
          [line: 1, column: 1, span: :aero_span.new(0, 5)],
          {:ident, [line: 1, column: 1, span: :aero_span.new(0, 1)], :z},
          [
            {:block, [line: 1, column: 3, span: :aero_span.new(2, 5)], []}
          ]
        }
      ]
    }

  parser_test "block with three expressions",
    source: """
      {
        one
        two; three
      }
      """,
    ast: {
      :source,
      [line: 1, column: 1, span: :aero_span.new(0, 23)],
      [
        {
          :block,
          [line: 1, column: 1, span: :aero_span.new(0, 22)],
          [
            {:ident, [line: 2, column: 3, span: :aero_span.new(4, 7)], :one},
            {:ident, [line: 3, column: 3, span: :aero_span.new(10, 13)], :two},
            {:ident, [line: 3, column: 8, span: :aero_span.new(15, 20)], :three}
          ]
        }
      ]
    }

  parser_test "empty module",
    source: "mod empty { }",
    ast: {
      :source,
      [line: 1, column: 1, span: :aero_span.new(0, 13)],
      [
        {
          :expand,
          [line: 1, column: 1, span: :aero_span.new(0, 13)],
          {:ident, [line: 1, column: 1, span: :aero_span.new(0, 3)], :mod},
          [
            {:ident, [line: 1, column: 5, span: :aero_span.new(4, 9)], :empty},
            {:block, [line: 1, column: 11, span: :aero_span.new(10, 13)], []}
          ]
        }
      ]
    }

  parser_test "two modules",
    source: """
      mod empty { }
      mod another_empty { }
      """,
    ast: {
      :source,
      [line: 1, column: 1, span: :aero_span.new(0, 36)],
      [
        {
          :expand,
          [line: 1, column: 1, span: :aero_span.new(0, 13)],
          {:ident, [line: 1, column: 1, span: :aero_span.new(0, 3)], :mod},
          [
            {:ident, [line: 1, column: 5, span: :aero_span.new(4, 9)], :empty},
            {:block, [line: 1, column: 11, span: :aero_span.new(10, 13)], []}
          ]
        },
        {
          :expand,
          [line: 2, column: 1, span: :aero_span.new(14, 35)],
          {:ident, [line: 2, column: 1, span: :aero_span.new(14, 17)], :mod},
          [
            {:ident, [line: 2, column: 5, span: :aero_span.new(18, 31)], :another_empty},
            {:block, [line: 2, column: 19, span: :aero_span.new(32, 35)], []}
          ]
        },
      ]
    }

  parser_test "empty module with comma",
    source: "mod empty, { }",
    ast: {
      :source,
      [line: 1, column: 1, span: :aero_span.new(0, 14)],
      [
        {
          :expand,
          [line: 1, column: 1, span: :aero_span.new(0, 14)],
          {:ident, [line: 1, column: 1, span: :aero_span.new(0, 3)], :mod}, [
            {:ident, [line: 1, column: 5, span: :aero_span.new(4, 9)], :empty},
            {:block, [line: 1, column: 12, span: :aero_span.new(11, 14)], []}
          ]
        }
      ]
    }

  parser_test "three argument macro",
    source: "a b, c, d",
    ast: {
      :source,
      [line: 1, column: 1, span: :aero_span.new(0, 9)],
      [
        {
          :expand,
          [line: 1, column: 1, span: :aero_span.new(0, 9)],
          {:ident, [line: 1, column: 1, span: :aero_span.new(0, 1)], :a},
          [
            {:ident, [line: 1, column: 3, span: :aero_span.new(2, 3)], :b},
            {:ident, [line: 1, column: 6, span: :aero_span.new(5, 6)], :c},
            {:ident, [line: 1, column: 9, span: :aero_span.new(8, 9)], :d}
          ]
        }
      ]
    }
end
