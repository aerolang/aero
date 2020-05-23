defmodule Aero.ParserTest do
  use Aero.ParserCase

  parser_test "empty source",
    tokens: [
      {:eof, 0}
    ],
    ast: {:source, [], []}

  parser_test "only a newline",
    tokens: [
      {:newline, 1},
      {:eof, 0}
    ],
    ast: {:source, [], []}

  parser_test "hello world",
    # source:
    #   log "Hello, World!"
    tokens: [
      {:ident, 1, :log},
      {:string_lit, 1, "Hello, World!"},
      {:newline, 1},
      {:eof, 0}
    ],
    ast: {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :log},
        args: [
          {:pos_arg, [line: 1], {:string_lit, [line: 1], "Hello, World!"}}
        ]
      ]}
    ]}

  parser_test "macro with a block",
    # source:
    #   z { }
    tokens: [
      {:ident, 1, :z},
      {:"{", 1},
      {:"}", 1},
      {:newline, 1},
      {:eof, 0}
    ],
    ast: {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :z},
        args: [
          {:pos_arg, [line: 1], {:block, [line: 1], []}}
        ]
      ]}
    ]}

  parser_test "block with three expressions",
    # source:
    #   {
    #     one
    #     two; three
    #   }
    tokens: [
      {:"{", 1},
      {:newline, 1},
      {:ident, 2, :one},
      {:newline, 2},
      {:ident, 3, :two},
      {:newline, 3},
      {:ident, 3, :three},
      {:newline, 3},
      {:"}", 4},
      {:newline, 4},
      {:eof, 0}
    ],
    ast: {:source, [], [
      {:block, [line: 1], [
        {:ident, [line: 2], :one},
        {:ident, [line: 3], :two},
        {:ident, [line: 3], :three}
      ]}
    ]}

  parser_test "empty module",
    # source:
    #   mod empty { }
    tokens: [
      {:ident, 1, :mod},
      {:ident, 1, :empty},
      {:"{", 1},
      {:"}", 1},
      {:newline, 1},
      {:eof, 0}
    ],
    ast: {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :mod},
        args: [
          {:pos_arg, [line: 1], {:ident, [line: 1], :empty}},
          {:pos_arg, [line: 1], {:block, [line: 1], []}}
        ]
      ]}
    ]}

  parser_test "two modules",
    # source:
    #   mod empty { }
    #   mod another_empty { }
    tokens: [
      {:ident, 1, :mod},
      {:ident, 1, :empty},
      {:"{", 1},
      {:"}", 1},
      {:newline, 1},
      {:ident, 2, :mod},
      {:ident, 2, :another_empty},
      {:"{", 2},
      {:"}", 2},
      {:newline, 2},
      {:eof, 0}
    ],
    ast: {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :mod},
        args: [
          {:pos_arg, [line: 1], {:ident, [line: 1], :empty}},
          {:pos_arg, [line: 1], {:block, [line: 1], []}}
        ]
      ]},
      {:macro_call, [line: 2], [
        name: {:ident, [line: 2], :mod},
        args: [
          {:pos_arg, [line: 2], {:ident, [line: 2], :another_empty}},
          {:pos_arg, [line: 2], {:block, [line: 2], []}}
        ]
      ]}
    ]}

  parser_test "empty module with comma",
    # source:
    #   mod empty, { }
    tokens: [
      {:ident, 1, :mod},
      {:ident, 1, :empty},
      {:",", 1},
      {:"{", 1},
      {:"}", 1},
      {:newline, 1},
      {:eof, 0}
    ],
    ast: {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :mod},
        args: [
          {:pos_arg, [line: 1], {:ident, [line: 1], :empty}},
          {:pos_arg, [line: 1], {:block, [line: 1], []}}
        ]
      ]}
    ]}

  parser_test "three argument macro",
    # source:
    #   a b, c, d
    tokens: [
      {:ident, 1, :a},
      {:ident, 1, :b},
      {:",", 1},
      {:ident, 1, :c},
      {:",", 1},
      {:ident, 1, :d},
      {:newline, 1},
      {:eof, 0}
    ],
    ast: {:source, [], [
      {:macro_call, [line: 1], [
        name: {:ident, [line: 1], :a},
        args: [
          {:pos_arg, [line: 1], {:ident, [line: 1], :b}},
          {:pos_arg, [line: 1], {:ident, [line: 1], :c}},
          {:pos_arg, [line: 1], {:ident, [line: 1], :d}}
        ]
      ]}
    ]}
end
