defmodule Aero.Builder.Transform do
  # Convert the Aero AST into Elixir AST at the source level.
  def transform({:source, [], expr_list}) do
    ast = transform({:block, [], expr_list})

    # Wrapping this in a big defmodule. We need this since we need to
    # be able to require the kernel which will be defined above.
    {:defmodule, [context: Elixir, import: Kernel], [:_source, [do: ast]]}
  end

  def transform({:macro_call, _meta, [name: name, args: args]})
  when is_list(args) do
    macro = name |> extract_ident() |> elixir_value()
    remote = {:., [], [:_aero_kernel, macro]}

    {remote, [], transform_macro_args(macro, args)}
  end

  def transform({:pos_arg, _meta, arg}) do
    transform(arg)
  end

  def transform({:ident, meta, name}) when is_atom(name) do
    # Convert idents that are keywords in Elixir.
    name =
      case name do
        :true -> :true_
        :false -> :false_
        :nil -> :nil_
        :when -> :when_
        :and -> :and_
        :or -> :or_
        :in -> :in_
        :fn -> :fn_
        :do -> :do_
        :end -> :end_
        :catch -> :catch_
        :rescue -> :rescue_
        :after -> :after_
        :else -> :else_
        :_ -> :__
      end

    # If this corresponds to a zero-argument macro, convert it,
    # otherwise this is just an identifier.
    if name in [:true_, :false_] do
        transform({:macro_call, meta, [name: {:ident, meta, name}, args: []]})
    else
        {name, [], Elixir}
    end
  end

  def transform({:atom_lit, _meta, atom}) when is_atom(atom) do
    atom_t(atom)
  end

  def transform({:string_lit, _meta, string}) when is_binary(string) do
    string_t(string)
  end

  def transform({:block, _meta, expr_list}) when is_list(expr_list) do
    {:__block__, [], expr_list |> Enum.map(&transform/1)}
  end

  defp extract_ident({:ident, _meta, name}) do
    atom_t(name)
  end

  defp extract_pos_arg({:pos_arg, _meta, arg}) do
    arg
  end

  defp transform_macro_args(:mod, [module, block]) do
    [
      module |> extract_pos_arg() |> extract_ident(),
      [do: block |> transform()]
    ]
  end

  defp transform_macro_args(:log, [string]) do
    [
      string |> transform()
    ]
  end

  defp transform_macro_args(:if, [expr, then]) do
    [
      expr |> transform(),
      [then: then |> transform()]
    ]
  end

  defp transform_macro_args(:true_, []), do: []
  defp transform_macro_args(:false_, []), do: []

  # Functions to convert erlang types to Aero kernel type structs.
  defp atom_t(value), do: value |> aero_value(:atom_t)
  defp string_t(value), do: value |> aero_value(:string_t)

  # Convert a value to be a struct for an Aero type, e.g., :atom_t.
  defp aero_value(value, type), do: {:%, [], [type, {:%{}, [], value: value}]}

  # Get the value from inside an Aero type struct.
  defp elixir_value({:%, [], [_type, {:%{}, [], value: value}]}), do: value
end
