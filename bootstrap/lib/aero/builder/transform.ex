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

  def transform({:ident, _meta, name}) when is_atom(name) do
    {name, [], Elixir}
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

  # Functions to convert erlang types to Aero kernel type structs.
  defp atom_t(value), do: value |> aero_value(:atom_t)
  defp string_t(value), do: value |> aero_value(:string_t)

  # Convert a value to be a struct for an Aero type, e.g., :atom_t.
  defp aero_value(value, type), do: {:%, [], [type, {:%{}, [], value: value}]}

  # Get the value from inside an Aero type struct.
  defp elixir_value({:%, [], [_type, {:%{}, [], value: value}]}), do: value
end
