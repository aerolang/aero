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
    name_t = name |> extract_ident()
    remote = {:., [], [:_aero_kernel, name_t]}

    {remote, [], transform_macro_args(name_t, args)}
  end

  def transform({:pos_arg, _meta, arg}) do
    transform(arg)
  end

  def transform({:ident, _meta, name}) when is_atom(name) do
    {name, [], Elixir}
  end

  def transform({:string_lit, _meta, string}) when is_binary(string) do
    string
  end

  def transform({:block, _meta, expr_list}) when is_list(expr_list) do
    {:__block__, [], expr_list |> Enum.map(&transform/1)}
  end

  defp extract_ident({:ident, _meta, name}) do
    name
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
end
