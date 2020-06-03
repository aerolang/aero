defmodule Aero.Transform do
  # Convert the Aero AST into Elixir AST at the source level.
  def transform({:source, [], expr_list}) do
    ast = transform({:block, [], expr_list})

    # Wrapping this in a big defmodule. We need this since we need to
    # be able to require the kernel which will be defined above.
    {:defmodule, [context: Elixir, import: Kernel], [:_source, [do: ast]]}
  end

  def transform({:expand, _meta, macro, args}) do
    callee =
      case macro do
        {:ident, _, _} = ident -> transform(ident)
        {:op, _, op_name} -> op_name |> to_string()
      end

    {
      {:., [], [:_aero_kernel, callee]},
      [],
      args |> Enum.map(&transform/1)
    }
  end

  # Zero-argument macros.
  def transform({:ident, _meta, ident}) when ident in [:true, :false] do
    {
      {:., [], [:_aero_kernel, safe_ident(ident)]},
      [],
      []
    }
  end

  def transform({:ident, _meta, ident}) do
    {safe_ident(ident), [], Elixir}
  end

  def transform({:atom_lit, _meta, atom}) do
    atom
  end

  def transform({:string_lit, _meta, string}) do
    string
  end

  def transform({:block, _meta, exprs}) do
    {:__block__, [], exprs |> Enum.map(&transform/1)}
  end

  # Convert idents that are keywords in Elixir.
  defp safe_ident(:true), do: :true_
  defp safe_ident(:false), do: :false_
  defp safe_ident(:nil), do: :nil_
  defp safe_ident(:when), do: :when_
  defp safe_ident(:and), do: :and_
  defp safe_ident(:or), do: :or_
  defp safe_ident(:in), do: :in_
  defp safe_ident(:fn), do: :fn_
  defp safe_ident(:do), do: :do_
  defp safe_ident(:end), do: :end_
  defp safe_ident(:catch), do: :catch_
  defp safe_ident(:rescue), do: :rescue_
  defp safe_ident(:after), do: :after_
  defp safe_ident(:else), do: :else_
  defp safe_ident(:_), do: :__
  defp safe_ident(other), do: other
end
