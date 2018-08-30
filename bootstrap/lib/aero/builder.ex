defmodule Aero.Builder do
  def build(ast) do
    ast
    |> Aero.Builder.Transform.transform()
    |> Aero.Builder.Kernel.add_kernel()
    |> eval()
  end

  # Evaluate transformed AST to produce a module.
  defp eval(elixir_ast) do
    Code.eval_quoted elixir_ast, []
  end
end
