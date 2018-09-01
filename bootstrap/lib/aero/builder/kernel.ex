defmodule Aero.Builder.Kernel do
  @kernel (
    quote do
      defmodule :_aero_kernel do
        defmodule :atom_t do
          @enforce_keys [:value]
          defstruct [:value]
        end

        defmodule :string_t do
          @enforce_keys [:value]
          defstruct [:value]
        end

        defmacro mod(module, {:__block__, _, _} = block) do
          Module.create module.value, block, Macro.Env.location(__ENV__)
        end

        defmacro log(content) do
          quote do
            IO.puts(unquote(content).value)
          end
        end
      end
    end
  )

  @kernel_require (
    quote do
      require :_aero_kernel
    end
  )

  def add_kernel(elixir_ast) do
    {macro, meta, [name, [do: {:__block__, [], expr_list}]]} = elixir_ast

    {:__block__, [], [
      @kernel,
      {macro, meta, [
        name, [do: {:__block__, [], [@kernel_require] ++ expr_list}]
      ]}
    ]}
  end
end
