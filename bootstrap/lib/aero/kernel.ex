defmodule Aero.Kernel do
  @kernel (
    quote do
      defmodule :_aero_kernel do
        defmacro mod(module, {:__block__, _, _} = block) do
          Module.create module, block, Macro.Env.location(__ENV__)
        end

        defmacro log(content) do
          quote do
            IO.puts(unquote(content))
          end
        end

        defmacro if(expr, then) do
          quote do
            case unquote(expr) do
              :true -> {:some, unquote(then)}
              :false -> :none
            end
          end
        end

        defmacro unquote(:=)(pat, expr) do
          quote do
            var!(unquote(pat)) = unquote(expr)
          end
        end

        defmacro true_ do
          quote do
            :true
          end
        end

        defmacro false_ do
          quote do
            :false
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
