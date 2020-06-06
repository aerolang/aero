defmodule Aero.Kernel do
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

  defmacro unquote(:=)(left, right) do
    quote do
      var!(unquote(left)) = unquote(right)
    end
  end

  defmacro true_ do
    quote do
      true
    end
  end

  defmacro false_ do
    quote do
      false
    end
  end

  defmacro nil_ do
    quote do
      []
    end
  end
end
