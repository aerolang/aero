defmodule Aero.Kernel do
  defmacro mod(name, body) do
    name =
      Kernel.if name === :__source__ do
        __CALLER__.file |> Path.basename(".ex") |> String.to_atom()
      else
        name
      end

    quote do
      defmodule unquote(name) do
        unquote(body)
      end
    end
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
