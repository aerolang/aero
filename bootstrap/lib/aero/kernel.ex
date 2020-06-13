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

  @doc "Bind the left pattern to the right."
  defmacro left = right do
    # Using the Elixir context does the very unhygienic thing allowing access
    # to and setting variables in the callee's scope.
    quote context: Elixir, do: unquote(left) = unquote(right)
  end

  @doc "Unary plus, returns the same number."
  defmacro unquote(:"+_")(value) do
    quote do: Kernel.+(unquote(value))
  end

  @doc "Unary minus, switches the sign on a number."
  defmacro unquote(:"-_")(value) do
    quote do: Kernel.-(unquote(value))
  end

  @doc "Add two numbers."
  defmacro left + right do
    quote do: Kernel.+(unquote(left), unquote(right))
  end

  @doc "Subtract the right from the left."
  defmacro left - right do
    quote do: Kernel.-(unquote(left), unquote(right))
  end

  @doc "Multiply two numbers."
  defmacro left * right do
    quote do: Kernel.*(unquote(left), unquote(right))
  end

  @doc "Divide the left by the right, two ints give another int."
  defmacro left / right do
    quote do
      Kernel.if is_integer(unquote(left)) and is_integer(unquote(right)) do
        # Floor division for integers.
        Kernel.div(unquote(left), unquote(right))
      else
        Kernel./(unquote(left), unquote(right))
      end
    end
  end

  @doc "Get the remainder when dividing two ints."
  defmacro unquote(:%)(left, right) do
    quote do: Kernel.rem(unquote(left), unquote(right))
  end

  @doc "Concatenate two lists or binaries."
  defmacro left ++ right do
    quote do
      Kernel.if is_binary(unquote(left)) and is_binary(unquote(right)) do
        Kernel.<>(unquote(left), unquote(right))
      else
        Kernel.++(unquote(left), unquote(right))
      end
    end
  end

  @doc "Less-than operator."
  defmacro left < right do
    quote do: Kernel.<(unquote(left), unquote(right))
  end

  @doc "More-than operator."
  defmacro left > right do
    quote do: Kernel.>(unquote(left), unquote(right))
  end

  @doc "Check if left is less than or equal to the right."
  defmacro left <= right do
    quote do: Kernel.<=(unquote(left), unquote(right))
  end

  @doc "Check if left is greater than or equal to the right."
  defmacro left >= right do
    quote do: Kernel.>=(unquote(left), unquote(right))
  end

  @doc "Check if the left is equal to the right."
  defmacro left == right do
    quote do: Kernel.===(unquote(left), unquote(right))
  end

  @doc "Check if the left is not equal to the right."
  defmacro left != right do
    quote do: Kernel.!==(unquote(left), unquote(right))
  end

  @doc "Boolean not."
  defmacro unquote(:"!_")(value) do
    quote do: Kernel.not(unquote(value))
  end

  @doc "Boolean and."
  defmacro left && right do
    quote do: Kernel.and(unquote(left), unquote(right))
  end

  @doc "Boolean or."
  defmacro left || right do
    quote do: Kernel.or(unquote(left), unquote(right))
  end

  @doc "Truthy boolean value."
  defmacro true_, do: true

  @doc "Falsy boolean value."
  defmacro false_, do: false

  @doc "Empty list."
  defmacro nil_, do: []
end
