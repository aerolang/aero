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

  @doc "If expression macro."
  defmacro if(expr, then) do
    case aero_expand(then) do
      {:_else_, [left, right]} ->
        # When an else block is provided return either case.
        quote do
          case unquote(expr) do
            true -> unquote(left)
            false -> unquote(right)
          end
        end
      _ ->
        # Otherwise, wrap into an optional type.
        quote do
          case unquote(expr) do
            true -> {:some, unquote(then)}
            false -> :none
          end
        end
    end
  end

  @doc "Cond expression macro."
  defmacro cond(cases) do
    ex_cases =
      aero_block(cases)
      |> Enum.flat_map(fn case_ ->
        {:"_->_", [left, right]} = aero_expand(case_)
        [left_arg] = aero_args(left)

        # Elixir allows truthy values, enforcing only `true`.
        quote do
          unquote(left_arg) === true -> unquote(right)
        end
      end)

    quote do
      cond do
        unquote(ex_cases)
      end
    end
  end

  @doc "Match expression macro."
  defmacro match(expr, cases) do
    ex_cases =
      aero_block(cases)
      |> Enum.flat_map(fn case_ ->
        {:"_->_", [left, right]} = aero_expand(case_)

        # When more than one arg is passed, turn it into a tuple.
        left_arg =
          case aero_args(left) do
            [arg] -> arg
            args -> quote do: {unquote_splicing(args)}
          end

        quote do
          unquote(left_arg) -> unquote(right)
        end
      end)

    quote do
      case unquote(expr) do
        unquote(ex_cases)
      end
    end
  end

  @doc "Construct a tuple."
  defmacro unquote(:"(_)")(exprs) do
    args = aero_args(exprs)
    quote do: {unquote_splicing(args)}
  end

  @doc "Construct an array."
  defmacro unquote(:"[_]")(exprs) do
    args = aero_args(exprs)
    quote do: :array.from_list(unquote(args))
  end

  @doc "Construct a dict."
  defmacro unquote(:"\\\\\\\#{_}")(exprs) do
    # Backslashes get duplicated through the unquoting process.
    args =
      for arg <- aero_args(exprs) do
        case aero_expand(arg) do
          {:"_=>_", [left, right]} -> {left, right}
          {:__tag__, [left, right]} -> {aero_ident(left), right}
        end
      end

    quote do: %{unquote_splicing(args)}
  end

  @doc "Construct a struct."
  defmacro unquote(:"#(_)")(exprs) do
    # Structs are just maps with atom keys only.
    args =
      for arg <- aero_args(exprs) do
        {:__tag__, [left, right]} = aero_expand(arg)
        {aero_ident(left), right}
      end

    quote do: %{unquote_splicing(args)}
  end

  @doc "Construct a bitstring."
  defmacro unquote(:"<<_>>")(exprs) do
    args =
      for arg <- aero_args(exprs) do
        case aero_expand(arg) do
          {:__tag__, [left, _right]} ->
            # TODO: Need to translate tags into the corresponding syntax in Elixir.
            left
          _ ->
            arg
        end
      end

    quote do: <<unquote_splicing(args)>>
  end

  @doc "Bind the left pattern to the right."
  defmacro unquote(:"_=_")(left, right) do
    # Using the Elixir context does the very unhygienic thing allowing access
    # to and setting variables in the caller's scope.
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
  defmacro unquote(:"_+_")(left, right) do
    quote do: Kernel.+(unquote(left), unquote(right))
  end

  @doc "Subtract the right from the left."
  defmacro unquote(:"_-_")(left, right) do
    quote do: Kernel.-(unquote(left), unquote(right))
  end

  @doc "Multiply two numbers."
  defmacro unquote(:"_*_")(left, right) do
    quote do: Kernel.*(unquote(left), unquote(right))
  end

  @doc "Divide the left by the right, two ints give another int."
  defmacro unquote(:"_/_")(left, right) do
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
  defmacro unquote(:"_%_")(left, right) do
    quote do: Kernel.rem(unquote(left), unquote(right))
  end

  @doc "Concatenate two lists or binaries."
  defmacro unquote(:"_++_")(left, right) do
    quote do
      Kernel.if is_binary(unquote(left)) and is_binary(unquote(right)) do
        Kernel.<>(unquote(left), unquote(right))
      else
        Kernel.++(unquote(left), unquote(right))
      end
    end
  end

  @doc "Cons operator."
  defmacro unquote(:"_::_")(left, right) do
    quote do: [unquote(left) | unquote(right)]
  end

  @doc "Less-than operator."
  defmacro unquote(:"_<_")(left, right) do
    quote do: Kernel.<(unquote(left), unquote(right))
  end

  @doc "More-than operator."
  defmacro unquote(:"_>_")(left, right) do
    quote do: Kernel.>(unquote(left), unquote(right))
  end

  @doc "Check if left is less than or equal to the right."
  defmacro unquote(:"_<=_")(left, right) do
    quote do: Kernel.<=(unquote(left), unquote(right))
  end

  @doc "Check if left is greater than or equal to the right."
  defmacro unquote(:"_>=_")(left, right) do
    quote do: Kernel.>=(unquote(left), unquote(right))
  end

  @doc "Check if the left is equal to the right."
  defmacro unquote(:"_==_")(left, right) do
    quote do: Kernel.===(unquote(left), unquote(right))
  end

  @doc "Check if the left is not equal to the right."
  defmacro unquote(:"_!=_")(left, right) do
    quote do: Kernel.!==(unquote(left), unquote(right))
  end

  @doc "Boolean not."
  defmacro unquote(:"!_")(value) do
    quote do: Kernel.not(unquote(value))
  end

  @doc "Boolean and."
  defmacro unquote(:"_&&_")(left, right) do
    quote do: Kernel.and(unquote(left), unquote(right))
  end

  @doc "Boolean or."
  defmacro unquote(:"_||_")(left, right) do
    quote do: Kernel.or(unquote(left), unquote(right))
  end

  @doc "Truthy boolean value."
  defmacro true_, do: true

  @doc "Falsy boolean value."
  defmacro false_, do: false

  @doc "Empty list."
  defmacro nil_, do: []

  defp aero_ident(ast) do
    case ast do
      {ident, _, context} when is_atom(context) -> ident
      _ -> nil
    end
  end

  defp aero_block(ast) do
    case ast do
      {:__block__, _, exprs} -> exprs
      other -> [other]
    end
  end

  defp aero_expand(ast) do
    case ast do
      {{:., _, [{:__aliases__, _, [:Aero, :Kernel]}, macro]}, _, args} -> {macro, args}
      _ -> nil
    end
  end

  defp aero_args(ast) do
    case aero_expand(ast) do
      {:__args__, args} -> args
      _ -> nil
    end
  end
end
