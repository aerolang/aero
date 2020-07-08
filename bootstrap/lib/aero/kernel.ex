defmodule Aero.Kernel do
  defmacro __source__(exprs) do
    quote do
      unquote_splicing(aero_args(exprs))
    end
  end

  defmacro mod(name, body) do
    define_mod(nil, name, body, __CALLER__)
  end

  defmacro func(head, body) do
    define_func(nil, head, body)
  end

  defmacro proc(head, body) do
    define_func(nil, head, body)
  end

  defmacro const(expr) do
    define_const(nil, expr)
  end

  defmacro pub(definition) do
    case aero_expand(definition) do
      {:mod, [name, body]} ->
        define_mod(:pub, name, body, __CALLER__)
      {:func, [head, body]} ->
        define_func(:pub, head, body)
      {:proc, [head, body]} ->
        define_func(:pub, head, body)
      {:const, [expr]} ->
        define_const(:pub, expr)
    end
  end

  defp define_mod(_vis, name, body, caller) do
    name = aero_ident(name)
    nested? = caller.module !== nil

    full_name =
      Kernel.if nested? do
        :"#{caller.module}.#{name}"
      else
        :"aero.#{name}"
      end

    quote do
      defmodule unquote(full_name) do
        unquote(body)
      end
    end
  end

  defp define_func(vis, head, body) do
    {:"_->_", [head_left_args, _head_right]} = aero_expand(head)
    [head_left] = aero_args(head_left_args)
    {:"_(_)", [name, head_args]} = aero_expand(head_left)

    ident = aero_ident(name)
    args =
      for arg <- aero_args(head_args) do
        case aero_expand(arg) do
          {:__tag__, [left, _right]} -> left
          _ -> arg
        end
      end

    case vis do
      :pub ->
        quote do
          def unquote(ident)(unquote_splicing(args)) do
            unquote(body)
          end
        end
      _ ->
        quote do
          defp unquote(ident)(unquote_splicing(args)) do
            unquote(body)
          end
        end
    end
  end

  defp define_const(vis, expr) do
    {:"_=_", [left, right]} = aero_expand(expr)
    IO.inspect aero_tag(left) |> elem(0)
    ident = aero_tag(left) |> elem(0) |> aero_ident()

    case vis do
      :pub ->
        quote do
          def unquote(ident)() do
            unquote(right)
          end
        end
      _ ->
        quote do
          defp unquote(ident)() do
            unquote(right)
          end
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
  defmacro cond_(cases) do
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

  @doc "If mixfix macro."
  defmacro _if_(left, right) do
    {:_else_, [middle, right]} = aero_expand(right)

    quote do
      case unquote(middle) do
        true -> unquote(left)
        false -> unquote(right)
      end
    end
  end

  @doc "For comprehension."
  defmacro _for_(left, right) do
    [for_comp, if_comp] = build_comp(right, [:_for_, :_if_])

    for_exprs =
      for_comp
      |> Enum.map(fn expr ->
        {:"_<-_", [e_left, e_right]} = aero_expand(expr)
        quote do: unquote(e_left) <- unquote(e_right)
      end)

    if_exprs =
      if_comp
      |> Enum.map(fn expr -> quote do: unquote(expr) === true end)

    quote do
      for unquote_splicing(for_exprs), unquote_splicing(if_exprs) do
        unquote(left)
      end
    end
  end

  @doc "Loop macro."
  defmacro loop(head, body) do
    [[state_comp], for_comp, while_comp, if_comp] =
      build_comp(head, [:_, :_for_, :_while_, :_if_])

    for_exprs = loop_generators(for_comp)
    break = loop_condition(while_comp)
    continue = loop_condition(if_comp)

    {initial, body} =
      case loop_state(state_comp) do
        nil ->
          {
            Macro.escape({}),
            quote do
              unquote(body)
              {}
            end
          }
        {state, initial} ->
          {
            initial,
            quote do
              unquote(state) = acc
              unquote(body)
            end
          }
      end

    quote do
      break_ref = make_ref()

      try do
        for unquote_splicing(for_exprs), reduce: unquote(initial) do
          acc ->
            cond do
              unquote(break) -> throw {break_ref, acc}
              unquote(continue) -> acc
              true -> unquote(body)
            end
        end
      catch
        {^break_ref, acc} -> acc
      end
    end
  end

  # Build comprehension with repeating infix macros.
  defp build_comp(expr, macros) do
    build_comp(expr, macros, List.duplicate([], length(macros)))
    |> Enum.map(&Enum.reverse/1)
  end

  defp build_comp(expr, macros, comp) do
    case aero_expand(expr) do
      {macro, [left, right]} ->
        Kernel.if macro in macros do
          {_, next_macros} =
            macros
            |> Enum.split_while(fn m -> m !== macro end)
          build_comp(right, next_macros, update_comp(left, -length(macros), comp))
        else
          update_comp(expr, -length(macros), comp)
        end
      _ ->
        update_comp(expr, -length(macros), comp)
    end
  end

  defp update_comp(expr, position, comp) do
    comp
    |> List.update_at(position, fn m_comp -> [expr | m_comp] end)
  end

  # Loop generator is infinite when no clauses with `for` macro.
  defp loop_generators(comp) do
    Kernel.if Enum.empty?(comp) do
      [quote do: _ <- Stream.cycle([nil])]
    else
      comp
      |> Enum.map(fn expr ->
        {:"_<-_", [left, right]} = aero_expand(expr)
        quote do: unquote(left) <- unquote(right)
      end)
    end
  end

  defp loop_condition(comp) do
    Kernel.if Enum.empty?(comp) do
      false
    else
      clauses =
        comp
        |> Enum.flat_map(fn expr -> quote do: (unquote(expr) === false -> true) end)
        |> Kernel.++(quote do: (true -> false))

      quote do
        cond do: unquote(clauses)
      end
    end
  end

  defp loop_state(state_comp) do
    cond do
      aero_ident(state_comp) === :__ -> nil
      true -> loop_state_pattern(state_comp)
    end
  end

  defp loop_state_pattern(pattern) do
    case {aero_ident(pattern), aero_expand(pattern)} do
      {ident, _} when ident !== nil ->
        # Without a default value, shadow an existing value of the same name.
        var = Macro.var(ident, nil)
        {var, var}

      {_, {:'_\\\\\\\\\\\\\\\\_', [left, right]}} ->
        # \\ operator.
        {safe_pattern(left), right}

      {_, {:"(_)", [exprs]}} ->
        args = aero_args(exprs) |> Enum.map(&loop_state_pattern/1)
        {patterns, initials} = args |> Enum.unzip()
        {
          (quote do: {unquote_splicing(patterns)}),
          (quote do: {unquote_splicing(initials)})
        }
    end
  end

  # A pattern which will always match with the correct type.
  defp safe_pattern(pattern) do
    case {aero_ident(pattern), aero_expand(pattern)} do
      {ident, _} when ident !== nil ->
        Macro.var(ident, nil)

      {_, {:"(_)", [exprs]}} ->
        args = aero_args(exprs) |> Enum.map(&safe_pattern/1)
        quote do: {unquote_splicing(args)}
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

  @doc "Create an anonymous function."
  defmacro unquote(:"_->_")(left, right) do
    args = aero_args(left)
    quote do: fn unquote_splicing(args) -> unquote(right) end
  end

  @doc "Function call (for both named and anonymous functions)."
  defmacro unquote(:"_(_)")(callee, exprs) do
    leftmost = leftmost_callee(callee)
    args = aero_args(exprs)

    cond do
      leftmost === nil or Macro.Env.has_var?(__CALLER__, {leftmost, __CALLER__.context}) ->
        quote do: unquote(callee).(unquote_splicing(args))
      true ->
        case aero_expand(callee) do
          {:"_._", [left, right]} ->
            left = left_module(left)
            right = aero_ident(right)
            quote do: unquote(:"aero.#{left}").unquote(right)(unquote_splicing((args)))
          _ ->
            quote do: unquote(:"aero.#{leftmost}")(unquote_splicing(args))
        end
    end
  end

  defp leftmost_callee(callee) do
    case {aero_ident(callee), aero_expand(callee)} do
      {ident, _} when ident !== nil ->
        ident
      {_, {:"_._", [left, _right]}} ->
        leftmost_callee(left)
      _ ->
        nil
    end
  end

  defp left_module(callee) do
    case {aero_ident(callee), aero_expand(callee)} do
      {ident, _} when ident !== nil ->
        ident
      {_, {:"_._", [left, right]}} ->
       :"#{left_module(left)}.#{left_module(right)}"
    end
  end

  defmacro unquote(:"_._")(left, right) do
    quote do: unquote(left).unquote(right)
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

  @doc "Range operator."
  defmacro unquote(:"_.._")(left, right) do
    quote do: Kernel.".."(unquote(left), unquote(right))
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
    # Aliases case covers idents starting with a capital letter which Elixir
    # thinks is a module.
    case ast do
      {ident, _, context} when is_atom(context) -> ident
      {:__aliases__, _, [ident]} when is_atom(ident) -> ident
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

  defp aero_tag(ast) do
    case aero_expand(ast) do
      {:__tag__, [left, right]} -> {left, right}
      _ -> nil
    end
  end
end
