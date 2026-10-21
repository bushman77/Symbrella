defmodule Core.Response.CodeRequestValidation do
  @moduledoc """
  Conservative source check for the basic GenServer answer contract.

  Parses the proposed source as Elixir syntax; it does not evaluate,
  compile, execute, persist, or otherwise trust the model's code.
  This is not general Elixir verification: it accepts only a tiny
  GenServer skeleton with start_link/1 and init/1, where the
  initial state is passed through unchanged. More elaborate handlers
  are not accepted without their own validation contract.
  """

  @max_source_bytes 4_096
  @fence ~r/\x60\x60\x60(?:elixir)?[ \t]*\r?\n(.*?)\r?\n\x60\x60\x60/su
  @direct_callback ~r/\.\s*handle_(?:info|cast|call)\s*\(/u

  @spec complete?(String.t(), String.t()) :: boolean()
  def complete?(module_name, draft)
      when is_binary(module_name) and is_binary(draft) do
    with false <- Regex.match?(@direct_callback, draft),
         {:ok, source} <- extract_source(draft),
         {:ok, tree} <- Code.string_to_quoted(source),
         true <- valid_module?(tree, module_name) do
      true
    else
      _ -> false
    end
  end

  def complete?(_, _), do: false

  defp extract_source(text) do
    case Regex.scan(@fence, text, capture: :all_but_first) do
      [[source]] ->
        source = String.trim(source)
        if byte_size(source) in 1..@max_source_bytes,
          do: {:ok, source}, else: :error

      [] ->
        source = String.trim(text)
        if String.starts_with?(source, "defmodule ") and
             byte_size(source) in 1..@max_source_bytes,
          do: {:ok, source}, else: :error

      _ ->
        :error
    end
  end

  defp valid_module?(
         {:defmodule, _, [{:__aliases__, _, [module_atom]}, [do: body]]},
         module_name
       ) when is_atom(module_atom) do
    Atom.to_string(module_atom) == module_name and
      validate_body(body)
  end

  defp valid_module?(_, _), do: false

  defp validate_body({:__block__, _, forms}) when is_list(forms),
    do: validate_forms(forms)
  defp validate_body(form), do: validate_forms([form])

  defp validate_forms(forms) do
    Enum.count(forms, &use_genserver?/1) == 1 and
      Enum.count(forms, &start_link?/1) == 1 and
      Enum.count(forms, &init?/1) == 1 and
      Enum.all?(forms, fn form ->
        use_genserver?(form) or start_link?(form) or init?(form) or
          impl_attribute?(form)
      end)
  end

  defp use_genserver?({:use, _, [{:__aliases__, _, [:GenServer]}]}),
    do: true
  defp use_genserver?(_), do: false

  defp impl_attribute?({:@, _, [{:impl, _, [true]}]}), do: true
  defp impl_attribute?(_), do: false

  defp start_link?({:def, _, [{:start_link, _, [arg]}, [do: body]]}) do
    with {:ok, variable} <- argument_name(arg),
         true <- starts_same_state?(body, variable) do
      true
    else
      _ -> false
    end
  end

  defp start_link?(_), do: false

  defp argument_name({:\\, _, [argument, {:%{}, _, []}]}),
    do: bare_variable(argument)
  defp argument_name(argument), do: bare_variable(argument)

  defp bare_variable({name, _, context})
       when is_atom(name) and (is_atom(context) or is_nil(context)),
       do: {:ok, name}
  defp bare_variable(_), do: :error

  defp starts_same_state?(
         {{:., _, [{:__aliases__, _, [:GenServer]}, :start_link]}, _,
          [module_ast, state_ast]}, variable
       ) do
    module_reference?(module_ast) and variable_reference?(state_ast, variable)
  end

  defp starts_same_state?(
         {{:., _, [{:__aliases__, _, [:GenServer]}, :start_link]}, _,
          [module_ast, state_ast, [name: name_ast]]}, variable
       ) do
    module_reference?(module_ast) and
      variable_reference?(state_ast, variable) and
      module_reference?(name_ast)
  end

  defp starts_same_state?(_, _), do: false

  defp module_reference?({:__MODULE__, _, _}), do: true
  defp module_reference?(_), do: false

  defp variable_reference?({name, _, context}, name)
       when is_atom(name) and (is_atom(context) or is_nil(context)),
       do: true
  defp variable_reference?(_, _), do: false

  defp init?({:def, _, [{:init, _, [argument]}, [do: {:ok, returned}]]}) do
    with {:ok, variable} <- bare_variable(argument),
         true <- variable_reference?(returned, variable) do
      true
    else
      _ -> false
    end
  end

  defp init?(_), do: false
end
