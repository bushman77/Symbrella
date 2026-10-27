defmodule Core.Semantic.Composition.Expression do
  @moduledoc """
  A tiny, closed, serializable typed lambda calculus for nominal properties.

  Terms are data, never executable Elixir closures or dictionary-provided code.
  Predicate symbols are explicitly enumerated and mean only what the prototype
  declares. In particular, a well-typed predicate is not a factual assertion.
  """

  @nouns %{"bird" => :bird, "cat" => :cat, "dog" => :dog}
  @adjectives %{"black" => :black, "white" => :white, "small" => :small}
  @predicates MapSet.new([:bird, :cat, :dog, :black, :white, :small])

  @type semantic_type :: :entity | :truth | {:arrow, semantic_type(), semantic_type()}
  @type t ::
          {:var, String.t()}
          | {:lambda, String.t(), semantic_type(), t()}
          | {:apply, t(), t()}
          | {:predicate, atom(), t()}
          | {:and, t(), t()}

  @spec noun(String.t()) :: {:ok, t()} | :unsupported
  def noun(word) when is_binary(word) do
    case Map.fetch(@nouns, word) do
      {:ok, symbol} -> {:ok, {:lambda, "n", :entity, {:predicate, symbol, {:var, "n"}}}}
      :error -> :unsupported
    end
  end

  def noun(_), do: :unsupported

  @spec adjective(String.t()) :: {:ok, t()} | :unsupported
  def adjective(word) when is_binary(word) do
    case Map.fetch(@adjectives, word) do
      {:ok, symbol} ->
        {:ok,
         {:lambda, "property", {:arrow, :entity, :truth},
          {:lambda, "x", :entity,
           {:and, {:apply, {:var, "property"}, {:var, "x"}},
            {:predicate, symbol, {:var, "x"}}}}}}

      :error -> :unsupported
    end
  end

  def adjective(_), do: :unsupported

  @spec type_of(t(), map()) :: {:ok, semantic_type()} | {:error, atom()}
  def type_of(term, environment \\ %{})

  def type_of({:var, name}, env) when is_binary(name) and is_map(env) do
    case Map.fetch(env, name) do
      {:ok, type} -> {:ok, type}
      :error -> {:error, :free_variable}
    end
  end

  def type_of({:lambda, name, type, body}, env)
      when is_binary(name) and is_map(env) do
    if valid_type?(type) do
      with {:ok, result} <- type_of(body, Map.put(env, name, type)) do
        {:ok, {:arrow, type, result}}
      end
    else
      {:error, :invalid_parameter_type}
    end
  end

  def type_of({:apply, function, argument}, env) do
    with {:ok, {:arrow, expected, result}} <- type_of(function, env),
         {:ok, ^expected} <- type_of(argument, env) do
      {:ok, result}
    else
      _ -> {:error, :incompatible_application}
    end
  end

  def type_of({:predicate, symbol, argument}, env) do
    if MapSet.member?(@predicates, symbol) do
      with {:ok, :entity} <- type_of(argument, env) do
        {:ok, :truth}
      else
        _ -> {:error, :predicate_argument_type}
      end
    else
      {:error, :unapproved_predicate}
    end
  end

  def type_of({:and, left, right}, env) do
    with {:ok, :truth} <- type_of(left, env),
         {:ok, :truth} <- type_of(right, env) do
      {:ok, :truth}
    else
      _ -> {:error, :conjunction_type}
    end
  end

  def type_of(_, _), do: {:error, :invalid_expression}

  def valid_type?(:entity), do: true
  def valid_type?(:truth), do: true
  def valid_type?({:arrow, left, right}), do: valid_type?(left) and valid_type?(right)
  def valid_type?(_), do: false
end
