defmodule Core.Semantic.Composition.Reducer do
  @moduledoc """
  Pure, deterministic beta-reduction for the closed composition AST.

  Uses capture-avoiding substitution (alpha-renaming bound variables when
  necessary). Reduction is bounded by term size and a fixed step budget.
  """
  alias Core.Semantic.Composition.Expression

  @max_steps 24
  @max_nodes 128

  @spec reduce(Expression.term()) ::
          {:ok, Expression.term(), [map()]} | {:error, atom()}
  def reduce(expression) do
    with {:ok, type} <- Expression.type_of(expression),
         true <- size(expression) <= @max_nodes do
      loop(expression, type, [], @max_steps)
    else
      false -> {:error, :expression_too_large}
      {:error, reason} -> {:error, reason}
    end
  end

  defp loop(expression, type, steps, remaining) do
    case step(expression) do
      :normal ->
        {:ok, expression, Enum.reverse(steps)}

      {:ok, next} when remaining > 0 ->
        with {:ok, ^type} <- Expression.type_of(next),
             true <- size(next) <= @max_nodes do
          loop(next, type,
            [%{rule: :beta, before: expression, after: next} | steps], remaining - 1)
        else
          false -> {:error, :expression_too_large}
          _ -> {:error, :type_not_preserved}
        end

      {:ok, _} ->
        {:error, :reduction_limit}
    end
  end

  @doc "Perform exactly one leftmost-outermost beta step, or return :normal."
  @spec step(Expression.term()) :: {:ok, Expression.term()} | :normal
  def step({:apply, {:lambda, name, _type, body}, argument}) do
    {:ok, substitute(body, name, argument)}
  end

  def step({:apply, function, argument}) do
    case step(function) do
      {:ok, next} -> {:ok, {:apply, next, argument}}
      :normal ->
        case step(argument) do
          {:ok, next} -> {:ok, {:apply, function, next}}
          :normal -> :normal
        end
    end
  end

  def step({:lambda, name, type, body}) do
    case step(body) do
      {:ok, next} -> {:ok, {:lambda, name, type, next}}
      :normal -> :normal
    end
  end

  def step({:and, left, right}) do
    case step(left) do
      {:ok, next} -> {:ok, {:and, next, right}}
      :normal ->
        case step(right) do
          {:ok, next} -> {:ok, {:and, left, next}}
          :normal -> :normal
        end
    end
  end

  def step({:predicate, symbol, argument}) do
    case step(argument) do
      {:ok, next} -> {:ok, {:predicate, symbol, next}}
      :normal -> :normal
    end
  end

  def step(_), do: :normal

  @doc "Capture-avoiding substitution of a free variable."
  def substitute({:var, name}, name, replacement), do: replacement
  def substitute({:var, _} = variable, _, _), do: variable

  def substitute({:lambda, name, type, body} = lambda, name, _replacement),
    do: lambda

  def substitute({:lambda, name, type, body}, variable, replacement) do
    if MapSet.member?(free_variables(replacement), name) do
      forbidden = MapSet.union(all_variables(body), all_variables(replacement))
      fresh = fresh_name(name, MapSet.put(forbidden, variable))
      renamed = substitute(body, name, {:var, fresh})
      {:lambda, fresh, type, substitute(renamed, variable, replacement)}
    else
      {:lambda, name, type, substitute(body, variable, replacement)}
    end
  end

  def substitute({:apply, function, argument}, variable, replacement) do
    {:apply, substitute(function, variable, replacement),
     substitute(argument, variable, replacement)}
  end

  def substitute({:and, left, right}, variable, replacement) do
    {:and, substitute(left, variable, replacement),
     substitute(right, variable, replacement)}
  end

  def substitute({:predicate, symbol, argument}, variable, replacement) do
    {:predicate, symbol, substitute(argument, variable, replacement)}
  end

  def substitute(other, _variable, _replacement), do: other

  defp free_variables({:var, name}), do: MapSet.new([name])

  defp free_variables({:lambda, name, _, body}),
    do: MapSet.delete(free_variables(body), name)

  defp free_variables({:apply, left, right}),
    do: MapSet.union(free_variables(left), free_variables(right))

  defp free_variables({:and, left, right}),
    do: MapSet.union(free_variables(left), free_variables(right))

  defp free_variables({:predicate, _, argument}), do: free_variables(argument)
  defp free_variables(_), do: MapSet.new()

  defp all_variables({:var, name}), do: MapSet.new([name])

  defp all_variables({:lambda, name, _, body}),
    do: MapSet.put(all_variables(body), name)

  defp all_variables({:apply, left, right}),
    do: MapSet.union(all_variables(left), all_variables(right))

  defp all_variables({:and, left, right}),
    do: MapSet.union(all_variables(left), all_variables(right))

  defp all_variables({:predicate, _, argument}), do: all_variables(argument)
  defp all_variables(_), do: MapSet.new()

  defp fresh_name(base, forbidden) do
    Enum.find_value(1..(@max_nodes + 1), fn i ->
      candidate = base <> "_" <> Integer.to_string(i)
      if not MapSet.member?(forbidden, candidate), do: candidate
    end)
  end

  defp size({:var, _}), do: 1
  defp size({:lambda, _, _, body}), do: 1 + size(body)
  defp size({:apply, left, right}), do: 1 + size(left) + size(right)
  defp size({:and, left, right}), do: 1 + size(left) + size(right)
  defp size({:predicate, _, arg}), do: 1 + size(arg)
  defp size(_), do: @max_nodes + 1
end
