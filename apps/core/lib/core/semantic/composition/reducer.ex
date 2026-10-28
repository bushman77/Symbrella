defmodule Core.Semantic.Composition.Reducer do
  @moduledoc """
  Pure, deterministic beta-reduction for the closed composition AST.

  Uses capture-avoiding substitution (alpha-renaming bound variables when
  necessary). Reduction is bounded by term size and a fixed step budget.
  """
  alias Core.Semantic.Composition.Expression

  @max_steps 24
  @max_nodes 128

  @spec reduce(Expression.t()) ::
          {:ok, Expression.t(), [map()]} | {:error, atom()}
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
  @spec step(Expression.t()) :: {:ok, Expression.t()} | :normal
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

  def step({:event_action, action, event}) do
    case step(action) do
      {:ok, next} -> {:ok, {:event_action, next, event}}
      :normal -> step_event(event, &{:event_action, action, &1})
    end
  end

  def step({:event_aspect, aspect, event}) do
    step_event(event, &{:event_aspect, aspect, &1})
  end

  def step({:event_actor_kind, kind, event}) do
    case step(kind) do
      {:ok, next} -> {:ok, {:event_actor_kind, next, event}}
      :normal -> step_event(event, &{:event_actor_kind, kind, &1})
    end
  end

  def step({:event_actor_entity, entity, event}) do
    case step(entity) do
      {:ok, next} -> {:ok, {:event_actor_entity, next, event}}
      :normal -> step_event(event, &{:event_actor_entity, entity, &1})
    end
  end

  def step({:event_spatial, relation, landmark, event}) do
    # A landmark is now a first-class typed term. Reduce it, if needed,
    # before reducing the event expression in the surrounding predicate.
    case step(landmark) do
      {:ok, next} -> {:ok, {:event_spatial, relation, next, event}}
      :normal -> step_event(event, &{:event_spatial, relation, landmark, &1})
    end
  end

  def step(_), do: :normal

  defp step_event(term, rebuild) do
    case step(term) do
      {:ok, next} -> {:ok, rebuild.(next)}
      :normal -> :normal
    end
  end

  @doc "Capture-avoiding substitution of a free variable."
  def substitute({:var, name}, variable, replacement)
      when name == variable,
      do: replacement
  def substitute({:var, _} = variable, _, _), do: variable

  def substitute({:lambda, binder, _type, _body} = lambda, variable, _replacement)
      when binder == variable,
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

  def substitute({:event_action, action, event}, variable, replacement) do
    {:event_action, substitute(action, variable, replacement),
     substitute(event, variable, replacement)}
  end

  def substitute({:event_aspect, aspect, event}, variable, replacement) do
    {:event_aspect, aspect, substitute(event, variable, replacement)}
  end

  def substitute({:event_actor_kind, kind, event}, variable, replacement) do
    {:event_actor_kind, substitute(kind, variable, replacement),
     substitute(event, variable, replacement)}
  end

  def substitute({:event_actor_entity, entity, event}, variable, replacement) do
    {:event_actor_entity, substitute(entity, variable, replacement),
     substitute(event, variable, replacement)}
  end

  def substitute({:event_spatial, relation, landmark, event}, variable, replacement) do
    {:event_spatial, relation,
     substitute(landmark, variable, replacement),
     substitute(event, variable, replacement)}
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
  defp free_variables({:event_action, action, event}),
    do: MapSet.union(free_variables(action), free_variables(event))
  defp free_variables({:event_aspect, _, event}), do: free_variables(event)
  defp free_variables({:event_actor_kind, kind, event}),
    do: MapSet.union(free_variables(kind), free_variables(event))
  defp free_variables({:event_actor_entity, entity, event}),
    do: MapSet.union(free_variables(entity), free_variables(event))
  defp free_variables({:event_spatial, _, landmark, event}),
    do: MapSet.union(free_variables(landmark), free_variables(event))
  defp free_variables(_), do: MapSet.new()

  defp all_variables({:var, name}), do: MapSet.new([name])

  defp all_variables({:lambda, name, _, body}),
    do: MapSet.put(all_variables(body), name)

  defp all_variables({:apply, left, right}),
    do: MapSet.union(all_variables(left), all_variables(right))

  defp all_variables({:and, left, right}),
    do: MapSet.union(all_variables(left), all_variables(right))

  defp all_variables({:predicate, _, argument}), do: all_variables(argument)
  defp all_variables({:event_action, action, event}),
    do: MapSet.union(all_variables(action), all_variables(event))
  defp all_variables({:event_aspect, _, event}), do: all_variables(event)
  defp all_variables({:event_actor_kind, kind, event}),
    do: MapSet.union(all_variables(kind), all_variables(event))
  defp all_variables({:event_actor_entity, entity, event}),
    do: MapSet.union(all_variables(entity), all_variables(event))
  defp all_variables({:event_spatial, _, landmark, event}),
    do: MapSet.union(all_variables(landmark), all_variables(event))
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
  defp size({:action_kind, _}), do: 1
  defp size({:event_action, action, event}),
    do: 1 + (if is_atom(action), do: 0, else: size(action)) + size(event)
  defp size({:event_aspect, _, event}), do: 1 + size(event)
  defp size({:actor_kind, _}), do: 1
  defp size({:event_actor_kind, kind, event}),
    do: 1 + (if is_atom(kind), do: 0, else: size(kind)) + size(event)
  defp size({:landmark_kind, _}), do: 1
  defp size({:event_actor_entity, entity, event}),
    do: 1 + size(entity) + size(event)
  defp size({:event_spatial, _, landmark, event}),
    do: 1 + (if is_atom(landmark), do: 0, else: size(landmark)) + size(event)
  defp size(_), do: @max_nodes + 1
end
