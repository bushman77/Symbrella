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
  @event_actions MapSet.new([:sit, :sleep, :fly])
  @event_actors MapSet.new([:bird, :cat, :dog])
  @landmarks MapSet.new([:fence, :tree, :table])
  @spatial_relations MapSet.new([:on, :in, :under, :beside, :above, :below, :into])

  @type semantic_type :: :entity | :event | :truth | :landmark_kind |
          :actor_kind | :action_kind | {:arrow, semantic_type(), semantic_type()}
  @type t ::
          {:var, String.t()}
          | {:lambda, String.t(), semantic_type(), t()}
          | {:apply, t(), t()}
          | {:predicate, atom(), t()}
          | {:landmark_kind, atom()}
          | {:actor_kind, atom()}
          | {:action_kind, atom()}
          | {:and, t(), t()}
          | {:event_action, atom() | t(), t()}
          | {:event_aspect, :progressive, t()}
          | {:event_actor_kind, atom() | t(), t()}
          | {:event_actor_entity, t(), t()}
          | {:event_spatial, atom(), atom(), t()}

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

  def type_of({:action_kind, action}, _env) do
    if MapSet.member?(@event_actions, action),
      do: {:ok, :action_kind},
      else: {:error, :unapproved_event_symbol}
  end

  def type_of({:actor_kind, actor}, _env) do
    if MapSet.member?(@event_actors, actor),
      do: {:ok, :actor_kind},
      else: {:error, :unapproved_event_symbol}
  end

  def type_of({:event_action, action, event}, env) do
    event_predicate_type(kind_valid?(action, :action_kind, @event_actions, env),
      event, env)
  end

  def type_of({:event_aspect, :progressive, event}, env),
    do: event_predicate_type(true, event, env)

  def type_of({:event_actor_kind, kind, event}, env) do
    event_predicate_type(kind_valid?(kind, :actor_kind, @event_actors, env),
      event, env)
  end

  # This proposition relates a possible entity and a possible event; it
  # does not assert that either exists or resolve a discourse referent.
  def type_of({:event_actor_entity, entity, event}, env) do
    with {:ok, :entity} <- type_of(entity, env),
         {:ok, :event} <- type_of(event, env) do
      {:ok, :truth}
    else
      _ -> {:error, :entity_event_actor_argument_type}
    end
  end

  def type_of({:landmark_kind, kind}, _env) do
    if MapSet.member?(@landmarks, kind),
      do: {:ok, :landmark_kind},
      else: {:error, :unapproved_event_symbol}
  end

  def type_of({:event_spatial, relation, landmark, event}, env) do
    landmark_valid? =
      case landmark do
        kind when is_atom(kind) -> MapSet.member?(@landmarks, kind)
        term -> type_of(term, env) == {:ok, :landmark_kind}
      end

    event_predicate_type(
      MapSet.member?(@spatial_relations, relation) and landmark_valid?,
      event,
      env
    )
  end

  def type_of(_, _), do: {:error, :invalid_expression}

  defp kind_valid?(symbol, _type, allowed, _env) when is_atom(symbol),
    do: MapSet.member?(allowed, symbol)

  defp kind_valid?(term, type, _allowed, env),
    do: type_of(term, env) == {:ok, type}

  defp event_predicate_type(false, _event, _env), do: {:error, :unapproved_event_symbol}

  defp event_predicate_type(true, event, env) do
    case type_of(event, env) do
      {:ok, :event} -> {:ok, :truth}
      _ -> {:error, :event_argument_type}
    end
  end

  def valid_type?(:entity), do: true
  def valid_type?(:event), do: true
  def valid_type?(:truth), do: true
  def valid_type?(:landmark_kind), do: true
  def valid_type?(:actor_kind), do: true
  def valid_type?(:action_kind), do: true
  def valid_type?({:arrow, left, right}), do: valid_type?(left) and valid_type?(right)
  def valid_type?(_), do: false
end
