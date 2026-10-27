defmodule Core.Semantic.Composition.Event do
  @moduledoc """
  Closed, pure progressive event composer.

  The result is a PROPERTY OF POSSIBLE EVENTS (event -> truth), not an
  existential claim. Actor and landmark nouns are kind constraints, not
  resolved individuals. Direction :into never licenses an arrival predicate.
  """

  alias Core.Semantic.Composition.{Expression, Proof, Reducer}

  @actions %{"sitting" => :sit, "sleeping" => :sleep, "flying" => :fly}
  @actors %{"bird" => :bird, "cat" => :cat, "dog" => :dog}
  @landmarks %{"fence" => :fence, "tree" => :tree, "table" => :table}
  @relations %{
    "on" => :on, "in" => :in, "under" => :under, "beside" => :beside,
    "above" => :above, "below" => :below, "into" => :into
  }
  @static_relations MapSet.new([:on, :in, :under, :beside, :above, :below])

  @spec compose(map()) :: {:ok, map()} | :unsupported
  def compose(%{predicate: predicate, actor: actor, preposition: prep,
                landmark: landmark}) do
    with {:ok, action} <- Map.fetch(@actions, predicate),
         {:ok, actor_kind} <- Map.fetch(@actors, actor),
         {:ok, landmark_kind} <- Map.fetch(@landmarks, landmark),
         {:ok, relation} <- Map.fetch(@relations, prep),
         true <- licensed?(action, relation) do
      initial =
        {:apply,
         spatial_modifier(relation, landmark_kind),
         {:apply, actor_modifier(actor_kind), progressive_action(action)}}

      with {:ok, {:arrow, :event, :truth}} <- Expression.type_of(initial),
           {:ok, result, steps} <- Reducer.reduce(initial) do
        derivation = %{
          initial: initial,
          steps: steps,
          result: result,
          result_type: {:arrow, :event, :truth}
        }

        # Project the reporting map from the NORMAL FORM, never directly
        # from the input words. This binds human-readable fields to the AST.
        with :ok <- Proof.verify(derivation),
             {:ok, semantics} <- project(result) do
          {:ok, Map.put(derivation, :semantics, semantics)}
        else
          _ -> :unsupported
        end
      else
        _ -> :unsupported
      end
    else
      _ -> :unsupported
    end
  end

  def compose(_), do: :unsupported

  @doc """
  Decode only the exact, closed four-constraint normal form of a progressive
  event. Unknown clauses, extra assertions, and unbound variables fail closed.

  :not_established and :not_asserted denote absent proof premises, NOT
  claims that arrival or occurrence are factually false.
  """
  @spec project(Expression.t()) :: {:ok, map()} | :unsupported
  def project(
        {:lambda, event_var, :event,
         {:and,
          {:and,
           {:and,
            {:event_action, action, {:var, action_event}},
            {:event_aspect, :progressive, {:var, aspect_event}}},
           {:event_actor_kind, actor_kind, {:var, actor_event}}},
          {:event_spatial, relation, landmark_kind, {:var, spatial_event}}}}
      )
      when is_binary(event_var) and event_var == action_event and
             event_var == aspect_event and event_var == actor_event and
             event_var == spatial_event do
    if action in Map.values(@actions) and actor_kind in Map.values(@actors) and
         landmark_kind in Map.values(@landmarks) and
         relation in Map.values(@relations) and licensed?(action, relation) do
      {:ok,
       %{
         aspect: :progressive,
         predicate: action,
         actor: %{kind: actor_kind, referent: :unresolved},
         spatial: %{
           relation: relation,
           landmark_kind: landmark_kind,
           reading: if(relation == :into, do: :directional, else: :locative),
           arrival: :not_established
         },
         event_existence: :not_asserted
       }}
    else
      :unsupported
    end
  end

  def project(_), do: :unsupported

  @doc """
  Verify both the mathematical derivation and the exact reported semantics.
  Proof.verify/1 intentionally checks ONLY the lambda calculus derivation.
  """
  @spec verify(map()) :: :ok | {:error, atom()}
  def verify(%{event_semantics: semantics} = derivation),
    do: verify(Map.put(derivation, :semantics, semantics))

  def verify(%{result: result, semantics: semantics} = derivation) do
    with :ok <- Proof.verify(derivation),
         {:ok, ^semantics} <- project(result) do
      :ok
    else
      _ -> {:error, :invalid_event_semantics}
    end
  end

  def verify(_), do: {:error, :invalid_event_semantics}

  defp licensed?(:fly, :into), do: true
  defp licensed?(action, relation) when action in [:sit, :sleep],
    do: MapSet.member?(@static_relations, relation)
  defp licensed?(_, _), do: false

  defp progressive_action(action) do
    {:lambda, "e", :event,
     {:and, {:event_action, action, {:var, "e"}},
      {:event_aspect, :progressive, {:var, "e"}}}}
  end

  defp actor_modifier(kind) do
    {:lambda, "P", {:arrow, :event, :truth},
     {:lambda, "e", :event,
      {:and, {:apply, {:var, "P"}, {:var, "e"}},
       {:event_actor_kind, kind, {:var, "e"}}}}}
  end

  defp spatial_modifier(relation, landmark) do
    {:lambda, "P", {:arrow, :event, :truth},
     {:lambda, "e", :event,
      {:and, {:apply, {:var, "P"}, {:var, "e"}},
       {:event_spatial, relation, landmark, {:var, "e"}}}}}
  end
end
