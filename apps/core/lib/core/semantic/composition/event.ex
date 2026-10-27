defmodule Core.Semantic.Composition.Event do
  @moduledoc """
  Closed, pure progressive event composer.

  The result is a PROPERTY OF POSSIBLE EVENTS (event -> truth), not an
  existential claim. Actor and landmark nouns are kind constraints, not
  resolved individuals. Direction :into never licenses an arrival predicate.
  """

  alias Core.Semantic.Composition.{Expression, Lexical, Proof, Reducer}

  @actions %{"sitting" => :sit, "sleeping" => :sleep, "flying" => :fly}
  @actors %{"bird" => :bird, "cat" => :cat, "dog" => :dog}
  @static_relations MapSet.new([:on, :in, :under, :beside, :above, :below])

  @spec compose(map()) :: {:ok, map()} | :unsupported
  def compose(%{predicate: predicate, actor: actor, preposition: prep,
                landmark: landmark}) do
    with {:ok, action} <- Map.fetch(@actions, predicate),
         {:ok, actor_kind} <- Map.fetch(@actors, actor),
         {:ok, %{term: relation_term, symbol: relation}} <-
           Lexical.spatial_modifier(prep),
         {:ok, %{term: landmark_term}} <- Lexical.landmark(landmark),
         true <- licensed?(action, relation) do
      # Compose the spatial phrase incrementally: the preposition is a
      # function of a landmark before it modifies an event property.
      initial =
        {:apply,
         {:apply, relation_term, landmark_term},
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
          {:event_spatial, relation, landmark_term, {:var, spatial_event}}}}
      )
      when is_binary(event_var) and event_var == action_event and
             event_var == aspect_event and event_var == actor_event and
             event_var == spatial_event do
    landmark_kind =
      case landmark_term do
        {:landmark_kind, kind} -> kind
        kind when is_atom(kind) -> kind
        _ -> nil
      end

    if action in Map.values(@actions) and actor_kind in Map.values(@actors) and
         Lexical.known_landmark?(landmark_kind) and
         Lexical.known_relation?(relation) and licensed?(action, relation) do
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
  def verify(%{event_semantics: semantics, semantics: reported} = derivation) do
    if semantics == reported do
      verify_summary(derivation, semantics)
    else
      {:error, :invalid_event_semantics}
    end
  end

  def verify(%{event_semantics: semantics} = derivation),
    do: verify_summary(derivation, semantics)

  def verify(%{semantics: semantics} = derivation),
    do: verify_summary(derivation, semantics)

  def verify(_), do: {:error, :invalid_event_semantics}

  # Deliberately non-recursive. The two accepted summary field names refer
  # to the same normal form, but neither can override a contradictory other.
  defp verify_summary(%{result: result} = derivation, semantics) do
    with :ok <- Proof.verify(derivation),
         {:ok, ^semantics} <- project(result) do
      :ok
    else
      _ -> {:error, :invalid_event_semantics}
    end
  end

  defp verify_summary(_, _), do: {:error, :invalid_event_semantics}

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

end
