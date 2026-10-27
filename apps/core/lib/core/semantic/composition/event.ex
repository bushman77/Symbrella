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

        if Proof.verify(derivation) == :ok do
          {:ok, Map.put(derivation, :semantics, %{
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
          })}
        else
          :unsupported
        end
      else
        _ -> :unsupported
      end
    else
      _ -> :unsupported
    end
  end

  def compose(_), do: :unsupported

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
