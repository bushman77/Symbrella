defmodule Core.Semantic.Composition.EntityEvent do
  @moduledoc """
  A typed, replayable join of two *properties*, not two asserted facts.

  An independently verified nominal property N : entity -> truth and
  progressive event property V : event -> truth are joined by an actor relation:

      λx:entity. λe:event. N(x) ∧ V(e) ∧ actor(x, e)

  The resulting type is entity -> event -> truth. Quantifiers are NOT
  introduced: there is no claim of an actual entity, event, or established
  identity. Binding a historical description to a current pronoun requires
  separate same-session discourse evidence, checked by DiscourseComposition.
  """

  alias Core.Semantic.Composition.{Event, Expression, Proof, Reducer}

  @nominal_type {:arrow, :entity, :truth}
  @event_type {:arrow, :event, :truth}
  @joined_type {:arrow, :entity, {:arrow, :event, :truth}}

  @spec compose(map(), map()) :: {:ok, map()} | :unsupported
  def compose(%{} = nominal, %{} = event) do
    with :ok <- Proof.verify(nominal),
         :ok <- Event.verify(event),
         true <- nominal.result_type == @nominal_type,
         true <- event.result_type == @event_type,
         {:ok, actor_kind} <- nominal_actor_kind(nominal.result),
         %{actor: %{kind: ^actor_kind, referent: :unresolved},
           event_existence: :not_asserted} <- event.semantics,
         initial = {:apply, {:apply, operator(), nominal.result}, event.result},
         {:ok, @joined_type} <- Expression.type_of(initial),
         {:ok, result, steps} <- Reducer.reduce(initial),
         true <- linked_normal_form?(result),
         derivation = %{
           kind: :entity_event_relation_property,
           status: :derived,
           initial: initial,
           steps: steps,
           result: result,
           result_type: @joined_type,
           actor_kind: actor_kind,
           quantifier_applied?: false,
           referent_identity: :unresolved,
           event_existence: :not_asserted,
           factual_authority?: false
         },
         :ok <- Proof.verify(derivation) do
      {:ok, derivation}
    else
      _ -> :unsupported
    end
  end

  def compose(_, _), do: :unsupported

  @doc """
  Replay the exact join against both currently supplied independent proofs.

  An arbitrary well-typed property is insufficient: source derivations,
  actor-kind agreement, link normal form, and every proof step must agree.
  """
  @spec verify(map(), map(), map()) :: :ok | {:error, atom()}
  def verify(%{} = saved, %{} = nominal, %{} = event) do
    with :ok <- Proof.verify(saved),
         {:ok, ^saved} <- compose(nominal, event) do
      :ok
    else
      _ -> {:error, :invalid_entity_event_join}
    end
  end

  def verify(_, _, _), do: {:error, :invalid_entity_event_join}

  defp operator do
    {:lambda, "N", @nominal_type,
     {:lambda, "V", @event_type,
      {:lambda, "x", :entity,
       {:lambda, "e", :event,
        {:and, {:apply, {:var, "N"}, {:var, "x"}},
         {:and, {:apply, {:var, "V"}, {:var, "e"}},
          {:event_actor_entity, {:var, "x"}, {:var, "e"}}}}}}}}
  end

  # Two adjectives composed with the noun: every nominal predicate must
  # describe the same bound entity and the head must be a licensed actor kind.
  defp nominal_actor_kind(
         {:lambda, variable, :entity,
          {:and,
           {:and, {:predicate, kind, {:var, noun_var}},
                  {:predicate, _, {:var, inner_var}}},
           {:predicate, _, {:var, outer_var}}}}
       )
       when is_binary(variable) and noun_var == variable and
              inner_var == variable and outer_var == variable and
              kind in [:bird, :cat, :dog], do: {:ok, kind}

  defp nominal_actor_kind(_), do: :unsupported

  # The actor connection must be present, relate exactly the two outer
  # variables, and be in the canonical join position.
  defp linked_normal_form?(
         {:lambda, x, :entity,
          {:lambda, e, :event,
           {:and, _nominal_property,
            {:and, _event_property,
             {:event_actor_entity, {:var, actor_var}, {:var, event_var}}}}}}
       ) when is_binary(x) and is_binary(e) and
              actor_var == x and event_var == e, do: true

  defp linked_normal_form?(_), do: false
end
