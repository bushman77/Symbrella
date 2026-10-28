defmodule Core.Semantic.CompositionEntityEventTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.{EntityEvent, Event, Expression, Nominal, Proof}

  defp nominal(head \\ "bird", modifiers \\ ["small", "black"]) do
    {:ok, derivation} = Nominal.compose(head, modifiers)
    derivation
  end

  defp event(actor \\ "bird", action \\ "sitting", prep \\ "on",
             landmark \\ "fence") do
    {:ok, derivation} = Event.compose(%{
      predicate: action, actor: actor,
      preposition: prep, landmark: landmark
    })
    derivation
  end

  test "a real beta proof joins nominal and event properties through actor(x,e)" do
    n = nominal()
    v = event()

    assert {:ok, derivation} = EntityEvent.compose(n, v)
    assert :ok = Proof.verify(derivation)
    assert :ok = EntityEvent.verify(derivation, n, v)
    assert derivation.kind == :entity_event_relation_property
    assert derivation.status == :derived
    assert derivation.actor_kind == :bird
    assert derivation.result_type == {:arrow, :entity, {:arrow, :event, :truth}}
    assert Expression.type_of(derivation.result) == {:ok, derivation.result_type}

    assert {:lambda, entity, :entity,
             {:lambda, episode, :event,
              {:and, _, {:and, _, {:event_actor_entity,
                {:var, actor}, {:var, linked_event}}}}}} = derivation.result
    assert actor == entity
    assert linked_event == episode
    assert length(derivation.steps) > 0

    # A typed property join does not introduce an individual or a happening.
    assert derivation.quantifier_applied? == false
    assert derivation.referent_identity == :unresolved
    assert derivation.event_existence == :not_asserted
    assert derivation.factual_authority? == false
  end

  test "the same operator applies to an independently composed cat event" do
    n = nominal("cat", ["white", "small"])
    v = event("cat", "sleeping", "under", "table")

    assert {:ok, joined} = EntityEvent.compose(n, v)
    assert :ok = EntityEvent.verify(joined, n, v)
    assert joined.actor_kind == :cat
    assert joined.result_type == {:arrow, :entity, {:arrow, :event, :truth}}
  end

  test "unrelated actor kinds never join even when each proof is well typed" do
    n = nominal("bird")
    v = event("cat", "sleeping", "under", "table")

    assert :ok = Proof.verify(n)
    assert :ok = Event.verify(v)
    assert EntityEvent.compose(n, v) == :unsupported
    assert EntityEvent.verify(%{}, n, v) ==
      {:error, :invalid_entity_event_join}
  end

  test "tampered actor relation, proof steps, and epistemic flags fail replay" do
    n = nominal()
    v = event()
    {:ok, joined} = EntityEvent.compose(n, v)

    forged_result = %{joined | result: {:lambda, "x", :entity,
      {:lambda, "e", :event, {:and, {:predicate, :bird, {:var, "x"}},
        {:event_action, :sit, {:var, "e"}}}}}}
    assert Proof.verify(forged_result) != :ok
    assert EntityEvent.verify(forged_result, n, v) ==
      {:error, :invalid_entity_event_join}

    forged_step = %{joined | steps:
      [%{rule: :beta, before: joined.initial, after: joined.result}]}
    assert EntityEvent.verify(forged_step, n, v) ==
      {:error, :invalid_entity_event_join}

    factual = %{joined | factual_authority?: true}
    assert Proof.verify(factual) == :ok
    assert EntityEvent.verify(factual, n, v) ==
      {:error, :invalid_entity_event_join}

    existential = %{joined | event_existence: :asserted}
    assert EntityEvent.verify(existential, n, v) ==
      {:error, :invalid_entity_event_join}
  end

  test "event semantics, source proof, and nominal normal-form tampering are rejected" do
    n = nominal()
    v = event()
    {:ok, joined} = EntityEvent.compose(n, v)

    lied = put_in(v, [:semantics, :event_existence], :asserted)
    assert Event.verify(lied) != :ok
    assert EntityEvent.compose(n, lied) == :unsupported
    assert EntityEvent.verify(joined, n, lied) ==
      {:error, :invalid_entity_event_join}

    broken_nominal = %{n | result: {:var, "forged"}}
    assert EntityEvent.compose(broken_nominal, v) == :unsupported

    changed_spatial = event("bird", "sleeping", "under", "tree")
    assert EntityEvent.verify(joined, n, changed_spatial) ==
      {:error, :invalid_entity_event_join}
  end
end
