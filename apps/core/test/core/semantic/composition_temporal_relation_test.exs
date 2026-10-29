defmodule Core.Semantic.CompositionTemporalRelationTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.{Event, Expression, Proof, TemporalRelation}

  defp sitting do
    {:ok, proof} = Event.compose(%{
      predicate: "sitting", actor: "bird",
      preposition: "on", landmark: "fence"
    })
    proof
  end

  defp sleeping do
    {:ok, proof} = Event.compose(%{
      predicate: "sleeping", actor: "bird",
      preposition: "under", landmark: "tree"
    })
    proof
  end

  defp endpoints(%{result: {:lambda, e1, :event,
       {:lambda, e2, :event,
        {:and, {:and, first_event, second_event},
         {:event_precedes, {:var, earlier_var}, {:var, later_var}}}}}}) do
    %{first: e1, second: e2, before: earlier_var, after: later_var,
      first_event: first_event, second_event: second_event}
  end

  test "before composes two independent event properties and proves the ordering operator" do
    first = sitting()
    second = sleeping()

    assert {:ok, proof} = TemporalRelation.compose(:before, first, second)
    assert :ok = Proof.verify(proof)
    assert :ok = TemporalRelation.verify(proof, :before, first, second)
    assert proof.kind == :temporal_event_relation_property
    assert proof.connective == :before
    assert proof.result_type == {:arrow, :event, {:arrow, :event, :truth}}
    assert Expression.type_of(proof.result) == {:ok, proof.result_type}
    assert length(proof.steps) >= 4

    out = endpoints(proof)
    assert out.first != out.second
    assert out.before == out.first
    assert out.after == out.second
    assert Expression.type_of(out.first_event, %{out.first => :event}) == {:ok, :truth}
    assert Expression.type_of(out.second_event, %{out.second => :event}) == {:ok, :truth}

    assert proof.source_event_properties_verified?
    refute proof.connective_witness_verified?
    assert proof.referent_identity == :not_established
    assert proof.event_occurrence == :not_established
    assert proof.actual_event_chronology == :not_established
    assert proof.quantifier_applied? == false
    assert proof.factual_authority? == false
  end

  test "after reverses only temporal direction without swapping event semantics" do
    first = sleeping()
    second = sitting()

    assert {:ok, proof} = TemporalRelation.compose(:after, first, second)
    assert :ok = TemporalRelation.verify(proof, :after, first, second)
    out = endpoints(proof)

    assert out.before == out.second
    assert out.after == out.first
    assert Expression.type_of(out.first_event, %{out.first => :event}) == {:ok, :truth}
    assert Expression.type_of(out.second_event, %{out.second => :event}) == {:ok, :truth}
    assert out.first_event != out.second_event

    # Claiming that the same derivation came from a different connective
    # fails even though both expressions are mathematically well typed.
    assert TemporalRelation.verify(proof, :before, first, second) ==
      {:error, :invalid_temporal_relation}
  end

  test "unknown connectives, non-event arguments and unverified event witnesses abstain" do
    assert TemporalRelation.compose(:during, sitting(), sleeping()) == :unsupported
    assert TemporalRelation.compose(:before, %{}, sleeping()) == :unsupported
    assert TemporalRelation.compose(:after, sitting(), nil) == :unsupported

    changed = put_in(sitting(), [:semantics, :event_existence], :asserted)
    assert Event.verify(changed) != :ok
    assert TemporalRelation.compose(:before, changed, sleeping()) == :unsupported

    assert Expression.type_of({:event_precedes,
      {:var, "x"}, {:var, "e"}}, %{"x" => :entity, "e" => :event}) ==
      {:error, :temporal_event_argument_type}

    assert Expression.type_of({:event_precedes,
      {:var, "one"}, {:var, "two"}}, %{"one" => :event, "two" => :event}) ==
      {:ok, :truth}
  end

  test "tampering with beta steps, precedence direction or certainty flags is rejected" do
    first = sitting()
    second = sleeping()
    {:ok, proof} = TemporalRelation.compose(:before, first, second)

    out = endpoints(proof)
    inverted = %{proof | result:
      {:lambda, out.first, :event,
       {:lambda, out.second, :event,
        {:and, {:and, out.first_event, out.second_event},
         {:event_precedes, {:var, out.second}, {:var, out.first}}}}}}

    assert Proof.verify(inverted) != :ok
    assert TemporalRelation.verify(inverted, :before, first, second) ==
      {:error, :invalid_temporal_relation}

    skipped = %{proof | steps: []}
    assert TemporalRelation.verify(skipped, :before, first, second) ==
      {:error, :invalid_temporal_relation}

    forged_factual = %{proof | factual_authority?: true}
    assert Proof.verify(forged_factual) == :ok
    assert TemporalRelation.verify(forged_factual, :before, first, second) ==
      {:error, :invalid_temporal_relation}

    forged_occurrence = %{proof | event_occurrence: :established}
    assert TemporalRelation.verify(forged_occurrence, :before, first, second) ==
      {:error, :invalid_temporal_relation}

    swapped = %{proof | connective: :after}
    assert TemporalRelation.verify(swapped, :before, first, second) ==
      {:error, :invalid_temporal_relation}
  end

  test "a derived relation is tied to both exact source event properties" do
    first = sitting()
    second = sleeping()
    {:ok, proof} = TemporalRelation.compose(:before, first, second)

    assert TemporalRelation.verify(proof, :before, first, sitting()) ==
      {:error, :invalid_temporal_relation}
    assert TemporalRelation.verify(proof, :before, second, first) ==
      {:error, :invalid_temporal_relation}
  end
end
