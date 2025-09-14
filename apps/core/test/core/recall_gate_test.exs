defmodule Core.RecallGateTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput, as: SI
  alias Core.Recall.Gate
  alias Core.Recall.Plan

  defp si_stub() do
    %SI{
      original_sentence: "hello there",
      sentence: "hello there",
      source: :test,
      tokens: [
        %Core.Token{phrase: "hello", span: {0, 5}, mw: false, source: :assumed, instances: [], n: 1},
        %Core.Token{phrase: "there", span: {6, 11}, mw: false, source: :assumed, instances: [], n: 1}
      ],
      active_cells: [],
      brain_state_ref: nil,
      trace: []
    }
  end

  test "skip when confidence high and no triggers" do
    {:skip, si2} = Gate.gate(si_stub(), confidence: 0.9, requires_knowledge?: false, oov_terms: [], unmet_slots: [])
    ev = List.last(si2.trace)
    assert ev.stage == :recall_gate
    assert ev.meta.decision == :skip
    assert ev.meta.reasons == []
  end

  test "plan when confidence low" do
    {:plan, %Plan{} = plan, si2} = Gate.gate(si_stub(), confidence: 0.3)
    assert :low_conf in plan.triggers
    assert plan.budget_ms > 0
    assert plan.max_items > 0
    assert Enum.member?(plan.strategies, :exact)
    assert Enum.member?(plan.strategies, :embedding)
    ev = List.last(si2.trace)
    assert ev.meta.decision == :plan
    assert :low_conf in ev.meta.reasons
  end

  test "plan when intent requires knowledge even with high confidence" do
    {:plan, %Plan{} = plan, _si2} = Gate.gate(si_stub(), confidence: 0.92, requires_knowledge?: true)
    assert :intent_requires_knowledge in plan.triggers
  end

  test "plan when OOV terms present" do
    {:plan, %Plan{} = plan, si2} = Gate.gate(si_stub(), confidence: 0.8, oov_terms: ["kreuzberg"])
    assert :oov_terms_present in plan.triggers
    ev = List.last(si2.trace)
    assert :oov_terms_present in ev.meta.reasons
    # OOV nudges strategy order to prefer embedding early unless overridden
    assert plan.strategies |> Enum.member?(:embedding)
  end

  test "plan when unmet slots present" do
    {:plan, %Plan{} = plan, _si2} = Gate.gate(si_stub(), confidence: 0.8, unmet_slots: [:object])
    assert :unmet_slots in plan.triggers
  end
end

