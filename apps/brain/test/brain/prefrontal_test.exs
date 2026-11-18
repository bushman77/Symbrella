defmodule Brain.PrefrontalTest do
  use ExUnit.Case, async: true

  alias Brain.Prefrontal
  alias Brain.ControlSignals

  @ctx %{
    acc_conflict: 0.72,
    novelty: 0.55,
    wm_load: 0.38,
    wm_diversity: 0.46,
    predicted_confidence: 0.45,
    actual_outcome: 0.0,
    recent_success_rate: 0.35,
    avg_cost_ms: 220,
    ofc_value: 0.48
  }

  test "planner report identifies contributors and exposes canonical bounded control keys" do
    report = Prefrontal.report(@ctx)

    assert report.version == 1
    assert length(report.planners) == 5
    assert report.provenance[:top_k] == Brain.DLPFC.Policy
    assert report.provenance[:branch_budget] == Brain.FPC
    assert report.provenance[:confidence_scale] == Brain.DmPFC
    assert report.provenance[:utility_prior] == Brain.VmPFC
    assert report.provenance[:salience_boost] == Brain.Salience

    assert Enum.all?(report.planners, fn planner ->
             planner.status in [:produced, :no_opinion, :unavailable, :failed, :rejected] and
               is_atom(planner.planner) and is_atom(planner.function) and
               is_list(planner.accepted_keys) and is_list(planner.superseded_keys)
           end)

    assert %ControlSignals{} = Prefrontal.signals(@ctx)
    assert %ControlSignals{} = struct!(ControlSignals, report.signals)
    assert is_integer(report.signals.top_k)
    assert report.signals.top_k in 1..10
  end

  test "a planner's recorded merge keys are not claimed as executed actions" do
    report = Prefrontal.report(@ctx)
    dlpfc = Enum.find(report.planners, &(&1.planner == Brain.DLPFC.Policy))

    assert dlpfc.status == :produced
    assert :top_k in dlpfc.accepted_keys
    assert :max_retries in dlpfc.accepted_keys
    refute Map.has_key?(dlpfc, :executed_action)
    refute Map.has_key?(report, :selected_action)
  end
end
