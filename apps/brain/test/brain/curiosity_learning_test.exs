defmodule Brain.CuriosityLearningTest do
  use ExUnit.Case, async: true
  alias Brain.CuriosityLearning, as: Learning

  defp row(id, attributes) do
    Map.merge(%{issue_id: id, completed_at: :done, status: "resolved",
      origin: "verified_direct_proof", acquisition: "not_attempted",
      acquired_proof_id: nil, parent_proof_ids: []}, attributes)
  end

  defp issue(id, resolution) do
    {id, %{status: "resolved", resolution: resolution}}
  end

  test "learns only when the audit agrees with a resolved issue and proof lineage" do
    attempts = [
      row(1, %{}),
      row(2, %{origin: "hippocampal_two_hop_cue", parent_proof_ids: [10, 11],
        acquisition: "captured", acquired_proof_id: 11}),
      row(3, %{origin: "verified_direct_proof"})
    ]

    issues = Map.new([
      issue(1, %{"reasoning_rule" => "direct_population_comparison", "proof_id" => 9}),
      issue(2, %{"reasoning_rule" => "transitive_population_order", "parent_proof_ids" => [10, 11]}),
      {3, %{status: "open", resolution: nil}}
    ])

    projection = Learning.project(attempts, issues)
    assert projection.v == 1
    assert projection.success_count == 2
    assert projection.sample_count == 2
    assert projection.biases.direct > 0
    assert projection.biases.two_hop > 0
    assert projection.biases.acquisition > 0
    assert projection.biases.direct > projection.biases.missing_link
    assert projection.biases.two_hop > projection.biases.missing_link
  end

  test "duplicate audit entries, incomplete attempts, and invented proof identities earn nothing" do
    attempts = [
      row(1, %{origin: "hippocampal_two_hop_cue", parent_proof_ids: [9, 10]}),
      row(1, %{}),
      row(2, %{completed_at: nil}),
      row(3, %{})
    ]
    issues = Map.new([
      issue(1, %{"reasoning_rule" => "transitive_population_order",
                 "parent_proof_ids" => [10, 11]}),
      issue(2, %{"reasoning_rule" => "direct_population_comparison", "proof_id" => 12})
    ])
    result = Learning.project(attempts, issues)
    assert result == Learning.neutral()
  end

  test "contradictions are protective, unavailable evidence neutral and scores bounded" do
    rows =
      Enum.map(1..150, fn id ->
        row(id, %{status: "unresolved", reason: "proof_not_authorized"})
      end)

    result = Learning.project(rows, %{})
    assert result.biases.missing_link == -0.12
    assert result.failure_count == 100
    assert result.biases.two_hop == -0.12
    assert Learning.project([row(1, %{status: "unresolved", reason: "no_two_hop_cue"})], %{}).biases.missing_link == 0.0
    assert Learning.project([row(1, %{status: "unresolved",
      reason: "direct_proof_not_authorized"})], %{}).biases.direct == -0.012
    assert Learning.normalize(%{"v" => 1, "biases" => %{"direct" => 800}}).biases.direct == 0.12
    assert Learning.normalize(%{v: 999, biases: %{direct: 1.0}}) == Learning.neutral()
  end

  test "model export and continuity preserve the versioned learned projection" do
    learned = Learning.normalize(%{v: 1, biases: %{direct: 0.06}, sample_count: 2})
    model = %Brain.SelfModel{investigative_biases: learned}
    assert {:ok, imported} = model |> Brain.SelfModel.export() |> Brain.SelfModel.import()
    assert imported.investigative_biases == learned
    assert {:ok, restored} =
      model |> Brain.SelfContinuity.to_snapshot() |> Brain.SelfContinuity.from_snapshot()
    assert restored.investigative_biases == learned
  end
end
