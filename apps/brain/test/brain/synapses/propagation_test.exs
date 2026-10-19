defmodule Brain.Synapses.PropagationTest do
  use ExUnit.Case, async: true

  alias Brain.Synapses.{Shadow, Propagation}

  setup do
    {:ok, server} = Shadow.start_link(name: nil, durable?: false)
    %{server: server}
  end

  defp learn(server, verb, actor, patient, evidence) do
    Shadow.observe(%{
      provenance: :user_reported,
      evidence: evidence,
      session_id: "propagation-shadow-test",
      edges: [
        {verb, actor, :actor},
        {verb, patient, :patient}
      ]
    }, server)
  end

  test "one-hop signal is restricted to matching patient role and actual verb ID", %{server: server} do
    assert {:ok, :recorded} =
             learn(server, "draws|verb|1", "artist|noun|1",
               "picture|noun|1", "observation-1")

    candidates = ["draws|verb|2", "draws|verb|1", "draws|verb|3"]

    on = Propagation.compare(candidates, "picture|noun|1", :patient, server: server)
    off = Propagation.compare(candidates, "picture|noun|1", :patient,
      server: server, propagation?: false)

    assert on.status == :shadow_only
    assert on.supported_count == 1
    assert on.signal_only_order == ["draws|verb|1", "draws|verb|2", "draws|verb|3"]
    assert on.applied_to_lifg? == false
    assert on.brain_cells_stimulated? == false
    assert on.learned_meaning_verified? == false

    first = Enum.find(on.candidates, &(&1.id == "draws|verb|1"))
    second = Enum.find(on.candidates, &(&1.id == "draws|verb|2"))
    assert_in_delta first.shadow_signal, 0.0025, 1.0e-10
    assert_in_delta first.reviewed_signal, 0.0, 1.0e-10
    assert_in_delta second.shadow_signal, 0.0, 1.0e-10
    assert off.status == :propagation_off
    assert off.baseline_candidate_order == candidates
    assert off.supported_count == 0
    assert Enum.all?(off.candidates, &(abs(&1.shadow_signal) < 1.0e-10))

    wrong_patient = Propagation.compare(candidates, "water|noun|1", :patient,
      server: server)
    assert wrong_patient.supported_count == 0
    wrong_role = Propagation.compare(candidates, "picture|noun|1", :actor,
      server: server)
    assert wrong_role.supported_count == 0
    correct_actor = Propagation.compare(candidates, "artist|noun|1", :actor,
      server: server)
    assert correct_actor.supported_count == 1
  end

  test "reviewed experience has stronger signal; rejection reverses it", %{server: server} do
    observe = %{
      provenance: :user_reported,
      evidence: "unique-review-input",
      edges: [
        {"draws|verb|4", "sketcher|noun|1", :actor},
        {"draws|verb|4", "canvas|noun|1", :patient}
      ]
    }

    assert {:ok, :recorded} = Shadow.observe(observe, server)
    signal = fn ->
      Propagation.compare(["draws|verb|4"], "canvas|noun|1", :patient,
        server: server).candidates |> hd()
    end

    assert_in_delta signal.().shadow_signal, 0.0025, 1.0e-10

    review = %{provenance: :human_review, reviewer: "curator",
      evidence: observe.evidence, decision: :confirm}

    assert {:ok, :confirmed} = Shadow.review(review, server)
    assert_in_delta signal.().shadow_signal, 0.03, 1.0e-10
    assert_in_delta signal.().provisional_signal, 0.0, 1.0e-10
    assert_in_delta signal.().reviewed_signal, 0.03, 1.0e-10

    assert {:ok, :rejected} =
             Shadow.review(%{review | decision: :reject}, server)
    assert_in_delta signal.().shadow_signal, 0.0, 1.0e-10
    assert_in_delta signal.().reviewed_signal, 0.0, 1.0e-10
  end

  test "support is capped even with many distinct contextual reports", %{server: server} do
    for n <- 1..20 do
      assert {:ok, :recorded} = learn(server, "draws|verb|5",
        "artist#{n}|noun|1", "picture|noun|1", "distinct-#{n}")
    end

    report = Propagation.compare(["draws|verb|5"], "picture|noun|1", :patient,
      server: server)
    assert [%{shadow_signal: signal, provisional_contexts: 20}] = report.candidates
    # A 0.08 cap on the edge, further scaled down for provisional reports.
    assert_in_delta signal, 0.02, 1.0e-10
    assert report.supported_count == 1
  end

  test "accepts large bounded exact-sense slates but refuses oversized or invented ones", %{server: server} do
    many = Enum.map(1..138, fn n -> "draws|verb|#{n}" end)
    result = Propagation.compare(many, "picture|noun|1", :patient, server: server)
    assert result.status == :shadow_only
    assert length(result.candidates) == 138
    assert result.supported_count == 0

    too_many = Enum.map(1..257, fn n -> "draws|verb|#{n}" end)
    assert %{status: :invalid_or_oversized_slate} =
             Propagation.compare(too_many, "picture|noun|1", :patient, server: server)
    assert %{status: :invalid_or_oversized_slate} =
             Propagation.compare(["imaginary|noun|1"], "picture|noun|1",
               :patient, server: server)
    assert %{status: :unavailable} =
             Propagation.compare(["draws|verb|1"], "not-a-real-id", :patient,
               server: server)
  end

  test "snapshot and exact edge remain unchanged across both A/B paths", %{server: server} do
    assert {:ok, :recorded} =
             learn(server, "draws|verb|9", "writer|noun|1",
               "novel|noun|1", "no-mutation")

    before = Shadow.snapshot(server)
    edge = Shadow.edge("draws|verb|9", "novel|noun|1", :patient, server)

    for mode <- [true, false], _ <- 1..4 do
      result = Propagation.compare(["draws|verb|9"], "novel|noun|1",
        :patient, server: server, propagation?: mode)
      assert result.applied_to_lifg? == false
    end

    assert Shadow.snapshot(server) == before
    assert Shadow.edge("draws|verb|9", "novel|noun|1", :patient, server) == edge
  end
end
