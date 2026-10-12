defmodule Brain.ATL.StatusTest do
  use ExUnit.Case, async: false

  alias Brain.ATL

  setup do
    case Process.whereis(ATL) do
      nil -> start_supervised!({ATL, keep: 10})
      _pid -> :ok
    end

    ATL.reset()
    :ok
  end

  test "the ATL server retains constituent evidence with existing counters" do
    winners = [
      %{id: "the|determiner|1", lemma: "the", token_index: 0, score: 0.8},
      %{id: "garden|noun|1", lemma: "garden", token_index: 1, score: 0.8}
    ]

    words = [%{index: 0, phrase: "the"}, %{index: 1, phrase: "garden"}]

    signatures = [
      %{
        id: :noun_phrase,
        span: {0, 1},
        roles: %{determiner: 0, head: 1},
        required_features: %{determiner: [:pos_determiner], head: [:pos_noun]}
      }
    ]

    slate = ATL.ingest(winners, words, signatures)
    assert slate.winner_count == 2
    assert slate.constituent_count == 1
    assert [%{status: :resolved}] = slate.constituents

    snapshot = ATL.snapshot()
    assert snapshot.last_slate.constituents == slate.constituents
    assert snapshot.concept_counts == %{"garden" => 1, "the" => 1}
    assert snapshot.sense_counts == %{"garden|noun|1" => 1, "the|determiner|1" => 1}
    assert ATL.status().last_winner_count == 2

    existing = ATL.ingest(winners, words)
    refute Map.has_key?(existing, :constituents)
    assert existing.by_id == slate.by_id
  end

  test "status/0 returns compact dashboard state before and after ingest" do
    assert %{
             region: :atl,
             status: :up,
             window_count: 0,
             concept_count: 0,
             sense_count: 0,
             last_winner_count: 0
           } = ATL.status()

    ATL.ingest(
      [
        %{id: "alpha|noun|0", lemma: "alpha", token_index: 0, score: 0.9},
        %{id: "beta|noun|0", lemma: "beta", token_index: 1, score: 0.8}
      ],
      [%{phrase: "alpha"}, %{phrase: "beta"}]
    )

    assert %{
             region: :atl,
             status: :up,
             window_count: 1,
             concept_count: 2,
             sense_count: 2,
             last_winner_count: 2,
             last_concept_count: 2,
             last_sense_count: 2
           } = ATL.status()
  end
end
