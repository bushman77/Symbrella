defmodule Brain.Hippocampus.SemanticAssociationWarmStartTest do
  use ExUnit.Case, async: true

  alias Brain.Hippocampus
  alias Brain.Hippocampus.{Recall, SemanticAssociations}

  test "rehydrates the original proof pointer from a persisted episode after a cold start" do
    fingerprint = String.duplicate("a", 64)

    # Db.Episode.si is virtual, so PostgreSQL restores an empty map here.
    # The opaque association fields survive in the persisted meta column.
    persisted_row = %{
      id: 42,
      tokens: ["vancouver", "burnaby"],
      tags: ["semantic_association"],
      meta: %{
        "kind" => "semantic_association",
        "key" => "population_comparison",
        "value" => fingerprint
      },
      si: %{}
    }

    episode = Hippocampus.db_row_to_episode(persisted_row)

    assert episode.meta.kind == "semantic_association"
    assert episode.meta.key == "population_comparison"
    assert episode.meta.value == fingerprint
    assert MapSet.equal?(episode.norms, MapSet.new(["vancouver", "burnaby"]))

    now = System.system_time(:millisecond)

    {hits, _, _} =
      Recall.run(["vancouver", "coquitlam"], [{now, episode}],
        scope: %{kind: "semantic_association"},
        limit: 12,
        ignore_head: :never,
        min_jaccard: 0.01
      )

    assert SemanticAssociations.extract_fingerprints(hits) == [fingerprint]
  end

  test "ordinary persisted episodes never become semantic proof associations" do
    fingerprint = String.duplicate("b", 64)

    row = %{
      tokens: ["vancouver", "burnaby"],
      tags: ["conversation"],
      meta: %{
        "kind" => "conversation",
        "key" => "population_comparison",
        "value" => fingerprint
      },
      si: %{}
    }

    episode = Hippocampus.db_row_to_episode(row)
    assert SemanticAssociations.extract_fingerprints([%{episode: episode}]) == []
  end
end
