defmodule Db.EpisodeKnnTest do
  use ExUnit.Case, async: false

  alias Db.Episode
  alias Ecto.Adapters.SQL.Sandbox

  @repo Db
  @embedding_dim Application.compile_env(:db, :embedding_dim, 1536)

  setup do
    # test_helper.exs should already set Sandbox.mode(Db, :manual)
    # Here we just checkout a connection for this test process.
    :ok = Sandbox.checkout(@repo)
    :ok
  end

  test "knn/2 returns the inserted episode for a matching embedding" do
    # Simple unit-like vector so cosine distance should be ~0.0 for self-match
    embedding = [1.0 | List.duplicate(0.0, @embedding_dim - 1)]

    si = %{
      "slate" => %{
        "winners" => [],
        "tokens" => ["alpha"]
      },
      "meta" => %{
        "route" => "test",
        "keyword" => "alpha"
      }
    }

    {:ok, ep} =
      Episode.insert(%{
        si: si,
        tokens: ["alpha"],
        tags: ["test-episode"],
        embedding: embedding
      })

    results = Episode.knn(embedding, k: 5)

    # We should at least see our inserted row
    assert Enum.any?(results, &(&1.id == ep.id))

    row = Enum.find(results, &(&1.id == ep.id))

    # Basic shape checks to match Brain.Hippocampus.DB expectations
    assert is_map(row)
    assert Map.has_key?(row, :id)
    assert Map.has_key?(row, :si)
    assert Map.has_key?(row, :tags)
    assert Map.has_key?(row, :inserted_at)
    assert Map.has_key?(row, :distance)

    assert row.id == ep.id
    assert is_map(row.si)
    assert is_list(row.tags)
    assert is_struct(row.inserted_at, NaiveDateTime)
    assert is_number(row.distance)
    assert row.distance >= 0.0

    # For a self-match, distance should be reasonably small
    assert row.distance <= 0.01
  end
end
