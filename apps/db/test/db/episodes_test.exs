defmodule Db.TestEmbedder do
  @moduledoc false
@dim Application.compile_env(:db, :embedding_dim, 1536)

  # Deterministic, fast, and dimension-stable for tests
  def embed(text) when is_binary(text) do
    seed = :erlang.phash2(text)
    vec = for i <- 1..@dim, do: :math.sin(seed + i)
    {:ok, vec}
  end
end

defmodule Db.EpisodesTest do
  use ExUnit.Case, async: false

  alias Db.Episodes
  alias Db.Episode

  setup_all do
    old_embedder = Application.get_env(:db, :embedder)
    Application.put_env(:db, :embedder, Db.TestEmbedder)

    on_exit(fn -> Application.put_env(:db, :embedder, old_embedder) end)
    :ok
  end

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    Ecto.Adapters.SQL.Sandbox.mode(Db, {:shared, self()})
    :ok
  end

  test "write_episode/2 (sync) normalizes tokens and stores embedding" do
    si = %{
      tokens: ["Order", "Adapter", "Maple"],
      sentence: "Order adapter maple",
      intent: "purchase"
    }

    assert {:ok, %Episode{} = ep} = Episodes.write_episode(si)
    assert ep.tokens == ["order", "adapter", "maple"]
    assert ep.token_count == 3
    refute is_nil(ep.embedding)
  end

  test "write_episode/2 (async) inserts without embedding, update_embedding/2 patches later" do
    si = %{
      tokens: ["invoice", "quote"],
      sentence: "need invoice and a quote",
      intent: "billing"
    }

    assert {:ok, %Episode{} = ep} = Episodes.write_episode(si, async_embedding: true)
    assert is_nil(ep.embedding)

    {:ok, emb} = Db.TestEmbedder.embed(Episodes.si_text_for_embedding(ep.si))
    assert {:ok, %Episode{} = ep2} = Episodes.update_embedding(ep.id, emb)
    refute is_nil(ep2.embedding)
  end

  test "recall_hybrid/3 returns episode via token-only when embedding is nil" do
    si = %{
      tokens: ["alpha", "beta", "gamma"],
      sentence: "alpha beta gamma",
      intent: "note"
    }

    {:ok, %Episode{} = _ep} = Episodes.write_episode(si, async_embedding: true)

    results = Episodes.recall_hybrid(["alpha", "beta"], nil, top_k: 5)
    assert is_list(results)
    assert Enum.any?(results, fn r -> r.jaccard > 0 end)
  end

  test "recall_hybrid/3 (token + vector) ranks the matching episode highest" do
    # Non-matching background episode
    {:ok, _} =
      Episodes.write_episode(%{tokens: ["car", "engine"], sentence: "car engine trouble"})

    # Strong match episode
    {:ok, %Episode{} = ep} =
      Episodes.write_episode(%{
        tokens: ["order", "adapter", "maple", "ridge"],
        sentence: "order adapter in maple ridge",
        intent: "purchase"
      })

    cues = ["order", "adapter", "maple"]

    # Let the function embed internally using the configured test embedder
    results = Episodes.recall_hybrid(cues, "order adapter maple", top_k: 5)

    assert [%{id: top_id} | _] = results
    assert top_id == ep.id

    # Score sanity: positive jaccard, vec_sim present or computed
    top = hd(results)
    assert top.jaccard > 0.0
    assert is_number(top.score)
  end
end
