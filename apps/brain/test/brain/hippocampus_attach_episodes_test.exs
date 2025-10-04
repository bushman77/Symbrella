defmodule Brain.HippocampusAttachEpisodesTest do
  use ExUnit.Case, async: false

  alias Brain.Hippocampus

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Hippocampus.reset()
    :ok
  end

  defp slate_with(word) do
    %{winners: [%{id: "#{word}|noun|0", lemma: word}]}
  end

  defp slate_with_many(words) when is_list(words) do
    %{
      winners:
        Enum.map(words, fn w ->
          %{id: "#{w}|noun|0", lemma: w}
        end)
    }
  end

  test "attach_episodes/2 writes episodes into si.evidence[:episodes] with proper shape" do
    Hippocampus.encode(slate_with("alpha"), %{})
    Hippocampus.encode(slate_with("beta"), %{})

    si0 = %{winners: [%{lemma: "alpha", id: "alpha|noun|0"}]}
    si1 = Hippocampus.attach_episodes(si0, limit: 3)

    assert is_map(si1)
    assert is_map(si1[:evidence])

    episodes = si1[:evidence][:episodes]
    assert is_list(episodes)
    assert length(episodes) >= 1

    # Shape: %{score: float, at: int, episode: %{slate: map, meta: map}}
    [%{score: score, at: at, episode: ep} | _] = episodes
    assert is_float(score)
    assert is_integer(at)
    assert is_map(ep)
    assert %{slate: _slate_map, meta: _meta_map} = ep
  end

  test "attach_episodes/2 honors :scope filter" do
    # Two otherwise-identical episodes but different scope in meta
    Hippocampus.encode(slate_with("alpha"), %{tenant: "a"})
    Hippocampus.encode(slate_with("alpha"), %{tenant: "b"})

    si = %{winners: [%{lemma: "alpha", id: "alpha|noun|0"}]}

    # Scope to tenant "a" → only those episodes should pass
    si_scoped = Hippocampus.attach_episodes(si, scope: %{tenant: "a"}, limit: 5)

    episodes = si_scoped[:evidence][:episodes]
    assert is_list(episodes)
    assert length(episodes) >= 1

    # All returned must match the scoped tenant
    assert Enum.all?(episodes, fn %{episode: %{meta: meta}} ->
             (meta[:tenant] || meta["tenant"]) == "a"
           end)
  end

  test "attach_episodes/2 honors :min_jaccard threshold" do
    # Episode with partial overlap: winners = ["alpha", "x", "y"] → J = 1/3 with cues ["alpha"]
    Hippocampus.encode(slate_with_many(["alpha", "x", "y"]), %{})
    # Non-overlapping (will be filtered by score>0 anyway)
    Hippocampus.encode(slate_with("zzz"), %{})

    si = %{winners: [%{lemma: "alpha", id: "alpha|noun|0"}]}

    # With a low threshold, the partial-overlap episode should appear
    si_low = Hippocampus.attach_episodes(si, min_jaccard: 0.0, limit: 5)
    eps_low = si_low[:evidence][:episodes]
    assert is_list(eps_low)
    assert Enum.any?(eps_low, fn %{episode: %{slate: %{winners: ws}}} ->
             Enum.any?(ws, &((&1[:lemma] || &1["lemma"]) == "x"))
           end)

    # With a higher threshold, 1/3 ≈ 0.333 should be filtered out
    si_high = Hippocampus.attach_episodes(si, min_jaccard: 0.5, limit: 5)
    eps_high = si_high[:evidence][:episodes]

    # Either empty or none contain the multi-token episode
    refute Enum.any?(eps_high, fn %{episode: %{slate: %{winners: ws}}} ->
             Enum.any?(ws, &((&1[:lemma] || &1["lemma"]) == "x"))
           end)
  end
end

