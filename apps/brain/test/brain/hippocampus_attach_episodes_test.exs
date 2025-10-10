defmodule Brain.HippocampusAttachEpisodesTest do
  use ExUnit.Case, async: false

  alias Brain.Hippocampus

  #
  # ───────────────────────────── Setup ─────────────────────────────
  #

  setup_all do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    :ok
  end

  setup do
    Hippocampus.reset()
    :ok
  end

  #
  # ───────────────────────────── Helpers ───────────────────────────
  #

  defp slate_with(word), do: %{winners: [%{id: "#{word}|noun|0", lemma: word}]}

  defp slate_with_many(words) when is_list(words) do
    %{winners: Enum.map(words, &%{id: "#{&1}|noun|0", lemma: &1})}
  end

  defp episodes_from(si, opts \\ []) do
    si
    |> Hippocampus.attach_episodes(Keyword.merge([limit: 5], opts))
    |> then(&get_in(&1, [:evidence, :episodes]) || [])
  end

  defp has_winner_lemma?(%{episode: %{slate: %{winners: ws}}}, lemma) when is_list(ws) do
    Enum.any?(ws, fn w -> (w[:lemma] || w["lemma"]) == lemma end)
  end

  defp all_scope?(episodes, scope) when is_map(scope) do
    Enum.all?(episodes, fn
      %{episode: %{meta: meta}} ->
        (meta[:tenant] || meta["tenant"]) == scope[:tenant]
      _ ->
        false
    end)
  end

  #
  # ───────────────────────────── Tests ─────────────────────────────
  #

  test "attach_episodes/2 writes episodes into si.evidence[:episodes] with proper shape" do
    Hippocampus.encode(slate_with("alpha"), %{})
    Hippocampus.encode(slate_with("beta"), %{})

    episodes =
      %{winners: [%{lemma: "alpha", id: "alpha|noun|0"}]}
      |> episodes_from(limit: 3)

    assert is_list(episodes)
    assert length(episodes) >= 1

    [%{score: score, at: at, episode: ep} | _] = episodes
    assert is_float(score)
    assert is_integer(at)
    assert is_map(ep)
    assert %{slate: _slate_map, meta: _meta_map} = ep
  end

  test "attach_episodes/2 honors :scope filter" do
    # two episodes, same winners, different tenant
    Hippocampus.encode(slate_with("alpha"), %{tenant: "a"})
    Hippocampus.encode(slate_with("alpha"), %{tenant: "b"})

    scope = %{tenant: "a"}

    episodes =
      %{winners: [%{lemma: "alpha", id: "alpha|noun|0"}]}
      |> episodes_from(scope: scope, limit: 5)

    assert is_list(episodes)
    assert length(episodes) >= 1
    assert all_scope?(episodes, scope)
  end

  test "attach_episodes/2 honors :min_jaccard threshold" do
    # winners = ["alpha","x","y"] has J=1/3 w.r.t. cues ["alpha"]
    Hippocampus.encode(slate_with_many(["alpha", "x", "y"]), %{})
    # totally disjoint episode
    Hippocampus.encode(slate_with("zzz"), %{})

    si = %{winners: [%{lemma: "alpha", id: "alpha|noun|0"}]}

    eps_low = episodes_from(si, min_jaccard: 0.0, limit: 5)
    assert is_list(eps_low)
    assert Enum.any?(eps_low, &has_winner_lemma?(&1, "x"))

    eps_high = episodes_from(si, min_jaccard: 0.5, limit: 5)
    refute Enum.any?(eps_high, &has_winner_lemma?(&1, "x"))
  end
end

