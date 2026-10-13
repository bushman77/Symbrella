defmodule Brain.HippocampusRecallTest do
  use ExUnit.Case, async: false

  alias Brain.Hippocampus

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Brain.Hippocampus.reset()
    :ok
  end

  defp slate_with(word) do
    %{winners: [%{id: "#{word}|noun|0", lemma: word}]}
  end

  defp slate_multi(words) when is_list(words) do
    %{
      winners:
        Enum.map(words, fn w ->
          %{id: "#{w}|noun|0", lemma: w}
        end)
    }
  end

  test "recall prefers overlapping episode (Jaccard > 0)" do
    Hippocampus.encode(slate_with("hello"), %{})
    Hippocampus.encode(slate_with("world"), %{})

    results = Hippocampus.recall(["hello"])
    assert length(results) == 1

    [%{episode: %{slate: %{winners: [%{lemma: "hello"}]}}}] = results
    assert hd(results).score > 0.0
  end

  test "recency half-life breaks ties in favor of newer episodes" do
    # Short half-life to amplify time effect
    Hippocampus.configure(half_life_ms: 50, window_keep: 10)

    # Same content, different times
    Hippocampus.encode(slate_with("foo"), %{})
    Process.sleep(10)
    Hippocampus.encode(slate_with("foo"), %{})

    [%{at: at_new} | _] = Hippocampus.recall(["foo"], limit: 2)

    # The first result should be the newest episode (greater timestamp)
    [first_entry | _] = Hippocampus.snapshot().window
    {at_top, _} = first_entry
    assert at_new == at_top
  end

  test "limit is respected and zero-overlap results are filtered out" do
    Hippocampus.reset()
    Hippocampus.configure(window_keep: 10, recall_limit: 2)

    # Two distinct episodes that both overlap "a" but are not deduped
    Hippocampus.encode(slate_multi(["a", "x"]), %{})
    Hippocampus.encode(slate_multi(["a", "y"]), %{})

    # A pure distractor that should never be returned for cue "a"
    Hippocampus.encode(slate_with("b"), %{})

    # No overlap → empty
    assert Hippocampus.recall(["zzz"]) == []

    # Overlap with 'a' → 2 results due to recall_limit and Jaccard > 0
    res = Hippocampus.recall(["a"])

    assert length(res) == 2
    assert Enum.all?(res, fn r -> r.score > 0.0 end)
  end

  test "ignore_head :auto skips the current map-shaped context when it exactly matches" do
    Hippocampus.encode(slate_with("alpha"), %{})

    assert Hippocampus.recall(slate_with("alpha"), ignore_head: :auto) == []

    results = Hippocampus.recall(slate_with("alpha"), ignore_head: :never)
    assert length(results) == 1
  end

  test "score is exact Jaccard overlap times recency" do
    Hippocampus.encode(slate_multi(["alpha", "beta"]), %{})

    [%{score: score}] = Hippocampus.recall(["alpha"], ignore_head: :never, limit: 1)

    assert_in_delta score, 0.5, 0.0001
  end
  test "attach_episodes uses caller-provided lexical cues instead of an SI without winners" do
    Hippocampus.encode(slate_with("river"), %{scope: :conversation})

    # Core.SemanticInput holds its lexical evidence in :tokens, not :winners.
    # This is the shape Core.Recall.Execute passes for episodic attachment.
    si = %{sentence: "What happened by the river?", tokens: [%{phrase: "river"}], evidence: %{}}

    # Without normalized cues the legacy slate-only lookup has no winners.
    assert %{evidence: %{episodes: []}} =
             Hippocampus.attach_episodes(si, limit: 3, min_jaccard: 0.1)

    # The :cues provided by Core.Recall.Execute must drive retrieval.
    result =
      Hippocampus.attach_episodes(si,
        cues: ["river"],
        limit: 3,
        min_jaccard: 0.1
      )

    assert [%{score: score, episode: %{slate: %{winners: [%{lemma: "river"}]}}}] =
             result.evidence.episodes

    assert score > 0.0
    assert result.sentence == si.sentence
    assert result.tokens == si.tokens
  end

  test "attach_episodes keeps existing slate winner fallback when no explicit cues are passed" do
    Hippocampus.encode(slate_with("orchard"), %{})

    si = %{winners: [%{lemma: "orchard"}], evidence: %{}}
    assert [%{score: score}] = Hippocampus.attach_episodes(si).evidence.episodes
    assert score > 0.0
  end

end
