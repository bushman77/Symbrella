# apps/brain/test/brain/hippocampus_behavior_test.exs
defmodule Brain.HippocampusBehaviorTest do
  use ExUnit.Case, async: false
  alias Brain.Hippocampus

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Brain.Hippocampus.reset()
    :ok
  end

  defp slate_with(word), do: %{winners: [%{id: "#{word}|noun|0", lemma: word}]}

  defp slate_multi(words) when is_list(words) do
    %{winners: Enum.map(words, fn w -> %{id: "#{w}|noun|0", lemma: w} end)}
  end

  test "dedup-on-write: same token set refreshes head timestamp without growing window" do
    # 1st write
    Hippocampus.encode(slate_with("same"), %{first: true})
    %{window: [{at1, ep1} | _]} = Hippocampus.snapshot()

    # meta on first write includes first: true (and may include emotion, latents, etc.)
    assert %{first: true} = ep1.meta

    # 2nd write with identical token set but different meta → should dedup & refresh timestamp
    Process.sleep(10)
    Hippocampus.encode(slate_with("same"), %{first: false})

    s2 = Hippocampus.snapshot()
    assert length(s2.window) == 1

    [{at2, ep2} | _] = s2.window
    assert at2 > at1

    # Meta remains the original head's meta (dedup keeps head episode, updates only timestamp)
    # Accepts extra keys, but still enforces first: true and emotional context
    assert %{first: true} = ep2.meta
    assert Map.has_key?(ep2.meta, :emotion)
    assert Map.has_key?(ep2.meta, :latents)
    assert Map.has_key?(ep2.meta, :tone_reaction)
  end

  test "scope filter: recall only returns episodes whose meta contain the given scope" do
    # IMPORTANT: Insert a spacer so dedup-on-write doesn't collapse the second "alpha".
    Hippocampus.encode(slate_with("alpha"), %{conv_id: 1})
    Hippocampus.encode(slate_with("spacer"), %{conv_id: 999})
    Hippocampus.encode(slate_with("alpha"), %{conv_id: 2})
    Hippocampus.encode(slate_with("beta"), %{conv_id: 1})

    # Scope conv_id: 1 → only the alpha in scope 1 overlaps with cue "alpha"
    res1 = Hippocampus.recall(["alpha"], limit: 5, scope: %{conv_id: 1})
    assert length(res1) == 1
    [%{episode: %{meta: %{conv_id: 1}}}] = res1

    # Scope conv_id: 2 → only the alpha in scope 2
    res2 = Hippocampus.recall(["alpha"], limit: 5, scope: %{conv_id: 2})
    assert length(res2) == 1
    [%{episode: %{meta: %{conv_id: 2}}}] = res2

    # Scope conv_id: 3 → no matches
    res3 = Hippocampus.recall(["alpha"], limit: 5, scope: %{conv_id: 3})
    assert res3 == []
  end

  test "min_jaccard: threshold excludes low-overlap even when recent" do
    # Episode tokens = {alpha, beta}; cue = {alpha}
    # Jaccard = 1 / 2 = 0.5
    Hippocampus.encode(slate_multi(["alpha", "beta"]), %{})

    # With threshold > 0.5, result should be filtered out
    res_high = Hippocampus.recall(["alpha"], min_jaccard: 0.6, limit: 5)
    assert res_high == []

    # With threshold < 0.5, result should be included
    res_low = Hippocampus.recall(["alpha"], min_jaccard: 0.4, limit: 5)
    assert length(res_low) == 1
    assert hd(res_low).score > 0.0
  end
end

