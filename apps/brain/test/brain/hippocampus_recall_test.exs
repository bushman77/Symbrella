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

    Hippocampus.encode(slate_with("a"), %{})
    Hippocampus.encode(slate_with("a"), %{})
    Hippocampus.encode(slate_with("b"), %{})

    # No overlap → empty
    assert Hippocampus.recall(["zzz"]) == []

    # Overlap with 'a' → 2 results due to default recall_limit
    res = Hippocampus.recall(["a"])

    assert length(res) == 2
    assert Enum.all?(res, fn r -> r.score > 0.0 end)
  end
end
