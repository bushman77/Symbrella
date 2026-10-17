defmodule Brain.PMTG.TokenIdentityTest do
  use ExUnit.Case, async: true

  alias Brain.PMTG

  test "lemma matches the choice token index when MWE indices are interleaved" do
    # Token indices are semantic identities, not list offsets. In a live
    # sentence, an MWE can have index 9 and appear second in the list.
    tokens = [
      %{index: 0, phrase: "how"},
      %{index: 9, phrase: "how far", n: 2, mw: true},
      %{index: 1, phrase: "far"},
      %{index: 2, phrase: "is"},
      %{index: 3, phrase: "maple"},
      %{index: 10, phrase: "maple ridge", n: 2, mw: true},
      %{index: 4, phrase: "ridge"},
      %{index: 5, phrase: "is"},
      %{index: 6, phrase: "from"},
      %{index: 7, phrase: "kamloops"},
      %{index: 8, phrase: "bc"}
    ]

    needy = [
      %{token_index: 9, chosen_id: "how far|adverb|1", alt_ids: []},
      %{token_index: 10, chosen_id: "maple ridge|proper_noun|1", alt_ids: []},
      %{token_index: 7, chosen_id: "kamloops|proper_noun|1", alt_ids: []},
      %{token_index: 8, chosen_id: "bc|proper_noun|1", alt_ids: []}
    ]

    assert {:ok, %{queries: queries}} = PMTG.resolve(%{tokens: tokens, trace: []}, needy)

    assert Enum.map(queries, &{&1.token_index, &1.lemma, &1.chosen_id}) == [
      {9, "how far", "how far|adverb|1"},
      {10, "maple ridge", "maple ridge|proper_noun|1"},
      {7, "kamloops", "kamloops|proper_noun|1"},
      {8, "bc", "bc|proper_noun|1"}
    ]
  end

  test "missing token identities fall back to the choice lemma, not another token" do
    tokens = [%{index: 0, phrase: "hello"}, %{index: 4, phrase: "world"}]
    needy = [
      %{token_index: 1, lemma: "unknown", chosen_id: "unknown|noun|1", alt_ids: []}
    ]

    assert {:ok, %{queries: [%{token_index: 1, lemma: "unknown"}]}} =
             PMTG.resolve(%{tokens: tokens, trace: []}, needy)
  end

  test "string-keyed indexed tokens keep their correct lexeme" do
    tokens = [%{"index" => 3, "phrase" => "maple ridge"}, %{"index" => 0, "phrase" => "where"}]
    needy = [%{token_index: 3, chosen_id: "maple ridge|proper_noun|1", alt_ids: []}]

    assert {:ok, %{queries: [%{token_index: 3, lemma: "maple ridge"}]}} =
             PMTG.resolve(%{tokens: tokens, trace: []}, needy)
  end
end
