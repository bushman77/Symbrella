defmodule Brain.PMTG.OverlapTest do
  use ExUnit.Case, async: true

  alias Brain.PMTG
  alias Brain.PMTG.Overlap

  defp tok(index, phrase, span, opts \\ []) do
    %{index: index, token_index: index, phrase: phrase, span: span,
      n: Keyword.get(opts, :n, 1), mw: Keyword.get(opts, :mw, false)}
  end

  defp choice(index, lemma, pos \\ "noun", opts \\ []) do
    %{token_index: index, lemma: lemma,
      chosen_id: "#{lemma}|#{pos}|1", alt_ids: [], margin: 0.0, weak?: true}
    |> Map.merge(Map.new(opts))
  end

  test "distance question consults complete MWEs but not their covered unigrams" do
    tokens = [
      tok(0, "how", {0, 3}), tok(9, "how far", {0, 7}, n: 2, mw: true),
      tok(1, "far", {4, 7}), tok(2, "is", {8, 10}),
      tok(3, "kamloops", {11, 19}), tok(4, "bc", {20, 22}),
      tok(5, "from", {23, 27}), tok(6, "maple", {28, 33}),
      tok(10, "maple ridge", {28, 39}, n: 2, mw: true),
      tok(7, "ridge", {34, 39}), tok(8, "bc", {40, 42})
    ]

    need = [
      choice(0, "how", "adverb"), choice(9, "how far", "adverb"),
      choice(1, "far", "proper_noun"), choice(3, "kamloops", "proper_noun"),
      choice(4, "bc", "proper_noun"), choice(6, "maple", "proper_noun"),
      choice(10, "maple ridge", "proper_noun"), choice(7, "ridge", "proper_noun"),
      choice(8, "bc", "proper_noun")
    ]

    {kept, suppressed} = Overlap.prune(need, tokens)
    assert Enum.map(kept, & &1.token_index) == [9, 3, 4, 10, 8]
    assert suppressed == [0, 1, 6, 7]

    assert {:ok, %{queries: queries, audit: audit, si: si}} =
      PMTG.resolve(%{tokens: tokens, trace: []}, need)

    assert Enum.map(queries, & &1.token_index) == [9, 3, 4, 10, 8]
    assert audit.needy_count == 5
    assert audit.suppressed_overlap_count == 4
    assert [%{meta: %{suppressed_overlap_count: 4, planned_queries: 5}}] = si.trace
  end

  test "never suppress distinct occurrences, uncovered tokens or explicitly disputed words" do
    tokens = [
      tok(0, "maple", {0, 5}),
      tok(1, "maple ridge", {0, 11}, n: 2, mw: true),
      tok(2, "ridge", {6, 11}),
      tok(3, "ridge", {25, 30})
    ]

    choices = [
      choice(0, "maple"),
      choice(1, "maple ridge", "proper_noun"),
      choice(2, "ridge", "noun", veto?: true),
      choice(3, "ridge", "noun")
    ]

    {kept, suppressed} = Overlap.prune(choices, tokens)
    assert Enum.map(kept, & &1.token_index) == [1, 2, 3]
    assert suppressed == [0]
  end

  test "unverified, missing-span or fallback MWE cannot suppress content words" do
    words = [tok(0, "river", {0, 5}), tok(1, "bank", {6, 10})]
    raw_mwe = tok(2, "river bank", {0, 10}, n: 2, mw: true)
    choices = [choice(0, "river"), choice(1, "bank"), choice(2, "river bank", "noun")]

    {kept, _} = Overlap.prune(choices, words ++ [Map.put(raw_mwe, :span, nil)])
    assert length(kept) == 3

    {kept, _} =
      Overlap.prune(
        List.replace_at(choices, 2, Map.put(Enum.at(choices, 2), :chosen_id, "river bank|phrase|fallback")),
        words ++ [raw_mwe]
      )
    assert length(kept) == 3

    {kept, _} = Overlap.prune(choices, words ++ [Map.merge(raw_mwe, %{mw: false, n: 1})])
    assert length(kept) == 3
  end
end
