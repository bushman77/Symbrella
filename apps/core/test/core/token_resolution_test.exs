defmodule Core.TokenResolutionTest do
  use ExUnit.Case, async: true

  alias Core.{SemanticInput, TokenFilters, TokenResolution}
  alias Core.MWE.Signatures

  test "resolves POS signature phrase into a non-overlapping token cover" do
    si =
      %SemanticInput{sentence: "open the file now", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Map.put(:active_cells, [
        %{norm: "open", pos: "verb"},
        %{norm: "the", pos: "determiner"},
        %{norm: "file", pos: "noun"},
        %{norm: "now", pos: "adverb"}
      ])
      |> Signatures.run(stage: :late)
      |> TokenResolution.resolve()

    assert [%{phrase: "open the file", kind: :phrase}, %{phrase: "now", kind: :token}] =
             si.token_cover

    assert si.resolved_tokens == si.token_cover
    assert [%{stage: :token_resolution, decision: :resolved, meta: meta} | _] = si.trace
    assert meta.cover_count == 2
    assert meta.phrase_count == 1
  end

  test "falls back to base tokens when no phrase signature exists" do
    si =
      %SemanticInput{sentence: "blue quiet lamp", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> TokenResolution.resolve()

    assert Enum.map(si.token_cover, & &1.phrase) == ["blue", "quiet", "lamp"]
    assert Enum.all?(si.token_cover, &(&1.kind == :token))
    assert [%{stage: :token_resolution, decision: :base_tokens} | _] = si.trace
  end

  test "can be disabled with opts" do
    si =
      %SemanticInput{sentence: "open the file", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> TokenResolution.resolve(token_resolution: :off)

    refute Map.get(si, :token_cover)
    refute Map.get(si, :resolved_tokens)
  end
end
