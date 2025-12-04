defmodule Brain.LIFG.Stage1SpanInvariantTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Guard
  alias Brain.LIFG.Stage1

  @sentence "good evening symbrella"

  @tokens [
    %{index: 0, span: {0, 22}, n: 3, phrase: "good evening symbrella", mw: true},
    %{index: 1, span: {0, 12}, n: 2, phrase: "good evening", mw: true},
    %{index: 2, span: {0, 4}, n: 1, phrase: "good", mw: false},
    %{index: 3, span: {5, 17}, n: 2, phrase: "evening symbrella", mw: true},
    %{index: 4, span: {5, 7}, n: 1, phrase: "evening", mw: false},
    %{index: 5, span: {13, 9}, n: 1, phrase: "symbrella", mw: false}
  ]

  describe "Guard span normalization" do
    test "normalizes {start, len} (and mismatched stop) spans into {start, stop} consistent with phrase length" do
      %{tokens: normed} =
        Guard.sanitize(%{
          sentence: @sentence,
          tokens: @tokens
        })

      spans = Enum.map(normed, &{&1.index, &1.span})

      assert spans == [
               {0, {0, 22}},
               {1, {0, 12}},
               {2, {0, 4}},
               {3, {5, 22}},  # "evening symbrella" is 17 bytes → 5 + 17 = 22
               {4, {5, 12}},  # "evening" is 7 bytes → 5 + 7 = 12
               {5, {13, 22}}  # "symbrella" is 9 bytes → 13 + 9 = 22
             ]

      # Strong invariant: normalized span slice should match phrase
      Enum.each(normed, fn tok ->
        assert slice(@sentence, tok.span) == tok.phrase
      end)
    end
  end

  describe "Stage1 scoring with full candidate coverage" do
    test "keeps all tokens (including good and symbrella) when candidates exist" do
      si = %{
        sentence: @sentence,
        tokens: @tokens,
        intent: :greet,
        confidence: 0.7,
        sense_candidates: %{
          0 => [%{id: "good evening symbrella|phrase|fallback", lemma: "good evening symbrella", pos: "phrase"}],
          1 => [%{id: "good evening|phrase|fallback", lemma: "good evening", pos: "phrase"}],
          2 => [%{id: "good|adj|0", lemma: "good", pos: "adj"}],
          3 => [%{id: "evening symbrella|phrase|fallback", lemma: "evening symbrella", pos: "phrase"}],
          4 => [
            %{id: "evening|noun|0", lemma: "evening", pos: "noun"},
            %{id: "evening|verb|0", lemma: "evening", pos: "verb"}
          ],
          5 => [%{id: "symbrella|noun|0", lemma: "symbrella", pos: "noun"}]
        }
      }

      assert {:ok, %{choices: choices, audit: audit}} =
               Stage1.run(si,
                 scores: :all,
                 margin_threshold: 0.15,
                 min_margin: 0.05,
                 mwe_fallback: true
               )

      token_indices =
        choices
        |> Enum.map(& &1.token_index)
        |> Enum.sort()

      assert token_indices == [0, 1, 2, 3, 4, 5]

      assert audit.kept_tokens == 6
      assert audit.dropped_tokens == 0
      assert audit.boundary_drops == 0
      assert audit.chargram_violation == 0
    end
  end

  describe "Stage1 when candidates are missing" do
    test "does not boundary-drop tokens; it simply produces no choice for indices with no candidates" do
      # Reproduces the live symptom: tokens present, but only some indices have candidates.
      si = %{
        sentence: @sentence,
        tokens: @tokens,
        intent: :greet,
        confidence: 0.7,
        sense_candidates: %{
          0 => [%{id: "good evening symbrella|phrase|fallback", lemma: "good evening symbrella", pos: "phrase"}],
          1 => [%{id: "good evening|phrase|fallback", lemma: "good evening", pos: "phrase"}],
          3 => [%{id: "evening symbrella|phrase|fallback", lemma: "evening symbrella", pos: "phrase"}],
          4 => [
            %{id: "evening|noun|0", lemma: "evening", pos: "noun"},
            %{id: "evening|verb|0", lemma: "evening", pos: "verb"}
          ]
          # NOTE: index 2 ("good") and 5 ("symbrella") have no candidates on purpose
        }
      }

      assert {:ok, %{choices: choices, audit: audit}} =
               Stage1.run(si,
                 scores: :all,
                 margin_threshold: 0.15,
                 min_margin: 0.05,
                 mwe_fallback: true
               )

      token_indices =
        choices
        |> Enum.map(& &1.token_index)
        |> Enum.sort()

      assert token_indices == [0, 1, 3, 4]

      # The important diagnostic: nothing was dropped by boundary/chargram.
      assert audit.boundary_drops == 0
      assert audit.chargram_violation == 0

      # But total "dropped_tokens" counts missing-candidate tokens as dropped (via no_cand).
      assert audit.kept_tokens == 4
      assert audit.dropped_tokens == 2
    end
  end

  defp slice(sentence, {start, stop}) do
    len = max(stop - start, 0)
    String.slice(sentence, start, len)
  end
end

