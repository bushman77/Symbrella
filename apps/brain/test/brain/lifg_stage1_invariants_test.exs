# apps/brain/test/brain/lifg_stage1_invariants_test.exs
defmodule Brain.LIFG.Stage1InvariantsTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  @moduledoc false

  describe "boundary guard invariants" do
    test "drops in-word non-boundary substrings and records boundary_drops" do
      # "hat" starting inside "what" (no word boundary on the left)
      si = %{
        sentence: "what hat",
        tokens: [
          %{index: 0, phrase: "hat", span: {1, 4}, mw: false}
        ],
        sense_candidates: %{
          0 => [
            %{
              id: "hat|noun|0",
              lemma: "hat",
              pos: "noun",
              score: 0.5
            }
          ]
        }
      }

      assert {:ok, %{choices: choices, audit: audit}} = Stage1.run(si, [])

      # Token should be fully dropped before scoring
      assert choices == []

      assert audit.kept_tokens == 0
      assert audit.dropped_tokens == 1

      # Guard + Stage1 should agree that this index was rejected
      assert audit.boundary_drops == 1
      assert audit.rejected_by_boundary == [0]

      # At least one chargram_violation-style drop recorded in the audit
      assert audit.chargram_violation >= 1
    end

    test "keeps aligned unigram tokens with clean word boundaries" do
      si = %{
        sentence: "hello there",
        tokens: [
          # "hello" at the start of the sentence
          %{index: 0, phrase: "hello", span: {0, 5}, mw: false},
          # "there" after the space; Guard will normalize {6, 5} → {6, 11}
          %{index: 1, phrase: "there", span: {6, 5}, mw: false}
        ],
        sense_candidates: %{
          0 => [
            %{
              id: "hello|noun|0",
              lemma: "hello",
              pos: "noun",
              score: 0.7
            }
          ],
          1 => [
            %{
              id: "there|noun|0",
              lemma: "there",
              pos: "noun",
              score: 0.6
            }
          ]
        }
      }

      assert {:ok, %{choices: choices, audit: audit}} = Stage1.run(si, [])

      # Both tokens should survive Guard + boundary checks and get winners
      assert length(choices) == 2

      assert audit.kept_tokens == 2
      assert audit.dropped_tokens == 0
      assert audit.boundary_drops == 0
      assert audit.rejected_by_boundary == []
    end

    test "allows aligned MWE tokens when mw: true (no boundary drop)" do
      si = %{
        sentence: "hello there",
        tokens: [
          # Proper MWE spanning the whole sentence
          %{
            index: 0,
            phrase: "hello there",
            span: {0, 11},
            mw: true,
            n: 2
          }
        ],
        sense_candidates: %{
          0 => [
            %{
              id: "hello there|phrase|fallback",
              lemma: "hello there",
              pos: "phrase",
              score: 1.0
            }
          ]
        }
      }

      assert {:ok, %{choices: choices, audit: audit}} = Stage1.run(si, [])

      # MWE token should pass through and get a winner
      assert length(choices) == 1
      assert Enum.any?(choices, fn ch ->
               ch.token_index == 0 and
                 is_binary(ch.id) and
                 String.contains?(ch.id, "hello there")
             end)

      assert audit.kept_tokens == 1
      assert audit.dropped_tokens == 0
      assert audit.boundary_drops == 0
      assert audit.rejected_by_boundary == []
    end
  end
end

