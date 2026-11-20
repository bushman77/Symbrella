defmodule Brain.LIFG.NeighborGlossContextTest do
  @moduledoc """
  P-031: optional neighbour-gloss context for ContextFit (extended-Lesk style).
  Neighbour glosses add half-strength evidence, never double-count literal
  sentence evidence, and are byte-identical when absent or switched off.
  """
  use ExUnit.Case, async: true

  alias Brain.LIFG.{ContextFit, Stage1}

  @river_bank %{id: "bank|noun|10", lemma: "bank", pos: "noun",
                definition: "Sloping land beside a body of water.", activation: 0.5}
  @finance_bank %{id: "bank|noun|1", lemma: "bank", pos: "noun",
                  definition: "An institution where one can deposit money.", activation: 0.5}
  @river %{id: "river|noun|1", lemma: "river", pos: "noun",
           definition: "A large natural stream of water.", activation: 0.5}

  describe "ContextFit.explain/4" do
    test "absent or empty neighbour context is byte-identical" do
      s = "I sat on the bank near the river."
      plain = ContextFit.explain(s, "bank", @river_bank)

      assert ContextFit.explain(s, "bank", @river_bank, neighbor_context: []) == plain
      assert ContextFit.explain(s, "bank", @river_bank, neighbor_context: nil) == plain
      refute Map.has_key?(plain, :neighbor_gloss_matches)
    end

    test "a neighbour gloss term adds half-strength evidence to the matching sense" do
      s = "I sat on the bank near the river."
      plain = ContextFit.explain(s, "bank", @river_bank)
      nb = ContextFit.explain(s, "bank", @river_bank, neighbor_context: [@river.definition])

      assert nb.neighbor_gloss_matches.definition == ["water"]
      # one novel match: 0.5 * overlap_strength(1) = 0.25 definition component
      assert_in_delta nb.component_scores.definition, 0.25, 1.0e-6
      assert nb.score > plain.score
    end

    test "an unrelated sense gains nothing from the same neighbour gloss" do
      s = "I sat on the bank near the river."
      plain = ContextFit.explain(s, "bank", @finance_bank)
      nb = ContextFit.explain(s, "bank", @finance_bank, neighbor_context: [@river.definition])

      assert nb.score == plain.score
      assert Enum.all?(Map.values(nb.neighbor_gloss_matches), &(&1 == []))
    end

    test "terms already in the literal sentence are not double-counted" do
      s = "Water lapped at the bank."
      plain = ContextFit.explain(s, "bank", @river_bank)
      nb = ContextFit.explain(s, "bank", @river_bank, neighbor_context: ["Clear water."])

      assert plain.score > 0.0
      assert nb.score == plain.score
    end

    test "neighbour evidence never exceeds half of a literal match" do
      literal = ContextFit.explain("Water lapped at the bank.", "bank", @river_bank)
      borrowed =
        ContextFit.explain("Something lapped at the bank.", "bank", @river_bank,
          neighbor_context: ["Clear water."])

      assert_in_delta borrowed.component_scores.definition,
                      literal.component_scores.definition / 2, 1.0e-6
    end
  end

  describe "P-031b filters" do
    test "gloss meta-vocabulary and generic gloss words never count as neighbour evidence" do
      cand = %{id: "pond|noun|1", lemma: "pond", pos: "noun",
               definition: "(countable, uncountable) A common, simple body of still water, used especially by a person."}

      nb =
        ContextFit.explain("They walked to the pond.", "pond", cand,
          neighbor_context: ["(uncountable, colloquial) Something common, simple and used especially by a person; water."])

      assert nb.neighbor_gloss_matches.definition == ["water"]
    end

    test "function-word targets receive no neighbour context in Stage1" do
      sentence = "the river"
      si = %{
        sentence: sentence,
        tokens: [
          %{index: 0, phrase: "the", n: 1, mw: false, span: {0, 3}},
          %{index: 1, phrase: "river", n: 1, mw: false, span: {4, 9}}
        ],
        sense_candidates: %{
          0 => [%{id: "the|determiner|1", lemma: "the", pos: "determiner",
                  definition: "Used before a noun for a stream of water already known.",
                  activation: 0.5}],
          1 => [@river]
        }
      }

      {:ok, %{choices: choices}} =
        Stage1.run(si, scores: :all, preserve_sense_candidates: true,
          cerebellum_learn?: false, neighbor_gloss_context?: true)

      the = Enum.find(choices, &(&1.token_index == 0))

      if the do
        refute Enum.any?(List.wrap(Map.get(the, :definition_context_evidence)),
                 &Map.has_key?(&1, :neighbor_gloss_matches))
      end
    end
  end

  describe "Stage1 integration" do
    defp river_si do
      sentence = "I sat on the bank near the river."
      {b, bw} = :binary.match(sentence, "bank")
      {r, rw} = :binary.match(sentence, "river")

      %{
        sentence: sentence,
        tokens: [
          %{index: 0, phrase: "bank", n: 1, mw: false, span: {b, b + bw}},
          %{index: 1, phrase: "river", n: 1, mw: false, span: {r, r + rw}}
        ],
        sense_candidates: %{0 => [@finance_bank, @river_bank], 1 => [@river]}
      }
    end

    defp bank_choice(opts) do
      assert {:ok, %{choices: choices}} =
               Stage1.run(river_si(), [scores: :all, preserve_sense_candidates: true,
                 cerebellum_learn?: false] ++ opts)

      Enum.find(choices, &(&1.token_index == 0))
    end

    test "explicit opt false disables neighbour context regardless of config" do
      # With no context evidence the two noun senses tie exactly. This holds
      # even when config :brain, :lifg_neighbor_gloss_context? is true,
      # because the run option takes precedence.
      off = bank_choice(neighbor_gloss_context?: false)
      assert off.scores["bank|noun|1"] == off.scores["bank|noun|10"]
    end

    test "switched on, the river gloss supports the river sense of bank" do
      off = bank_choice(neighbor_gloss_context?: false)
      on = bank_choice(neighbor_gloss_context?: true)

      assert on.scores["bank|noun|10"] > off.scores["bank|noun|10"]
      assert on.chosen_id == "bank|noun|10"
    end

    test "a single-token sentence has no neighbours and is unchanged when on" do
      si = %{
        sentence: "bank",
        tokens: [%{index: 0, phrase: "bank", n: 1, mw: false, span: {0, 4}}],
        sense_candidates: %{0 => [@finance_bank, @river_bank]}
      }

      run = fn opts ->
        {:ok, %{choices: [c]}} =
          Stage1.run(si, [scores: :all, preserve_sense_candidates: true,
            cerebellum_learn?: false] ++ opts)

        {c.chosen_id, c.scores}
      end

      assert run.(neighbor_gloss_context?: true) == run.(neighbor_gloss_context?: false)
    end
  end
end
