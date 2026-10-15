defmodule Brain.LIFG.Stage1FinitePresentTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "finite-present morphologically attested verb beats plural-noun readings" do
    common = %{
      norm: "draws", lemma: "draws", activation: 0.5,
      definition: "An inflected form of draw."
    }

    noun = Map.merge(common, %{
      id: "draws|noun|1", pos: "noun",
      semantic_atoms: ["tag:form-of", "tag:plural"]
    })

    plural_only = Map.merge(common, %{
      id: "draws|noun|2", pos: "noun",
      semantic_atoms: ["tag:plural", "tag:plural-only"]
    })

    verb = Map.merge(common, %{
      id: "draws|verb|1", pos: "verb",
      semantic_atoms: ["tag:form-of", "tag:indicative", "tag:present",
                       "tag:singular", "tag:third-person"]
    })

    si = %{
      sentence: "The artist draws a picture.",
      tokens: [%{index: 2, phrase: "draws", n: 1, mw: false, span: {11, 16}}],
      active_cells: [noun, plural_only, verb],
      sense_candidates: %{2 => [noun, plural_only, verb]},
      sentence_signatures: [
        %{id: :finite_present_svo_candidate,
          roles: %{subject_head: 1, predicate: 2, object_head: 4},
          evidence: [:contiguous_surface, :attested_nominal_subject,
                     :attested_nominal_object, :attested_finite_present_verb]}
      ]
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(si,
               scores: :all,
               db_backfill?: false,
               preserve_sense_candidates: true,
               semantic_control?: false,
               weights: [
                 lex_fit: 0.0, context_fit: 0.0, rel_prior: 0.0,
                 activation: 1.0, intent_bias: 0.0
               ]
             )

    assert choice.chosen_id == "draws|verb|1"
    assert choice.scores["draws|verb|1"] > choice.scores["draws|noun|1"]
    assert choice.scores["draws|verb|1"] > choice.scores["draws|noun|2"]
  end
end
