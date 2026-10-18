defmodule Core.Intent.StructuralStatementTest do
  use ExUnit.Case, async: true

  alias Core.Intent.StructuralStatement
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @svo_evidence [
    :contiguous_surface, :attested_nominal_subject,
    :attested_nominal_object, :attested_finite_present_verb
  ]

  @inflection_tags [
    "tag:form-of", "tag:indicative", "tag:present",
    "tag:singular", "tag:third-person"
  ]

  # A complete deterministic, source-grounded finite-present clause:
  # real IDs/grammatical structure, but no DB-dependent LIFG lookup.
  # It proves the policy, not the production dictionary's hit rate.
  defp projected_statement do
    choices = [
      %{token_index: 1, id: "baker|noun|1", lemma: "baker"},
      %{token_index: 2, id: "cooks|verb|1", lemma: "cooks"},
      %{token_index: 4, id: "meal|noun|1", lemma: "meal"}
    ]

    %{
      sentence: "The baker cooks a meal.",
      intent: :unknown,
      confidence: 0.4,
      source: :user,
      experience_origin: :user_turn,
      session_id: "synthetic-statement-policy",
      trace: [],
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "baker", n: 1},
        %{index: 2, phrase: "cooks", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "meal.", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate,
          source: :core_syntax, span: {0, 4},
          roles: %{subject_determiner: 0, subject_head: 1,
            predicate: 2, object_determiner: 3, object_head: 4},
          evidence: @svo_evidence}
      ],
      active_cells: [
        %{id: "cooks|verb|1", norm: "cooks", pos: "verb",
          definition: "An inflected form of cook.",
          semantic_atoms: @inflection_tags}
      ],
      lifg_choices: choices,
      semantic_choices: choices,
      atl_slate: %{winners: choices}
    }
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  test "revalidated finite SVO assertion becomes statement, not memory-write" do
    si = projected_statement()
    assert [%{source_frame: :finite_present_svo}] = si.episodic_candidates
    assert %{kind: :participant_assertion,
             speech_act: :assertion, source_frame: :finite_present_svo,
             status: :structurally_interpreted} = si.sentence_meaning

    out = StructuralStatement.attach(si)
    assert out.intent == :statement
    assert out.conversation_act == :statement
    assert out.confidence >= 0.6
    assert out.lifg_choices == si.lifg_choices
    assert out.atl_slate == si.atl_slate
    assert out.episodic_candidates == si.episodic_candidates

    assert %{stage: :structural_statement, decision: :statement,
             reason: :revalidated_participant_assertion,
             meta: %{writes_memory?: false, selects_verb_sense?: false}} =
      hd(out.trace)
  end

  test "non-unknown intents are never overwritten" do
    si = projected_statement()
    for intent <- [:command, :memory_write, :ask, :statement] do
      changed = %{si | intent: intent}
      assert StructuralStatement.attach(changed) == changed
    end
  end

  test "unvalidated, nonuser, forged and missing morphology evidence cannot promote" do
    si = projected_statement()

    for changed <- [
      %{si | sentence_meaning: nil},
      %{si | source: :assistant},
      %{si | experience_origin: :assistant_turn},
      %{si | atl_slate: %{winners: []}},
      %{si | episodic_candidates: []},
      %{si | sentence: "The baker does not cook a meal."},
      %{si | sentence_meaning: %{kind: :participant_assertion,
        speech_act: :assertion, status: :structurally_interpreted,
        source_frame: :finite_present_svo}}
    ] do
      assert StructuralStatement.attach(changed) == changed
    end
  end

  test "question frames and negative assertions do not become statements" do
    si = projected_statement()

    for sentence <- [
      "What did the baker cook?",
      "Does the baker cook a meal?",
      "The baker does not cook a meal."
    ] do
      changed = %{si | sentence: sentence}
      assert StructuralStatement.attach(changed) == changed
    end
  end
end
