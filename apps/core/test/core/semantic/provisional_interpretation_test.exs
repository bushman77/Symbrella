defmodule Core.Semantic.ProvisionalInterpretationTest do
  use ExUnit.Case, async: true
  alias Core.Semantic.ProvisionalInterpretation, as: Provisional
  alias Core.Comprehension.Summary

  defp input(sentence, opts \\ []) do
    tokens =
      sentence
      |> String.downcase()
      |> String.replace(~r/[?!.]+\z/u, "")
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {phrase, index} -> %{index: index, phrase: phrase, n: 1} end)

    %{
      sentence: sentence, tokens: tokens, trace: [], intent: :ask,
      active_cells: Keyword.get(opts, :cells, []),
      sentence_meaning: Keyword.get(opts, :meaning, %{
        status: :not_yet_interpreted, kind: :unknown
      })
    }
  end

  defp gap_input(sentence \\ "The fox is flarping on the fence.") do
    alias Core.SemanticInput
    alias Core.Syntax.{RoleCompetition, Signatures}
    alias Core.TokenFilters

    cells = [
      %{id: "the|determiner|1", norm: "the", pos: "determiner"},
      %{id: "fox|noun|1", norm: "fox", pos: "noun"},
      %{id: "is|verb|1", norm: "is", pos: "verb"},
      %{id: "on|preposition|1", norm: "on", pos: "preposition"},
      %{id: "fence|noun|1", norm: "fence", pos: "noun"}
    ]

    choices = [
      %{token_index: 1, id: "fox|noun|1", lemma: "fox", score: 0.8},
      %{token_index: 4, id: "on|preposition|1", lemma: "on", score: 0.8}
    ]

    %SemanticInput{
      sentence: sentence, source: :test,
      active_cells: cells, lifg_choices: choices,
      sentence_meaning: %{status: :not_yet_interpreted, kind: :unknown}
    }
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Signatures.attach()
    |> RoleCompetition.attach()
    |> Core.Brain.ATL.ingest([])
  end

  test "semantic provisional frame consumes only revalidated ATL unresolved predicate evidence" do
    si = gap_input()
    assert [%{role: :predicate, status: :unresolved}] =
      si.atl_slate.unresolved_grammar

    observed = Provisional.attach(si)
    assert {:ok, frame} = Provisional.verified(observed)
    assert frame.kind == :unresolved_progressive_predicate
    assert frame.status == :provisional
    assert frame.operation == :clarify_unresolved_predicate
    assert frame.speech_act == :statement
    assert frame.target.lemma == "flarping"
    assert frame.target.lexical_evidence == :unresolved
    assert frame.participants.actor.surface == "the fox"
    assert frame.participants.predicate.surface == "flarping"
    assert frame.participants.location == %{
      relation: "on", surface: "the fence",
      event_location_verified?: false
    }
    assert frame.evidence == [
      :original_surface_fingerprint, :core_syntax_gap,
      :lifg_unresolved_role, :atl_unresolved_grammar
    ]
    assert :speaker_clarification in frame.missing
    assert :lexical_predicate_sense in frame.missing
    assert frame.speaker_clarification_confirmed? == false
    assert frame.event_occurrence_verified? == false
    assert frame.factual_authority? == false

    assert observed.lifg_choices == si.lifg_choices
    assert observed.sense_candidates == si.sense_candidates
    assert observed.atl_slate == si.atl_slate
    assert observed.sentence_signatures == si.sentence_signatures
    assert observed.role_competition == si.role_competition
    assert observed.sentence_meaning == si.sentence_meaning

    summary = Summary.build(observed)
    assert summary.provisional_interpretation == frame
    assert summary.understood == []
  end

  test "stale signatures, stale ATL evidence and mismatched sentences cannot compose" do
    si = gap_input()
    observation = hd(si.atl_slate.unresolved_grammar)
    signature = Enum.find(si.sentence_signatures,
      &(&1.id == :progressive_lexical_gap_candidate))

    stale = [
      %{si | atl_slate: %{si.atl_slate | unresolved_grammar: []}},
      %{si | atl_slate: %{si.atl_slate |
        unresolved_grammar: [%{observation | token_index: 2}]}},
      %{si | sentence_signatures:
        Enum.reject(si.sentence_signatures,
          &(&1.id == :progressive_lexical_gap_candidate))},
      %{si | sentence_signatures:
        Enum.map(si.sentence_signatures, fn
          %{id: :progressive_lexical_gap_candidate} = sig ->
            %{sig | source_span_sha256: String.duplicate("0", 64)}
          other -> other
        end)},
      %{si | sentence: "The fox is dancing on the fence."},
      %{si | sentence: "The fox is flarping on the fence?"},
      %{si | active_cells: Enum.reject(si.active_cells, &(&1.norm == "fox"))},
      %{si | atl_slate: Map.put(si.atl_slate, :winners,
        si.atl_slate.winners ++ [
          %{token_index: 3, id: "flarping|verb|1", norm: "flarping"}
        ])}
    ]

    for altered <- stale do
      assert Provisional.detect(altered) == nil
    end

    attached = Provisional.attach(si)
    forged = put_in(attached,
      [:provisional_interpretation, :participants, :predicate, :surface],
      "dancing")

    assert Provisional.verified(forged) == :not_applicable
    assert Provisional.verified(%{attached | sentence: "The fox is dancing on the fence."}) ==
      :not_applicable
    assert signature.source_span_sha256 == attached.provisional_interpretation.source_span_sha256
  end

  test "existing established sentence meaning takes precedence over predicate-gap hypothesis" do
    si = gap_input()
    established = %{status: :structurally_interpreted, kind: :verified_question}
    assert Provisional.detect(%{si | sentence_meaning: established}) == nil
  end

  test "location question has a target, but no resolved location or referent" do
    si =
      input("Where is the bird?", cells: [
        %{id: "bird|noun|1", norm: "bird", pos: "noun"}
      ])
      |> Provisional.attach()
    assert {:ok, frame} = Provisional.verified(si)
    assert frame.operation == :locate_entity
    assert frame.speech_act == :question
    assert frame.target.lemma == "bird"
    assert frame.target.surface == "the bird"
    assert frame.target.lexical_evidence == :noun_pos_attested
    assert frame.target.referent_status == :unresolved
    assert frame.requested_relation == :location
    assert frame.missing == [:referent_binding, :location]
    assert frame.factual_authority? == false

    summary = Summary.build(si)
    assert summary.provisional_interpretation == frame
    assert "bird" in summary.recognized_topics
    assert summary.understood == []
    assert summary.lexical_meanings == []
  end

  test "concept question and one-word fragment retain only their partial speech act" do
    concept = input("What is a bird?") |> Provisional.attach()
    assert concept.provisional_interpretation.operation == :define_concept
    assert concept.provisional_interpretation.requested_relation == :definition
    assert concept.provisional_interpretation.target.lexical_evidence == :not_verified
    assert concept.provisional_interpretation.factual_authority? == false

    fragment = input("Sparrow") |> Provisional.attach()
    assert fragment.provisional_interpretation.operation == :ambiguous_topic
    assert fragment.provisional_interpretation.speech_act == :fragment
    assert :speech_act in fragment.provisional_interpretation.missing
    assert Summary.build(fragment).understood == []
  end

  test "malformed, unrelated, and tampered surfaces cannot carry a frame" do
    original = input("Where is the bird?") |> Provisional.attach()
    forged = put_in(original, [:provisional_interpretation, :target, :lemma], "dog")
    assert Provisional.verified(forged) == :not_applicable
    assert Summary.build(forged).provisional_interpretation == nil

    mismatched = %{original | tokens: input("Where is the dog?").tokens}
    assert Provisional.detect(mismatched) == nil

    for sentence <- ["Where is the bird beside the fence?",
                     "What does a car do?", "How does a car work?",
                     "tell me the history of artist"] do
      assert Provisional.detect(input(sentence)) == nil
    end
  end

  test "established sentence meaning and lexical state are never overwritten" do
    established = %{status: :structurally_interpreted, kind: :verified_question}
    si = input("What is a bird?", meaning: established)
    assert Provisional.attach(si).provisional_interpretation == nil

    initial =
      input("Where is the bird?")
      |> Map.put(:semantic_choices, [%{id: "bird|noun|1"}])
      |> Map.put(:sense_candidates, %{3 => [%{id: "bird|noun|1"}]})
      |> Map.put(:symbolic_frame, nil)
    result = Provisional.attach(initial)

    assert result.semantic_choices == initial.semantic_choices
    assert result.sense_candidates == initial.sense_candidates
    assert result.sentence_meaning == initial.sentence_meaning
    assert result.symbolic_frame == initial.symbolic_frame
  end
end
