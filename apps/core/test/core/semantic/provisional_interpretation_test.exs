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
