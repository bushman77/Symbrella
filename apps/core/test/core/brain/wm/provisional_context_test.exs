defmodule Core.Brain.WM.ProvisionalContextTest do
  use ExUnit.Case, async: true
  alias Core.Brain.WM.Comprehension
  alias Core.Semantic.ProvisionalInterpretation, as: Provisional
  alias Core.Comprehension.Summary

  defp input(sentence, previous \\ nil, session \\ "session-a") do
    words =
      sentence
      |> String.downcase()
      |> String.replace(~r/[?!.]+\z/u, "")
      |> String.split(~r/\s+/u, trim: true)

    %{
      sentence: sentence,
      tokens: Enum.with_index(words, fn phrase, index ->
        %{index: index, phrase: phrase, n: 1}
      end),
      session_id: session, intent: :ask, trace: [], active_cells: [],
      sentence_meaning: %{status: :not_yet_interpreted, kind: :unknown},
      wm_context: previous
    }
  end

  defp context(si) do
    out = Provisional.attach(si)
    Comprehension.context_from(out, Summary.build(out))
  end

  test "WM keeps the question kind and candidate subject, not a spatial fact" do
    wm = context(input("Where is the bird?"))
    assert wm.topic == "bird"
    assert wm.topic_source == :provisional_construction
    assert wm.interpretation_status == :provisional
    assert wm.requested_relation == :location
    assert wm.relation == nil
    assert wm.entities == []
    assert wm.event_roles == %{}
    assert wm.provisional_interpretation.operation == :locate_entity
    assert wm.provisional_interpretation.antecedent_candidates == []
    assert wm.provisional_interpretation.factual_authority? == false
    assert wm.verification == :conversation_context_only
  end

  test "prior entities are only tentative antecedent candidates" do
    prior = %{topic: nil, entities: ["the bird", "the fence"],
              relation: :on, verification: :conversation_context_only}
    wm = context(input("Where is the bird?", prior))
    assert wm.provisional_interpretation.antecedent_candidates == ["the bird"]
    assert wm.provisional_interpretation.referent_status == :candidate_same_session
    assert wm.relation == nil
    assert wm.entities == []

    unrelated = context(input("Where is the dog?", prior))
    assert unrelated.provisional_interpretation.antecedent_candidates == []
    assert unrelated.topic == "dog"

    no_session = context(input("Where is the bird?", prior, nil))
    assert no_session.provisional_interpretation.antecedent_candidates == []
  end

  test "fragment topic retains uncertain intent, with no fabricated entity" do
    wm = context(input("Sparrow"))
    assert wm.topic == "sparrow"
    assert wm.requested_relation == nil
    assert wm.provisional_interpretation.operation == :ambiguous_topic
    assert :speech_act in wm.provisional_interpretation.missing
    assert wm.entities == []
  end

  test "forged hypotheses cannot populate temporary WM" do
    si = input("Where is the bird?")
    forged = Provisional.attach(si)
      |> put_in([:provisional_interpretation, :target, :lemma], "horse")
    summary = Summary.build(forged)
    assert summary.provisional_interpretation == nil
    wm = Comprehension.context_from(forged, summary)
    assert wm.provisional_interpretation == nil
    assert wm.topic == nil
  end
end
