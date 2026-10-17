defmodule Core.Semantic.SentenceMeaningParticipantsTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}
  alias Core.Comprehension.Summary
  alias Core.Syntax.Signatures

  defp action_si do
    sentence = "The artist draws a picture."
    choices = [
      %{token_index: 1, id: "artist|noun|1"},
      %{token_index: 2, id: "draws|verb|1"},
      %{token_index: 4, id: "picture|noun|1"}
    ]

    %{
      sentence: sentence, source: :user, intent: :unknown, trace: [],
      tokens: ["The", "artist", "draws", "a", "picture."]
        |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{index: index, phrase: phrase, n: 1} end),
      active_cells: [
        %{id: "draws|verb|1", norm: "draws", pos: "verb",
          definition: "An inflected form of draw.",
          semantic_atoms: ~w(tag:form-of tag:indicative tag:present
                             tag:singular tag:third-person)}
      ],
      sentence_signatures: [
        %{
          id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4},
          evidence: [:contiguous_surface, :attested_nominal_subject,
                     :attested_nominal_object, :attested_finite_present_verb],
          roles: %{subject_determiner: 0, subject_head: 1, predicate: 2,
                   object_determiner: 3, object_head: 4}
        }
      ],
      lifg_choices: choices,
      atl_slate: %{winners: choices}
    }
  end

  defp spatial_si do
    words = ~w(A bird is sitting on the fence.)
    ids = ["a|determiner|1", "bird|noun|1", "is|verb|1",
           "sitting|adjective|1", "on|preposition|1",
           "the|determiner|1", "fence|noun|1"]

    choices =
      ids |> Enum.with_index()
      |> Enum.map(fn {id, index} -> %{id: id, token_index: index} end)

    %{
      sentence: "A bird is sitting on the fence.",
      source: :user, intent: :unknown, trace: [],
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end),
      active_cells: [
        %{id: "a|determiner|1", norm: "a", pos: "determiner"},
        %{id: "bird|noun|1", norm: "bird", pos: "noun"},
        %{id: "is|verb|1", norm: "is", pos: "verb"},
        %{id: "sitting|adjective|1", norm: "sitting", pos: "adjective"},
        %{id: "sitting|verb|1", norm: "sitting", pos: "verb",
          semantic_atoms: ~w(tag:form-of tag:present tag:participle)},
        %{id: "on|preposition|1", norm: "on", pos: "preposition"},
        %{id: "the|determiner|1", norm: "the", pos: "determiner"},
        %{id: "fence|noun|1", norm: "fence", pos: "noun"}
      ],
      lifg_choices: choices,
      atl_slate: %{winners: choices}
    }
    |> Signatures.attach()
  end

  test "action becomes a semantic proposition from the same independently validated event" do
    si = action_si() |> EpisodicProjection.attach() |> SentenceMeaning.attach()
    meaning = si.sentence_meaning

    assert %{kind: :participant_assertion, status: :structurally_interpreted,
             speech_act: :assertion, version: 2} = meaning
    assert meaning.definition ==
      "The speaker reports that the artist draws a picture."
    assert meaning.roles.actor.surface == "the artist"
    assert meaning.roles.predicate.lemma == "draw"
    assert meaning.roles.predicate.surface == "draws"
    assert meaning.roles.patient.surface == "a picture"
    assert meaning.grounding.exact_lexical_meanings == :not_verified
    assert meaning.grounding.external_facts == :not_checked

    assert %{type: :participant_assertion, event_type: :action,
             epistemic_status: :user_reported_unverified,
             content: %{predicate: %{lemma: "draw", surface: "draws"},
                        arguments: %{actor: "the artist", patient: "a picture"}},
             provenance: %{frame: :finite_present_svo,
                           persistence: :not_claimed}} = meaning.proposition

    # Sentence interpretation must not alter the evidence or the event.
    assert si.episodic_candidates == EpisodicProjection.project(si)
    assert si.lifg_choices == action_si().lifg_choices
    assert si.atl_slate == action_si().atl_slate

    summary = Summary.build(si)
    assert summary.sentence_proposition == meaning.proposition
    assert summary.sentence_meaning.status == :structurally_interpreted
    assert summary.understood == []
  end

  test "spatial and action frames use one shared proposition contract" do
    si = spatial_si() |> EpisodicProjection.attach() |> SentenceMeaning.attach()
    meaning = si.sentence_meaning

    assert meaning.kind == :participant_assertion
    assert meaning.status == :structurally_interpreted
    assert meaning.roles.actor.surface == "a bird"
    assert meaning.roles.predicate.surface == "sitting"
    assert meaning.roles.position.surface == "on"
    assert meaning.roles.location.surface == "the fence"
    assert meaning.definition ==
      "The speaker reports that a bird is sitting on the fence."

    assert %{type: :participant_assertion, event_type: :spatial,
             epistemic_status: :user_reported_unverified,
             content: %{predicate: %{surface: "sitting", aspect: :progressive},
                        arguments: %{actor: "a bird", location: "the fence"},
                        modifiers: %{position: "on"}}} = meaning.proposition
    assert meaning.grounding.external_facts == :not_checked
  end

  test "rejects forged, competing, or assistant-origin participant meanings" do
    action = action_si() |> EpisodicProjection.attach()
    [candidate] = action.episodic_candidates

    forged = %{action | episodic_candidates: [%{candidate | value: "a castle"}]}
    assert SentenceMeaning.build(forged).status == :not_yet_interpreted

    competing = %{action | episodic_candidates: [candidate, candidate]}
    assert SentenceMeaning.build(competing).status == :not_yet_interpreted

    assistant = %{action | source: :assistant}
    assert SentenceMeaning.build(assistant).status == :not_yet_interpreted

    unattributed = %{action | source: nil}
    assert SentenceMeaning.build(unattributed).status == :not_yet_interpreted

    altered_role = %{action | atl_slate: %{winners: []}}
    assert SentenceMeaning.build(altered_role).status == :not_yet_interpreted

    # No participant candidate is invented merely from free-text agreement.
    assert SentenceMeaning.build(action_si()).status == :not_yet_interpreted
  end

  test "projection never invents sentence understanding for a quoted, hedged, or interrogative claim" do
    action = action_si()
    for sentence <- [
      "If the artist draws a picture.",
      "I think the artist draws a picture.",
      "The artist draws a picture?"
    ] do
      modified = %{action | sentence: sentence}
      si = modified |> EpisodicProjection.attach() |> SentenceMeaning.attach()
      assert si.episodic_candidates == []
      assert si.sentence_meaning.status == :not_yet_interpreted
    end
  end
end
