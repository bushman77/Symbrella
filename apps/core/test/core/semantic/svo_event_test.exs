defmodule Core.Semantic.SVOEventTest do
  use ExUnit.Case, async: false
  import Ecto.Query

  alias Core.Semantic.{EpisodicProjection, SVOEvent}
  alias Core.Response.{EpisodicMemory, SideEffects}
  alias Core.Response

  @evidence [:contiguous_surface, :attested_nominal_subject,
             :attested_nominal_object, :attested_finite_present_verb]

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    sid = "svo-test-#{System.unique_integer([:positive])}"
    owner = sid <> "-owner"
    previous = Application.get_env(:symbrella, :memory_owner_id)
    Application.put_env(:symbrella, :memory_owner_id, owner)
    on_exit(fn ->
      if is_nil(previous), do: Application.delete_env(:symbrella, :memory_owner_id),
        else: Application.put_env(:symbrella, :memory_owner_id, previous)
      Db.delete_all(from(e in Db.Episode,
        where: fragment("? ->> 'owner' = ?", e.meta, ^owner)))
    end)
    %{sid: sid, owner: owner}
  end

  defp input do
    choices = [
      %{token_index: 1, id: "artist|noun|1", lemma: "artist"},
      %{token_index: 2, id: "draw|verb|1", lemma: "draw"},
      %{token_index: 4, id: "picture|noun|1", lemma: "picture"}
    ]

    %{
      sentence: "The artist draws a picture.", source: :user, intent: :tell,
      tokens: [
        %{index: 0, phrase: "The", n: 1},
        %{index: 1, phrase: "artist", n: 1},
        %{index: 2, phrase: "draws", n: 1},
        %{index: 3, phrase: "a", n: 1},
        %{index: 4, phrase: "picture.", n: 1}
      ],
      sentence_signatures: [
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {0, 4}, evidence: @evidence, roles: %{
            subject_determiner: 0, subject_head: 1, predicate: 2,
            object_determiner: 3, object_head: 4}}
      ],
      lifg_choices: choices,
      atl_slate: %{winners: choices},
      trace: []
    }
  end

  test "new predicate projects from syntax, LIFG and ATL and survives across sessions", %{sid: sid, owner: owner} do
    si = EpisodicProjection.attach(input())

    assert [%{entity_key: "artist", relation: "verb_draw", value: "a picture",
              source_frame: :finite_present_svo}] = si.episodic_candidates

    SideEffects.record_turn(sid, si.sentence, "I understand.", si)
    assert %{value: "a picture", sentence: "The artist draws a picture.",
             source_frame: "finite_present_svo", provenance: :user_reported} =
             Brain.Hippocampus.user_event(owner, "artist", "verb_draw")

    assert {:warm, "A picture.", meta} =
      Response.memory_reply(%{text: "What did the artist draw?",
        session_id: sid <> "-later", intent: :question, confidence: 0.9})
    assert meta.memory_source == :hippocampal_event

    :ok = Brain.Hippocampus.reset()
    assert {"A picture.", _} =
      EpisodicMemory.reply("What did the artist draw?")
  end

  test "verified SVO action carries participant roles into PostgreSQL and answers two directions",
       %{sid: sid, owner: owner} do
    si = EpisodicProjection.attach(inflected_input())

    assert [%{relation: "verb_draw", value: "a picture",
              participants: %{
                "actor" => "the artist",
                "predicate" => "draw",
                "surface_predicate" => "draws",
                "patient" => "a picture",
                "tense" => "present"
              }}] = si.episodic_candidates

    assert :ok = EpisodicMemory.remember_projected_events(si, si.sentence, sid)

    assert %{status: "asserted",
             participants: %{"actor" => "the artist", "patient" => "a picture",
                             "predicate" => "draw", "surface_predicate" => "draws"}} =
             Brain.Hippocampus.user_event(owner, "artist", "verb_draw")

    for {question, expected} <- [
          {"What does the artist draw?", "A picture."},
          {"What did the artist draw?", "A picture."},
          {"Who draws a picture?", "The artist."}
        ] do
      assert {:warm, ^expected,
              %{response_source: :symbolic_conversation,
                memory_source: :hippocampal_event, retrieval_found?: true}} =
        Response.memory_reply(%{text: question, session_id: sid <> "-recall",
                                intent: :question, confidence: 0.9})
    end

    # The source event, not the model transcript, remains authoritative
    # even when the Hippocampus process discards its in-memory window.
    assert :ok = Brain.Hippocampus.reset()
    assert {"The artist.", _} = EpisodicMemory.reply("Who draws a picture?")
  end

  test "action reverse lookup never guesses across multiple actors or stale events",
       %{sid: sid, owner: owner} do
    event = %{
      entity: "the artist", relation: "verb_draw", value: "a picture",
      source_frame: :finite_present_svo,
      participants: %{"actor" => "the artist", "predicate" => "draw",
                      "surface_predicate" => "draws", "patient" => "a picture",
                      "tense" => "present"}
    }

    assert :ok = EpisodicMemory.remember_event(event, sid, "The artist draws a picture.")
    assert {"The artist.", _} = EpisodicMemory.reply("Who draws a picture?")

    # A second actor meeting the same relation cannot be guessed away.
    other = %{event |
      entity: "the painter",
      participants: %{event.participants | "actor" => "the painter"}
    }
    assert :ok = EpisodicMemory.remember_event(other, sid, "The painter draws a picture.")
    assert EpisodicMemory.reply("Who draws a picture?") == nil

    # The latest actor/relation assertion supersedes an older patient value.
    moved = %{other |
      value: "a mural",
      participants: %{other.participants | "patient" => "a mural"}
    }
    assert :ok = EpisodicMemory.remember_event(moved, sid, "The painter draws a mural.")
    assert {"The artist.", _} = EpisodicMemory.reply("Who draws a picture?")
    assert {"The painter.", _} = EpisodicMemory.reply("Who draws a mural?")

    retracted = %{event | status: :retracted}
    assert :ok = EpisodicMemory.remember_event(retracted, sid, "I withdraw that assertion.")
    assert EpisodicMemory.reply("Who draws a picture?") == nil
    assert %{status: "retracted"} =
      Brain.Hippocampus.user_event(owner, "artist", "verb_draw")
  end

  test "action question grammar binds requested role, not arbitrary text" do
    alias Core.Semantic.ActionQuestions

    assert {:forward, "artist", "draw"} =
             ActionQuestions.parse("What does the artist draw?")
    assert {:reverse, "draws", "a picture"} =
             ActionQuestions.parse("Who draws a picture?")

    assert ActionQuestions.parse("Who drew a picture?") ==
             {:reverse, "drew", "a picture"}
    assert ActionQuestions.parse("Who might draw a picture?") == nil

    event = %{status: "asserted", relation: "verb_draw", value: "a picture",
              participants: %{
                "actor" => "the artist", "predicate" => "draw",
                "surface_predicate" => "draws", "patient" => "a picture",
                "tense" => "present"
              }}

    assert ActionQuestions.answer(event, :actor,
             %{surface_predicate: "draws", patient: "a picture"}) == "The artist."
    assert ActionQuestions.answer(event, :patient, %{predicate: "draw"}) == "A picture."
    assert ActionQuestions.answer(event, :actor,
             %{surface_predicate: "drew", patient: "a picture"}) == nil
    assert ActionQuestions.answer(%{event | status: "retracted"}, :actor, %{}) == nil
    assert ActionQuestions.answer(%{event | relation: "verb_paint"}, :actor, %{}) == nil
  end

  test "misclassified declarative still gets stored and is acknowledged", %{sid: sid, owner: owner} do
    # Input intent is wrong, but a verified SVO clause is not a question.
    si = EpisodicProjection.attach(%{input() | intent: :question})
    assert [%{relation: "verb_draw"}] = si.episodic_candidates

    assert {:warm, acknowledgment, meta} = Response.plan(si, %{})
    assert acknowledgment == "I’ve recorded what you told me: The artist draws a picture."
    assert meta.response_source == :symbolic_conversation
    assert meta.persistence_status == :persisted

    assert %{value: "a picture", sentence: "The artist draws a picture."} =
      Brain.Hippocampus.user_event(owner, "artist", "verb_draw")

    assert {:warm, "A picture.", recall_meta} =
      Response.memory_reply(%{
        text: "What did the artist draw?", session_id: sid <> "-new",
        intent: :question, confidence: 0.9
      })

    assert recall_meta.memory_source == :hippocampal_event
    assert recall_meta.retrieval_found?
    assert recall_meta.memory_event_id
  end

  test "partial competing SVO hypotheses do not block a unique full-clause event" do
    si = input()

    # Production vocabulary can assign a noun alternative to the article "a",
    # yielding a competing {0, 3} span that does not cover the full sentence.
    partial = %{
      id: :finite_present_svo_candidate,
      source: :core_syntax,
      span: {0, 3},
      evidence: @evidence,
      roles: %{
        subject_determiner: 0,
        subject_head: 1,
        predicate: 2,
        object_head: 3
      }
    }

    si = %{si | sentence_signatures: si.sentence_signatures ++ [partial]}

    assert [%{entity_key: "artist", relation: "verb_draw", value: "a picture",
              source_frame: :finite_present_svo}] =
             EpisodicProjection.attach(si).episodic_candidates
  end

  defp inflected_input do
    si = input()

    choices =
      Enum.map(si.lifg_choices, fn
        %{token_index: 2} = choice ->
          %{choice | id: "draws|verb|1", lemma: "draws"}

        choice ->
          choice
      end)

    si
    |> Map.put(:lifg_choices, choices)
    |> Map.put(:atl_slate, %{winners: choices})
    |> Map.put(:active_cells, [
      %{
        id: "draws|verb|1",
        pos: "verb",
        norm: "draws",
        definition: "An inflected form of draw.",
        semantic_atoms: [
          "tag:form-of", "tag:indicative", "tag:present",
          "tag:singular", "tag:third-person"
        ]
      }
    ])
  end

  test "inflected production verb is stored under its attested base verb", %{sid: sid, owner: owner} do
    si = EpisodicProjection.attach(inflected_input())

    assert [%{entity_key: "artist", relation: "verb_draw", value: "a picture",
              source_frame: :finite_present_svo}] = si.episodic_candidates

    SideEffects.record_turn(sid, si.sentence, "I understand.", si)

    assert %{value: "a picture", sentence: "The artist draws a picture."} =
             Brain.Hippocampus.user_event(owner, "artist", "verb_draw")

    assert {"A picture.", _meta} =
             EpisodicMemory.reply("What did the artist draw?")
  end

  test "does not guess a base form without matching morphology evidence" do
    si = inflected_input()
    [cell] = si.active_cells

    assert [%{relation: "verb_draws"}] =
             SVOEvent.project(%{si | active_cells: [%{cell | semantic_atoms: []}]}, si.sentence)

    assert [%{relation: "verb_draws"}] =
             SVOEvent.project(%{si | active_cells: [%{cell | id: "other|verb|1"}]}, si.sentence)
  end

  test "response attach preserves SVO proof and persists a live-planned assertion",
       %{sid: sid, owner: owner} do
    si =
      input()
      |> Map.put(:session_id, sid)
      |> Map.put(:confidence, 0.4)
      |> EpisodicProjection.attach()

    planned = Core.Response.Attach.maybe_build_response_plan(si, response: :auto)

    assert planned.response_text == "I’ve recorded what you told me: The artist draws a picture."
    assert planned.response_meta.response_source == :symbolic_conversation
    assert planned.response_meta.persistence_status == :persisted

    assert %{value: "a picture", sentence: "The artist draws a picture."} =
             Brain.Hippocampus.user_event(owner, "artist", "verb_draw")
  end

  test "rejects mismatched ATL winners or disputed grammatical roles", %{sid: sid, owner: owner} do
    si = input()
    wrong_atl = put_in(si, [:atl_slate, :winners],
      [%{token_index: 1, id: "artist|noun|1"},
       %{token_index: 2, id: "draw|noun|1"},
       %{token_index: 4, id: "picture|noun|1"}])
    assert SVOEvent.project(wrong_atl, wrong_atl.sentence) == []
    wrong_lifg = put_in(si, [:lifg_choices],
      [%{token_index: 1, id: "artist|noun|1"},
       %{token_index: 2, id: "draw|noun|1"},
       %{token_index: 4, id: "picture|noun|1"}])
    assert SVOEvent.project(wrong_lifg, wrong_lifg.sentence) == []

    duplicate = %{si | sentence_signatures: si.sentence_signatures ++ si.sentence_signatures}
    assert SVOEvent.project(duplicate, duplicate.sentence) == []

    wrong_source = %{si | source: :assistant}
    assert EpisodicProjection.project(wrong_source) == []

    SideEffects.record_turn(sid, wrong_source.sentence, "Okay.", wrong_source)
    assert Brain.Hippocampus.user_event(owner, "artist", "verb_draw") == nil
  end

  test "declines partial spans, hedged assertions and source text mismatch" do
    si = input()
    assert SVOEvent.project(%{si | sentence: "If the artist draws a picture."},
      "If the artist draws a picture.") == []
    assert SVOEvent.project(%{si | sentence: "The artist does not draw a picture."},
      "The artist does not draw a picture.") == []
    assert SVOEvent.project(%{si | sentence: "The artist draws a picture?"},
      "The artist draws a picture?") == []
    assert SVOEvent.project(%{si | sentence: "Rowan believes the artist draws a picture."},
      "Rowan believes the artist draws a picture.") == []
    assert SVOEvent.project(%{si | sentence_signatures:
      [put_in(hd(si.sentence_signatures), [:span], {1, 4})]}, si.sentence) == []
  end

  test "forged candidate cannot bypass the original SI proof", %{sid: sid, owner: owner} do
    projected = EpisodicProjection.attach(input())
    [event] = projected.episodic_candidates

    poisoned = %{projected | episodic_candidates: [%{event | value: "a castle"}]}
    SideEffects.record_turn(sid, projected.sentence, "Okay.", poisoned)

    assert Brain.Hippocampus.user_event(owner, "artist", "verb_draw") == nil
  end
end
