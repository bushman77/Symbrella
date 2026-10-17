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
