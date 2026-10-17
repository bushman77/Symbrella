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

    assert {:warm, "You told me: The artist draws a picture.", meta} =
      Response.memory_reply(%{text: "What did the artist draw?",
        session_id: sid <> "-later", intent: :question, confidence: 0.9})
    assert meta.memory_source == :hippocampal_event

    :ok = Brain.Hippocampus.reset()
    assert {"You told me: The artist draws a picture.", _} =
      EpisodicMemory.reply("What did the artist draw?")
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
