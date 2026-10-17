defmodule Core.Semantic.EpisodicProjectionTest do
  use ExUnit.Case, async: false
  import Ecto.Query
  alias Core.Semantic.{EpisodicProjection, EventFrames}
  alias Core.Response.{SideEffects, EpisodicMemory}

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    sid = "semantic-obligation-#{System.unique_integer([:positive])}"
    owner = sid <> "-owner"
    previous = Application.get_env(:symbrella, :memory_owner_id)
    Application.put_env(:symbrella, :memory_owner_id, owner)

    on_exit(fn ->
      if is_nil(previous),
        do: Application.delete_env(:symbrella, :memory_owner_id),
        else: Application.put_env(:symbrella, :memory_owner_id, previous)
      Db.delete_all(from(e in Db.Episode,
        where: fragment("? ->> 'owner' = ?", e.meta, ^owner)))
    end)

    %{sid: sid, owner: owner}
  end

  defp parsed_input do
    %{
      sentence: "I've got to leave.",
      source: :user,
      intent: :tell,
      tokens: [
        %{index: 0, phrase: "I've"},
        %{index: 1, phrase: "got"},
        %{index: 2, phrase: "to"},
        %{index: 3, phrase: "leave."}
      ],
      sentence_signatures: [%{id: :have_verb_to_infinitive,
        roles: %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3}}],
      lifg_choices: [
        %{token_index: 1, id: "got|verb|1"},
        %{token_index: 3, id: "leave|verb|1"}
      ],
      trace: []
    }
  end

  test "existing LIFG/syntax frame is projected and stored with provenance", %{sid: sid, owner: owner} do
    input = EventFrames.attach(parsed_input())
    assert %{type: :obligation_event, uncertain_roles: [],
      confidence: 0.8} = input.symbolic_frame

    projected = EpisodicProjection.attach(input)
    assert [%{entity: "me", relation: "has_obligation", value: "leave",
      source_frame: :obligation_event}] = projected.episodic_candidates

    SideEffects.record_turn(sid, projected.sentence, "Okay.", projected)

    assert %{value: "leave", relation: "has_obligation", provenance: :user_reported} =
      Brain.Hippocampus.user_event(owner, "me", "has_obligation")
    assert {"You told me you need to leave.", _} =
      EpisodicMemory.reply("What do I have to do?")

    row = Db.one(from e in Db.Episode,
      where: fragment("? ->> 'owner' = ?", e.meta, ^owner),
      where: fragment("? ->> 'relation' = 'has_obligation'", e.meta),
      limit: 1)

    assert row.meta["source_frame"] == "obligation_event"
    assert row.meta["frame_confidence"] == 0.8
    :ok = Brain.Hippocampus.reset()
    assert {"You told me you need to leave.", _} =
      EpisodicMemory.reply("What do I have to do?")
  end

  test "unresolved grammar, health and question frames are not admitted", %{sid: sid, owner: owner} do
    unresolved = put_in(parsed_input(), [:lifg_choices], [
      %{token_index: 1, id: "got|verb|1"},
      %{token_index: 3, id: "leave|noun|1"}
    ])

    projected = unresolved |> EventFrames.attach() |> EpisodicProjection.attach()
    assert projected.episodic_candidates == []
    SideEffects.record_turn(sid, projected.sentence, "Clarify.", projected)

    health = %{sentence: "I forgot my medication.", source: :user, intent: :tell,
      symbolic_frame: %{type: :health_support_event, subject: :user,
        confidence: 0.99, medication: "medication"}}
    assert EpisodicProjection.attach(health).episodic_candidates == []

    question = Map.merge(parsed_input(),
      %{intent: :question, sentence: "Have I got to leave?"})
    assert EpisodicProjection.attach(question).episodic_candidates == []

    assert Brain.Hippocampus.user_event(owner, "me", "has_obligation") == nil
  end

  test "tampered projected candidate and assistant source cannot cross the boundary", %{sid: sid, owner: owner} do
    projected = parsed_input() |> EventFrames.attach() |> EpisodicProjection.attach()
    [candidate] = projected.episodic_candidates
    forged = put_in(projected, [:episodic_candidates], [%{candidate | value: "buy tickets"}])

    SideEffects.record_turn(sid, projected.sentence, "Okay.", forged)
    SideEffects.record_turn(sid, projected.sentence, "Okay.", %{projected | source: :assistant})
    assert Brain.Hippocampus.user_event(owner, "me", "has_obligation") == nil
  end
end
