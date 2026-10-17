defmodule Core.Semantic.ParticipantSpatialTest do
  use ExUnit.Case, async: false
  import Ecto.Query

  alias Core.Response
  alias Core.Response.{EpisodicMemory, SideEffects}
  alias Core.Semantic.{EpisodicProjection, ParticipantSpatial}
  alias Core.Syntax.Signatures

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    sid = "participants-#{System.unique_integer([:positive])}"
    owner = sid <> "-owner"
    old_owner = Application.get_env(:symbrella, :memory_owner_id)
    Application.put_env(:symbrella, :memory_owner_id, owner)

    on_exit(fn ->
      if is_nil(old_owner), do: Application.delete_env(:symbrella, :memory_owner_id),
        else: Application.put_env(:symbrella, :memory_owner_id, old_owner)

      Db.delete_all(from(e in Db.Episode,
        where: fragment("? ->> 'owner' = ?", e.meta, ^owner)))
    end)

    %{sid: sid, owner: owner}
  end

  defp input do
    sentence = "A bird is sitting on the fence."
    words = ~w(A bird is sitting on the fence.)
    ids = ["a|determiner|1", "bird|noun|1", "is|auxiliary|0",
           "sitting|adjective|1", "on|preposition|1",
           "the|determiner|1", "fence|noun|1"]

    choices =
      ids
      |> Enum.with_index()
      |> Enum.map(fn {id, index} -> %{id: id, token_index: index} end)

    %{
      source: :user,
      intent: :unknown,
      confidence: 0.4,
      sentence: sentence,
      trace: [],
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end),
      active_cells: [
        %{id: "a|determiner|1", norm: "a", pos: "determiner"},
        %{id: "bird|noun|1", norm: "bird", pos: "noun"},
        %{id: "is|verb|1", norm: "is", pos: "verb"},
        %{id: "sitting|adjective|1", norm: "sitting", pos: "adjective"},
        %{id: "sitting|verb|1", norm: "sitting", pos: "verb",
          semantic_atoms: ["tag:form-of", "tag:present", "tag:participle"]},
        %{id: "on|preposition|1", norm: "on", pos: "preposition"},
        %{id: "the|determiner|1", norm: "the", pos: "determiner"},
        %{id: "fence|noun|1", norm: "fence", pos: "noun"}
      ],
      lifg_choices: choices,
      atl_slate: %{winners: choices}
    }
  end

  defp projected do
    input()
    |> Signatures.attach()
    |> EpisodicProjection.attach()
  end

  test "attested morphology builds a full progressive participant signature" do
    si = input() |> Signatures.attach()

    assert Enum.any?(si.sentence_signatures, fn
      %{id: :progressive_locative_candidate, span: {0, 6},
        roles: %{subject_head: 1, predicate: 3,
                 preposition: 4, location_head: 6}} -> true
      _ -> false
    end)

    assert [%{entity_key: "bird", relation: "spatial_state",
              value: "the fence",
              participants: %{"actor" => "a bird", "predicate" => "sitting",
                              "position" => "on", "location" => "the fence"}}] =
             ParticipantSpatial.project(si, si.sentence)
  end

  test "each question retrieves a different persisted participant role",
       %{sid: sid, owner: owner} do
    si = projected() |> Map.put(:session_id, sid)

    assert {:warm, "I’ve recorded what you told me: A bird is sitting on the fence.",
            %{persistence_status: :persisted}} = Response.plan(si, %{})

    assert %{status: "asserted", participants: %{"actor" => "a bird", "predicate" => "sitting",
              "position" => "on", "location" => "the fence"}} =
             Brain.Hippocampus.user_event(owner, "bird", "spatial_state")

    checks = [
      {"Where is the bird?", "On the fence."},
      {"What is sitting on the fence?", "A bird."},
      {"What is the bird doing?", "Sitting."},
      {"What is the bird sitting on?", "On the fence."}
    ]

    for {question, expected} <- checks do
      assert {:warm, ^expected, %{memory_source: :hippocampal_event,
                                 retrieval_found?: true}} =
        Response.memory_reply(%{text: question, session_id: sid <> "-fresh",
                                intent: :ask, confidence: 0.9})
    end

    :ok = Brain.Hippocampus.reset()
    assert {"A bird.", _} = EpisodicMemory.reply("What is sitting on the fence?")
  end

  test "superseded spatial state does not produce a stale reverse participant",
       %{sid: sid} do
    si = projected()
    SideEffects.record_turn(sid, si.sentence, "Recorded.", si)
    assert {"A bird.", _} = EpisodicMemory.reply("What is sitting on the fence?")

    assert :ok = EpisodicMemory.remember_event(%{
      entity: "a bird", relation: "spatial_state", value: "the branch",
      source_frame: :progressive_spatial,
      participants: %{"actor" => "a bird", "predicate" => "resting",
                      "position" => "on", "location" => "the branch"}
    }, sid, "A bird is resting on the branch.")

    assert EpisodicMemory.reply("What is sitting on the fence?") == nil
    assert {"A bird.", _} = EpisodicMemory.reply("What is resting on the branch?")
  end

  test "missing morphology, unsupported polarity, or forged participants are not stored",
       %{sid: sid, owner: owner} do
    si = input()
    cells = Enum.reject(si.active_cells, &(&1.id == "sitting|verb|1"))
    assert ParticipantSpatial.project(
      %{si | active_cells: cells} |> Signatures.attach(), si.sentence
    ) == []

    projected = projected()
    [event] = projected.episodic_candidates

    forged = %{projected | episodic_candidates: [
      %{event | participants: %{"actor" => "horse"}}
    ]}
    SideEffects.record_turn(sid, si.sentence, "Okay.", forged)
    assert Brain.Hippocampus.user_event(owner, "bird", "spatial_state") == nil

    assert ParticipantSpatial.project(%{projected | sentence: "Maybe a bird is sitting on the fence."},
      "Maybe a bird is sitting on the fence.") == []
  end
end
