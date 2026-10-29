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

  # Ordinary user reports are persisted by SideEffects.record_turn/4 without
  # becoming an unsolicited memory-write acknowledgement. Tests disable the LLM,
  # so speech generation fails independently of verified event persistence.
  defp assert_report_response_without_memory_ack(plan) do
    assert plan.response_meta.response_source == :model_unavailable
    refute Map.has_key?(plan.response_meta, :persistence_status)
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

  test "MWE-suppressed locative winners admit only dictionary-backed roles" do
    si = input() |> Signatures.attach()

    # Production omits on/fence while retaining full independent grammar
    # and exact dictionary rows. No substitute LIFG winners are manufactured.
    filtered = fn choices ->
      Enum.reject(choices, &(Map.get(&1, :token_index) in [4, 6]))
    end

    si = %{si |
      lifg_choices: filtered.(si.lifg_choices),
      atl_slate: %{winners: filtered.(si.atl_slate.winners)}
    }

    assert [%{entity_key: "bird", relation: "spatial_state",
              participants: %{"position" => "on", "location" => "the fence"}}] =
             ParticipantSpatial.project(si, si.sentence)

    # A missing row is not evidence. A wrong lexical selection is not
    # interchangeable with a dictionary candidate for that word.
    no_prep = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.id == "on|preposition|1"))}
    assert ParticipantSpatial.project(no_prep, si.sentence) == []

    no_noun = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.id == "fence|noun|1"))}
    assert ParticipantSpatial.project(no_noun, si.sentence) == []

    conflict = %{si | lifg_choices:
      si.lifg_choices ++ [%{id: "on|adverb|1", token_index: 4}]}
    assert ParticipantSpatial.project(conflict, si.sentence) == []
  end

  test "response adapter persists MWE-covered location roles, not only direct projection",
       %{sid: sid, owner: owner} do
    si = input() |> Signatures.attach()
    without_word_winners = fn winners ->
      Enum.reject(winners, &(Map.get(&1, :token_index) in [4, 6]))
    end
    si = %{si |
      lifg_choices: without_word_winners.(si.lifg_choices),
      atl_slate: %{winners: without_word_winners.(si.atl_slate.winners)}
    }
    si =
      si
      |> EpisodicProjection.attach()
      |> Map.put(:session_id, sid)

    planned = Core.Response.Attach.maybe_build_response_plan(si, response: :auto)

    assert_report_response_without_memory_ack(planned)

    assert %{value: "the fence", participants: %{"actor" => "a bird",
              "position" => "on", "location" => "the fence"}} =
      Brain.Hippocampus.user_event(owner, "bird", "spatial_state")

    assert {"On the fence.", %{memory_source: :hippocampal_event}} =
      EpisodicMemory.reply("Where is the bird?")
  end

  test "production proper-noun tie preserves attested common-noun actor",
       %{sid: sid, owner: owner} do
    # The real live diagnostics ranked bird|proper_noun|1 a hair above
    # bird|noun|1 for the lower-case phrase "A bird".
    # Concurrent PMTG coverage may also suppress the on/fence word winners.
    si = input() |> Signatures.attach()

    actual_choice =
      %{id: "bird|proper_noun|1", token_index: 1}

    choices =
      si.lifg_choices
      |> Enum.reject(&(Map.get(&1, :token_index) in [1, 4, 6]))
      |> Enum.concat([actual_choice])

    si = %{si |
      lifg_choices: choices,
      atl_slate: %{winners: choices}
    }

    assert [%{entity_key: "bird", relation: "spatial_state",
              participants: %{"actor" => "a bird", "location" => "the fence"}}] =
             ParticipantSpatial.project(si, si.sentence)

    validated =
      si
      |> EpisodicProjection.attach()
      |> Map.put(:session_id, sid)
      |> Core.Response.Attach.maybe_build_response_plan(response: :auto)

    assert_report_response_without_memory_ack(validated)

    assert %{status: "asserted", value: "the fence"} =
             Brain.Hippocampus.user_event(owner, "bird", "spatial_state")

    no_noun_evidence = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.id == "bird|noun|1"))}
    assert ParticipantSpatial.project(no_noun_evidence, si.sentence) == []

    other_pos = %{si | lifg_choices:
      Enum.map(si.lifg_choices, fn
        %{token_index: 1} = ch -> %{ch | id: "bird|verb|1"}
        ch -> ch
      end)}
    assert ParticipantSpatial.project(other_pos, si.sentence) == []
  end

  test "exact live trace: is|verb winner supplies auxiliary role and persists",
       %{sid: sid, owner: owner} do
    # In LiveView-equivalent production mode, Stage1 chooses is|verb|1,
    # not the synthetic is|auxiliary|0 winner used by earlier unit fixtures.
    # PMTG also leaves on/fence without word-level LIFG and ATL winners.
    si = input() |> Signatures.attach()

    choices =
      si.lifg_choices
      |> Enum.reject(&(Map.get(&1, :token_index) in [2, 4, 6]))
      |> Enum.concat([%{id: "is|verb|1", token_index: 2}])

    si = %{si | lifg_choices: choices, atl_slate: %{winners: choices}}

    assert [%{entity_key: "bird", relation: "spatial_state",
              participants: %{"actor" => "a bird", "position" => "on",
                              "location" => "the fence", "predicate" => "sitting"}}] =
             ParticipantSpatial.project(si, si.sentence)

    planned =
      si
      |> EpisodicProjection.attach()
      |> Map.put(:session_id, sid)
      |> Core.Response.Attach.maybe_build_response_plan(response: :auto)

    assert_report_response_without_memory_ack(planned)

    assert %{status: "asserted", value: "the fence",
              participants: %{"actor" => "a bird"}} =
             Brain.Hippocampus.user_event(owner, "bird", "spatial_state")

    assert {"On the fence.", %{memory_source: :hippocampal_event}} =
             EpisodicMemory.reply("Where is the bird?")

    # A forged auxiliary selection or a missing exact-word verb row cannot
    # silently pass as grammatical evidence.
    mismatched = %{si | lifg_choices:
      Enum.map(si.lifg_choices, fn
        %{token_index: 2} = ch -> %{ch | id: "is|noun|1"}
        ch -> ch
      end)}
    assert ParticipantSpatial.project(mismatched, si.sentence) == []

    no_verb_cell = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.id == "is|verb|1"))}
    assert ParticipantSpatial.project(no_verb_cell, si.sentence) == []

    foreign_verb =
      Enum.map(si.lifg_choices, fn
        %{token_index: 2} = ch -> %{ch | id: "was|verb|1"}
        ch -> ch
      end)
    wrong_lexeme = %{si | lifg_choices: foreign_verb,
                      atl_slate: %{winners: foreign_verb}}
    assert ParticipantSpatial.project(wrong_lexeme, si.sentence) == []
  end

  test "each question retrieves a different persisted participant role",
       %{sid: sid, owner: owner} do
    si = projected() |> Map.put(:session_id, sid)

    planned = Core.Response.Attach.maybe_build_response_plan(si, response: :auto)

    assert_report_response_without_memory_ack(planned)

    assert %{status: "asserted", participants: %{"actor" => "a bird", "predicate" => "sitting",
              "position" => "on", "location" => "the fence"}} =
             Brain.Hippocampus.user_event(owner, "bird", "spatial_state")

    checks = [
      {"Where is the bird?", "On the fence."},
      {"What is sitting on the fence?", "A bird."},
      {"What is the bird doing?", "Sitting."},
      {"what is the bird doing", "Sitting."},
      {"What was the bird doing?", "Sitting."},
      {"what was the bird doing", "Sitting."},
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

  test "question grammar is role-based and not bird-specific" do
    alias Core.Semantic.ParticipantQuestions, as: Questions

    assert {:forward, "cat", :location, %{}} =
             Questions.parse("Where is the cat?")

    assert {:forward, "cat", :predicate, %{}} =
             Questions.parse("What is the cat doing?")

    assert {:reverse, "resting", "under", "the table"} =
             Questions.parse("What is resting under the table?")

    assert {:forward, "cat", :location,
            %{predicate: "resting", position: "under"}} =
             Questions.parse("What is the cat resting under?")

    event = %{status: "asserted", relation: "spatial_state",
              participants: %{"actor" => "the cat", "predicate" => "resting",
                              "position" => "under", "location" => "the table"}}

    assert Questions.answer(event, :actor, %{}) == "The cat."
    assert Questions.answer(event, :location, %{}) == "Under the table."
    assert Questions.answer(event, :predicate, %{}) == "Resting."
    assert Questions.answer(event, :location, %{position: "on"}) == nil
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
