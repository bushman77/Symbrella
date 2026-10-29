defmodule Core.Semantic.TemporalActivityRecallMatrixTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Discourse.Ledger.Owner
  alias Core.Semantic.Composition.Event
  alias Core.Semantic.TemporalActivityRecall

  # The same closed domain as Composition.Lexical and Composition.Event.
  # This table is intentionally exhaustive over supported event properties,
  # not a claim that arbitrary verbs or prepositions have been understood.
  @actors ~w(bird cat dog)
  @landmarks ~w(fence tree table)
  @stationary_relations ~w(on in under beside above below)
  @valid_activities (
    for verb <- ~w(sitting sleeping), relation <- @stationary_relations,
      do: {verb, relation}
  ) ++ [{"flying", "into"}]

  setup do
    if not is_pid(Process.whereis(Owner)), do: start_supervised!(Owner)
    :ok
  end

  defp record_proved!(session, actor, verb, relation, landmark) do
    assert {:ok, proof} = Event.compose(%{
      predicate: verb,
      actor: actor,
      preposition: relation,
      landmark: landmark
    })
    assert :ok == Event.verify(proof)

    semantics = proof.semantics
    digest =
      :crypto.hash(:sha256, :erlang.term_to_binary(proof.result))
      |> Base.encode16(case: :lower)

    {:ok, _seq} = Owner.append(session, %{
      kind: :reported_progressive_event,
      status: :provisional,
      actor_kind: semantics.actor.kind,
      action: semantics.predicate,
      relation: semantics.spatial.relation,
      landmark: semantics.spatial.landmark_kind,
      reading: semantics.spatial.reading,
      arrival: :not_established,
      typed_proof_sha256: digest,
      event_existence: :not_asserted,
      referent_verified?: false,
      event_occurrence_verified?: false,
      provenance: :revalidated_event_composition,
      verification: :conversation_context_only,
      factual_authority?: false
    })
  end

  defp assert_report(session, question, role, expected) do
    assert {:ok, response, meta} = TemporalActivityRecall.reply(session, question),
      "failed form: #{inspect(question)}"
    assert String.contains?(response, expected),
      "wrong reported event for #{inspect(question)}: #{inspect(response)}"
    assert meta.requested_temporal_role == role
    assert meta.memory_source == :same_session_progressive_report
    assert meta.event_occurrence_verified? == false
    assert meta.actual_event_chronology_verified? == false
    assert meta.same_actor_identity_verified? == false
    assert meta.factual_authority? == false
    assert meta.llm_called? == false
  end

  test "all 117 attested event properties retrieve the correct role under twelve question variants" do
    for actor <- @actors, {verb, relation} <- @valid_activities,
        landmark <- @landmarks do
      session = "matrix-#{actor}-#{verb}-#{relation}-#{landmark}"
      try do
        record_proved!(session, actor, "sitting", "on", "fence")
        record_proved!(session, actor, verb, relation, landmark)

        latest = "a #{actor} #{verb} #{relation} a #{landmark}"
        previous = "a #{actor} sitting on a fence"

        current_forms = [
          "What is the #{actor} doing now?",
          "what is the #{actor} doing currently",
          "What is the #{actor} doing right now?",
          "WHAT IS THE #{String.upcase(actor)} DOING AT THE MOMENT?",
          "What's the #{actor} doing now?",
          "What’s a #{actor} doing currently?"
        ]

        earlier_forms = [
          "What was the #{actor} doing?",
          "what was the #{actor} doing",
          "What was the #{actor} doing earlier?",
          "WHAT WAS THE #{String.upcase(actor)} DOING BEFORE?",
          "What did the #{actor} do before?",
          "what did a #{actor} do previously"
        ]

        for question <- current_forms,
          do: assert_report(session, question, :latest, latest)

        for question <- earlier_forms,
          do: assert_report(session, question, :earlier, previous)

        # The ordinary present form must not fall through to a stale
        # owner-wide spatial_state once a directional report has followed it.
        present_paraphrases = [
          "What is the #{actor} doing?",
          "What's the #{actor} doing?",
          "Tell me what the #{actor} is doing now.",
          "Can you tell me what the #{actor} is doing?",
          "Where is the #{actor}?",
          "Where is the #{actor} now?",
          "Where's the #{actor}?",
          "Where’s a #{actor} right now?"
        ]

        for question <- Enum.take(present_paraphrases, 4),
          do: assert_report(session, question, :latest, latest)

        expected_location =
          if relation == "into" do
            "a #{actor} flying into a #{landmark}"
          else
            "a #{actor} #{relation} a #{landmark}"
          end

        for question <- Enum.drop(present_paraphrases, 4) do
          assert {:ok, response, meta} =
                   TemporalActivityRecall.reply(session, question),
                   "unanswered location form: #{inspect(question)}"
          assert response =~ expected_location
          assert meta.requested_slot == :location
          assert meta.requested_temporal_role == :latest
          assert meta.event_occurrence_verified? == false
          if relation == "into" do
            assert response =~ "Arrival and current location are not established."
            refute response =~ "places a #{actor} in a #{landmark}"
          end
        end

        for question <- [
          "Where was the #{actor}?",
          "Where was the #{actor} before?",
          "Where was the #{actor} earlier",
          "Tell me what the #{actor} was doing previously"
        ] do
          assert {:ok, response, meta} = TemporalActivityRecall.reply(session, question)
          assert response =~ "a #{actor} sitting on a fence"
          assert meta.requested_temporal_role == :earlier
          assert meta.requested_slot in [:activity, :location]
        end
      after
        Ledger.clear(session)
      end
    end
  end

  test "temporal parser refuses contradictory modifiers and unsupported requests" do
    session = "matrix-negative-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)
    record_proved!(session, "bird", "sitting", "on", "fence")
    record_proved!(session, "bird", "flying", "into", "tree")

    for question <- [
      "What was the bird doing now?",
      "What was the bird doing currently?",
      "What is the bird doing earlier?",
      "What did the bird do?",
      "Where was the bird now?",
      "Where is the bird earlier?",
      "What is the fox doing now?",
      "What is the bird eating now?",
      "What is the bird doing next?",
      "What was the bird doing yesterday?",
      "What is the bird doing now and why?",
      "What is the birds doing now?"
    ] do
      assert :not_applicable == TemporalActivityRecall.reply(session, question),
        "unexpectedly accepted #{inspect(question)}"
    end
  end

  test "exact actor matching survives interleaved reports and never crosses sessions" do
    session = "matrix-interleave-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)
    record_proved!(session, "bird", "sitting", "on", "fence")
    record_proved!(session, "cat", "sleeping", "under", "table")
    record_proved!(session, "bird", "flying", "into", "tree")
    record_proved!(session, "dog", "sitting", "beside", "fence")

    assert_report(session, "What was the bird doing?", :earlier, "a bird sitting on a fence")
    assert_report(session, "What is the bird doing now?", :latest, "a bird flying into a tree")
    assert_report(session, "What is the dog doing currently?", :latest, "a dog sitting beside a fence")
    assert :not_applicable == TemporalActivityRecall.reply(session, "What was the dog doing?")
    assert :not_applicable == TemporalActivityRecall.reply(session <> "-other",
      "What is the bird doing now?")
  end

  test "three qualified actor reports require clarification instead of guessing an earlier one" do
    session = "matrix-ambiguous-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)
    record_proved!(session, "bird", "sitting", "on", "fence")
    record_proved!(session, "bird", "sleeping", "under", "table")
    record_proved!(session, "bird", "flying", "into", "tree")

    assert {:ok, question, meta} =
      TemporalActivityRecall.reply(session, "What was the bird doing?")
    assert question =~ "Which earlier description"
    assert meta.requested_temporal_role == :ambiguous_earlier
    assert meta.retrieval_found? == false

    assert_report(session, "What is the bird doing now?", :latest, "a bird flying into a tree")
  end

  test "incomplete retention window fails closed instead of treating it as full history" do
    session = "matrix-retention-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)
    record_proved!(session, "bird", "sitting", "on", "fence")
    record_proved!(session, "bird", "flying", "into", "tree")

    for _ <- 1..127 do
      {:ok, _} = Owner.append(session, %{
        kind: :uninterpreted, status: :no_verified_semantics,
        factual_authority?: false
      })
    end

    assert length(Ledger.entries(session)) == 129
    assert :not_applicable == TemporalActivityRecall.reply(
      session, "What was the bird doing?"
    )
    assert :not_applicable == TemporalActivityRecall.reply(
      session, "What is the bird doing now?"
    )
  end

  test "only the compositional event vocabulary authorizes test event fixtures" do
    for {verb, relation} <- [
      {"flying", "on"}, {"sitting", "into"}, {"sleeping", "into"},
      {"dancing", "on"}, {"running", "under"}, {"flying", "under"}
    ] do
      assert :unsupported == Event.compose(%{
        predicate: verb, actor: "bird", preposition: relation, landmark: "tree"
      })
    end

    assert :unsupported == Event.compose(%{
      predicate: "sitting", actor: "fox", preposition: "on", landmark: "fence"
    })
    assert :unsupported == Event.compose(%{
      predicate: "sitting", actor: "bird", preposition: "on", landmark: "moon"
    })
  end
end
