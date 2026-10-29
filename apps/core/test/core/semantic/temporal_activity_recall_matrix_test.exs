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
    result = TemporalActivityRecall.reply(session, question)
    assert match?({:ok, _, _}, result),
      "failed form #{inspect(question)} in session #{inspect(session)}: #{inspect(result)}"
    {:ok, response, meta} = result
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

  test "117 attested event properties resolve 24 natural activity and location forms" do
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

        # Anaphora is licensed only when the entire retained event record
        # has one actor KIND, and never claims real-world identity.
        for question <- [
          "What is it doing now?",
          "What's it doing?",
          "What’s it doing currently?",
          "Can you tell me what it is doing?"
        ] do
          assert_report(session, question, :latest, latest)
        end

        for question <- [
          "What was it doing?",
          "Tell me what it was doing before."
        ] do
          assert_report(session, question, :earlier, previous)
        end

        for question <- [
          "Where is it?",
          "Where's it now?"
        ] do
          assert {:ok, answer, meta} = TemporalActivityRecall.reply(session, question)
          assert answer =~ expected_location
          assert meta.requested_slot == :location
          assert meta.same_actor_identity_verified? == false
        end

        for {question, requested_slot} <- [
          {"Where was the #{actor}?", :location},
          {"Where was the #{actor} before?", :location},
          {"Where was the #{actor} earlier", :location},
          {"Tell me what the #{actor} was doing previously", :activity}
        ] do
          assert {:ok, response, meta} = TemporalActivityRecall.reply(session, question)
          assert meta.requested_temporal_role == :earlier
          assert meta.requested_slot == requested_slot

          case requested_slot do
            :location ->
              assert response =~ "a #{actor} on a fence"
              assert response =~ "does not establish actual event chronology"

            :activity ->
              assert response =~ "a #{actor} sitting on a fence"
              assert response =~ "report order, not verified event chronology"
          end
        end
      after
        Ledger.clear(session)
      end
    end
  end

  test "embedded activity questions preserve natural subject-auxiliary order" do
    session = "matrix-embedded-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved!(session, "bird", "sitting", "on", "fence")
    record_proved!(session, "bird", "flying", "into", "tree")

    for question <- [
      "Tell me what the bird is doing now.",
      "Can you tell me what the bird is doing?",
      "tell me what a bird is doing",
      "CAN YOU TELL ME WHAT THE BIRD IS DOING AT THE MOMENT?"
    ] do
      assert_report(session, question, :latest, "a bird flying into a tree")
    end

    for question <- [
      "Tell me what the bird was doing previously",
      "Can you tell me what the bird was doing before?",
      "tell me what a bird was doing",
      "Tell me what the bird was doing earlier."
    ] do
      assert_report(session, question, :earlier, "a bird sitting on a fence")
    end

    for question <- [
      "Tell me what the bird was doing now.",
      "Can you tell me what the bird is doing earlier?",
      "Tell me what the fox is doing now.",
      "Tell me what the bird will be doing now."
    ] do
      assert :not_applicable == TemporalActivityRecall.reply(session, question)
    end
  end

  test "direction and arrival questions reflect proof limits, never inferred arrival" do
    for actor <- @actors, landmark <- @landmarks do
      session = "arrival-#{actor}-#{landmark}"
      try do
        record_proved!(session, actor, "flying", "into", landmark)

        for question <- [
          "Did the #{actor} reach the #{landmark}?",
          "Did the #{actor} arrive at the #{landmark}?",
          "Has the #{actor} reached the #{landmark}?",
          "Has the #{actor} arrived at the #{landmark}?"
        ] do
          assert {:ok, text, meta} = TemporalActivityRecall.reply(session, question)
          assert text =~ "a #{actor} flying into a #{landmark}"
          assert text =~ "does not establish that it arrived"
          assert meta.requested_slot == :arrival
          assert meta.event_occurrence_verified? == false
          assert meta.same_actor_identity_verified? == false
          assert meta.llm_called? == false
        end

        assert {:ok, text, meta} =
          TemporalActivityRecall.reply(session, "Where is the #{actor} flying?")
        assert text =~ "a #{actor} flying into a #{landmark}"
        assert text =~ "not a verified destination or arrival"
        assert meta.requested_slot == :direction

        # The target is binding evidence, not something supplied by guessing
        # from the latest narrative when the query names another landmark.
        for other <- @landmarks -- [landmark] do
          assert :not_applicable == TemporalActivityRecall.reply(session,
            "Did the #{actor} reach the #{other}?")
        end

        assert :not_applicable == TemporalActivityRecall.reply(session,
          "Did the #{actor} reach the moon?")
        assert :not_applicable == TemporalActivityRecall.reply(session,
          "Has the fox reached the #{landmark}?")
      after
        Ledger.clear(session)
      end
    end
  end

  test "arrival and direction questions do not borrow earlier movement after later static report" do
    session = "arrival-stale-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved!(session, "bird", "flying", "into", "tree")
    record_proved!(session, "bird", "sitting", "on", "fence")

    assert :not_applicable ==
      TemporalActivityRecall.reply(session, "Did the bird reach the tree?")
    assert :not_applicable ==
      TemporalActivityRecall.reply(session, "Where is the bird flying?")
    assert_report(session, "What is it doing?", :latest, "a bird sitting on a fence")
  end

  test "count reports per actor kind without confusing mentions with proven descriptions" do
    session = "count-actor-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved!(session, "bird", "sitting", "on", "fence")
    record_proved!(session, "cat", "sleeping", "under", "table")
    record_proved!(session, "bird", "flying", "into", "tree")

    for question <- [
      "How many times have I described the bird?",
      "How many times did I describe the bird?",
      "How many descriptions of the bird have I given?",
      "How many reports of the bird did I give?"
    ] do
      assert {:ok, answer, meta} = TemporalActivityRecall.reply(session, question)
      assert answer =~ "2 interpretable descriptions of a bird"
      assert meta.requested_slot == :count
      assert meta.requested_temporal_role == :count
      assert meta.actual_event_chronology_verified? == false
      assert meta.same_actor_identity_verified? == false
    end

    assert {:ok, one, _} =
      TemporalActivityRecall.reply(session, "How many times have I described the cat?")
    assert one =~ "1 interpretable description of a cat"

    assert {:ok, zero, _} =
      TemporalActivityRecall.reply(session, "How many times did I describe the dog?")
    assert zero =~ "0 interpretable descriptions of a dog"

    # Mentions and questions are not verified descriptive reports.
    {:ok, _} = Owner.append(session, %{
      kind: :uninterpreted, status: :no_verified_semantics,
      factual_authority?: false
    })
    assert {:ok, same, _} =
      TemporalActivityRecall.reply(session, "How many reports of the bird did I give?")
    assert same =~ "2 interpretable descriptions of a bird"

    assert :not_applicable ==
      TemporalActivityRecall.reply(session, "How many times have I described the fox?")
    assert :not_applicable ==
      TemporalActivityRecall.reply(session, "How many birds are in the tree?")
  end

  test "report count refuses truncated session history" do
    session = "count-truncated-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved!(session, "bird", "sitting", "on", "fence")

    for _ <- 1..128 do
      {:ok, _} = Owner.append(session, %{
        kind: :uninterpreted, status: :no_verified_semantics,
        factual_authority?: false
      })
    end

    assert :not_applicable ==
      TemporalActivityRecall.reply(session, "How many times have I described the bird?")
  end

  test "mixed actor kinds make it ambiguous instead of choosing latest mention" do
    session = "pronoun-mixed-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved!(session, "bird", "sitting", "on", "fence")
    record_proved!(session, "cat", "sleeping", "under", "table")
    record_proved!(session, "bird", "flying", "into", "tree")

    for question <- [
      "What is it doing now?",
      "Where is it?",
      "What was it doing?"
    ] do
      assert {:ok, text, meta} = TemporalActivityRecall.reply(session, question)
      assert text =~ "Do you mean"
      assert text =~ "the bird"
      assert text =~ "the cat"
      assert meta.requested_temporal_role == :ambiguous_referent
      assert meta.retrieval_found? == false
      assert meta.same_actor_identity_verified? == false
    end

    assert_report(session, "Where is the bird now?", :latest,
      "a bird flying into a tree")
    assert :not_applicable ==
      TemporalActivityRecall.reply(session <> "-other", "What is it doing now?")
  end

  test "it questions without proven antecedents fail closed" do
    session = "pronoun-empty-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    for text <- [
      "What is it doing?",
      "Where is it now?",
      "What was it doing?",
      "Tell me what it was doing before."
    ] do
      assert :not_applicable == TemporalActivityRecall.reply(session, text)
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
