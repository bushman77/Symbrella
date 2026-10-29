defmodule Core.Semantic.ReportVerificationTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Discourse.Ledger.Owner
  alias Core.Semantic.Composition.Event
  alias Core.Semantic.ReportVerification

  @actors ~w(bird cat dog)
  @landmarks ~w(fence tree table)
  @static ~w(on in under beside above below)
  @event_forms (for verb <- ~w(sitting sleeping), relation <- @static,
                do: {verb, relation}) ++ [{"flying", "into"}]

  setup do
    if not is_pid(Process.whereis(Owner)), do: start_supervised!(Owner)
    :ok
  end

  defp record!(session, actor, verb, relation, landmark, overrides \\ %{}) do
    assert {:ok, proof} = Event.compose(%{
      actor: actor, predicate: verb, preposition: relation, landmark: landmark
    })
    assert :ok == Event.verify(proof)

    semantics = proof.semantics
    digest =
      :crypto.hash(:sha256, :erlang.term_to_binary(proof.result))
      |> Base.encode16(case: :lower)

    record = %{
      kind: :reported_progressive_event, status: :provisional,
      actor_kind: semantics.actor.kind, action: semantics.predicate,
      relation: semantics.spatial.relation,
      landmark: semantics.spatial.landmark_kind,
      reading: semantics.spatial.reading,
      arrival: :not_established, event_existence: :not_asserted,
      typed_proof_sha256: digest,
      referent_verified?: false, event_occurrence_verified?: false,
      provenance: :revalidated_event_composition,
      verification: :conversation_context_only,
      factual_authority?: false
    }

    assert {:ok, _} = Owner.append(session, Map.merge(record, overrides))
  end

  test "117 qualified event properties support four natural report-confirmation forms" do
    for actor <- @actors, {verb, relation} <- @event_forms,
        landmark <- @landmarks do
      session = "report-confirm-#{actor}-#{verb}-#{relation}-#{landmark}"
      try do
        record!(session, actor, verb, relation, landmark)

        for question <- [
          "Did I say the #{actor} was #{verb} #{relation} the #{landmark}?",
          "Did I describe a #{actor} #{verb} #{relation} a #{landmark}?",
          "Have I said that the #{String.upcase(actor)} is #{verb} #{relation} the #{landmark}?",
          "have i mentioned the #{actor} #{verb} #{relation} the #{landmark}"
        ] do
          assert {:ok, response, metadata} = ReportVerification.reply(session, question),
            "unanswered question: #{inspect(question)}"
          assert response =~ "Yes. You described a #{actor} #{verb} #{relation} a #{landmark}"
          assert response =~ "not that the event occurred"
          assert metadata.retrieval_found? == true
          assert metadata.report_seq == 1
          assert metadata.matching_reports == 1
          assert metadata.requested_slot == :reported_event_confirmation
          assert metadata.llm_called? == false
          assert metadata.event_occurrence_verified? == false
          assert metadata.same_actor_identity_verified? == false
          assert metadata.factual_authority? == false
        end
      after
        Ledger.clear(session)
      end
    end
  end

  test "absence of matching interpreted report is not asserted as a factual denial" do
    session = "report-nonmatch-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record!(session, "bird", "sitting", "on", "fence")
    record!(session, "cat", "sleeping", "under", "table")

    for question <- [
      "Did I say the bird was sitting on the tree?",
      "Did I describe a bird sleeping under a table?",
      "Did I mention the dog sitting on the fence?"
    ] do
      assert {:ok, response, metadata} = ReportVerification.reply(session, question)
      assert response =~ "can't find a matching interpreted report"
      assert response =~ "does not establish you never said it"
      assert metadata.retrieval_found? == false
      assert metadata.matching_reports == 0
      assert metadata.report_seq == nil
    end

    assert {:ok, yes, _} =
      ReportVerification.reply(session, "Did I say the cat was sleeping under the table?")
    assert yes =~ "Yes. You described a cat sleeping under a table"
  end

  test "identical descriptions count as two reports, not proof of one individual's identity" do
    session = "report-repeat-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record!(session, "bird", "sitting", "on", "fence")
    record!(session, "bird", "sitting", "on", "fence")

    assert {:ok, answer, metadata} =
      ReportVerification.reply(session, "Did I say the bird was sitting on the fence?")

    assert answer =~ "Yes. You described"
    assert metadata.matching_reports == 2
    assert metadata.report_seq == 2
    assert metadata.same_actor_identity_verified? == false
  end

  test "tampered proof, failed provenance or fabricated events never confirm a report" do
    session = "report-invalid-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record!(session, "bird", "sitting", "on", "fence",
      %{typed_proof_sha256: String.duplicate("a", 64)})
    record!(session, "cat", "sleeping", "under", "table",
      %{event_occurrence_verified?: true})
    record!(session, "dog", "sitting", "under", "tree",
      %{provenance: :invented_from_llm})

    for question <- [
      "Did I say the bird was sitting on the fence?",
      "Did I say the cat was sleeping under the table?",
      "Did I say the dog was sitting under the tree?"
    ] do
      assert {:ok, response, metadata} = ReportVerification.reply(session, question)
      assert response =~ "can't find a matching interpreted report"
      assert metadata.retrieval_found? == false
    end
  end

  test "unsupported statements or dangerous assertions cannot turn into confirmations" do
    session = "report-out-of-vocab-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)
    record!(session, "bird", "flying", "into", "tree")

    for question <- [
      "Did I say the bird was flying on the tree?",
      "Did I say the bird was sitting into the tree?",
      "Did I say the bird was flarping on the fence?",
      "Did I say the fox was sitting on the fence?",
      "Did I say the bird was sitting on the moon?",
      "Did I say the bird wasn't flying into the tree?",
      "Did the bird actually fly into the tree?",
      "Did I say the bird has arrived at the tree?",
      "Did I describe the birds flying into the tree?"
    ] do
      assert :not_applicable == ReportVerification.reply(session, question),
        "unexpectedly accepted: #{inspect(question)}"
    end
  end

  test "nonmatches require complete retained history and never cross sessions" do
    session = "report-retention-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)
    record!(session, "bird", "sitting", "on", "fence")

    assert {:ok, missing, metadata} =
      ReportVerification.reply(session, "Did I say the bird was flying into the tree?")
    assert missing =~ "can't find"
    assert metadata.retrieval_found? == false

    assert {:ok, _, _} =
      ReportVerification.reply(session <> "-other",
        "Did I say the bird was sitting on the fence?")

    for _ <- 1..128 do
      assert {:ok, _} = Owner.append(session, %{
        kind: :uninterpreted, status: :no_verified_semantics,
        factual_authority?: false
      })
    end

    assert :not_applicable ==
      ReportVerification.reply(session, "Did I say the bird was sitting on the fence?")
  end

  test "empty valid session returns epistemic nonmatch without inventing a report" do
    session = "report-empty-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    assert {:ok, response, metadata} =
      ReportVerification.reply(session, "Have I described a bird sitting on the fence?")
    assert response =~ "can't find a matching interpreted report"
    assert metadata.retrieval_found? == false
  end
end
