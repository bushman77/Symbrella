defmodule Core.Agency.LexicalRoleAccountabilityTest do
  use ExUnit.Case, async: false

  alias Core.Agency.{
    LexicalRoleAccountability,
    LexicalRoleShadow,
    SemanticCorrectionEvaluator
  }
  alias Core.Response.AgencyMemory.Source
  alias Db.AgencyEvents

  setup do
    # Test config normally disables the agency ledger. Enable it only for
    # these synchronous storage tests and restore it after each case.
    previous = Application.fetch_env(:core, :agency_ledger_enabled?)
    Application.put_env(:core, :agency_ledger_enabled?, true)

    on_exit(fn ->
      case previous do
        {:ok, value} -> Application.put_env(:core, :agency_ledger_enabled?, value)
        :error -> Application.delete_env(:core, :agency_ledger_enabled?)
      end
    end)

    :ok
  end

  defp audited_input(session_id) do
    %Core.SemanticInput{
      source: :prod,
      session_id: session_id,
      experience_origin: :user_turn,
      frame_run_id: 42,
      frame_seq: 3,
      sentence: "The bird is flying.",
      tokens: [%{index: 0, phrase: "The"}, %{index: 1, phrase: "bird"}],
      lifg_choices: [
        %{token_index: 0, id: "the|determiner|1"},
        %{token_index: 1, id: "bird|noun|1"}
      ],
      trace: []
    }
    |> LexicalRoleShadow.attach()
  end

  test "accountability payload contains bounded provenance but no source prose or sense IDs" do
    si = audited_input("audit-payload")

    assert {:ok, attrs} = LexicalRoleAccountability.attrs(si)
    assert attrs.source == "lexical_role_accountability"
    assert attrs.action == "audit_lexical_role"
    assert attrs.input.frame_run_id == 42
    assert attrs.input.frame_seq == 3
    assert attrs.input.token_count == 2
    assert byte_size(attrs.input.original_lifg_sha256) == 64
    assert attrs.decision.status == :abstained
    assert attrs.decision.blocked_roles == []
    assert attrs.decision.alternatives == []
    assert attrs.reasons.structural_evidence == :not_established
    assert attrs.reasons.alternative_evidence == :none
    assert attrs.outcome.choice_fingerprints_comparable?
    refute attrs.outcome.selected_choices_changed_after_audit?
    refute attrs.outcome.lexical_sense_verified?
    refute attrs.outcome.learning_applied?
    refute attrs.reflection.outcome_verified?

    dumped = inspect(attrs)
    refute dumped =~ "The bird is flying."
    refute dumped =~ "bird|noun|1"
    refute dumped =~ "the|determiner|1"
  end

  test "durably stores an actual role audit without changing live lexical decisions" do
    session = "audit-#{System.unique_integer([:positive])}"
    si = audited_input(session)
    selected_before = si.lifg_choices

    recorded = LexicalRoleAccountability.record(si)

    assert %{stage: :reflective_accountability, decision: :persisted,
             reason: :agency_event_recorded, meta: %{ledger_id: id}} =
      Enum.find(recorded.trace, &match?(%{stage: :reflective_accountability}, &1))

    assert is_integer(id) and id > 0
    assert recorded.lifg_choices == selected_before

    assert [event] = LexicalRoleAccountability.recent(session, limit: 5)
    assert event.id == id
    assert event.source == "lexical_role_accountability"
    assert event.status == "observed"
    assert event.action == "audit_lexical_role"
    assert event.decision["status"] == "abstained"
    assert event.outcome["lexical_sense_verified?"] == false
    assert event.outcome["learning_applied?"] == false
    assert event.reflection["outcome_verified?"] == false

    # The existing response-learning window must not treat audit-only rows
    # as behavioral outcomes or silently have its finite limit diluted.
    assert [] = Source.recent(session, limit: 5)

    assert [same] = AgencyEvents.recent(
      session_id: session,
      include_sources: ["lexical_role_accountability"]
    )
    assert same.id == id
  end

  test "durable role receipt carries only the live label-free semantic evaluation" do
    si = audited_input("semantic-audit-attrs")
    |> SemanticCorrectionEvaluator.attach()

    assert {:ok, attrs} = LexicalRoleAccountability.attrs(si)

    evaluation = attrs.decision.semantic_evaluation
    assert evaluation.status == :not_evaluable
    assert evaluation.reference_status == :not_provided
    refute evaluation.independently_verified?
    refute evaluation.learning_applied?
    refute evaluation.policy_change_authorized?
    refute Map.has_key?(evaluation, :supplied_gold_sense_id)

    assert si.lifg_choices == audited_input("semantic-audit-attrs").lifg_choices
  end

  test "changes after the original audit are visible but not promoted to verified corrections" do
    si = audited_input("audit-mutation-fixture")
    changed = %{si | lifg_choices: [
      %{token_index: 0, id: "the|determiner|1"},
      %{token_index: 1, id: "bird|noun|2"}
    ]}

    assert {:ok, attrs} = LexicalRoleAccountability.attrs(changed)
    assert attrs.outcome.choice_fingerprints_comparable?
    assert attrs.outcome.selected_choices_changed_after_audit?
    refute attrs.outcome.lexical_sense_verified?
    refute attrs.outcome.learning_applied?
    assert changed.lifg_choices != si.lifg_choices
  end

  test "missing session or audit evidence is recorded as an explicit non-persistence outcome" do
    si = audited_input(nil)

    skipped = LexicalRoleAccountability.record(si)
    assert %{decision: :skipped, reason: :missing_session_scope} =
      Enum.find(skipped.trace, &match?(%{stage: :reflective_accountability}, &1))

    absent = %{si | session_id: "audit-missing", trace: []}
    unavailable = LexicalRoleAccountability.record(absent)

    assert %{decision: :unavailable, reason: :missing_role_audit} =
      Enum.find(unavailable.trace, &match?(%{stage: :reflective_accountability}, &1))

    assert [] == LexicalRoleAccountability.recent("", limit: 10)
  end

  test "disabled durable ledger still preserves a visible in-memory audit" do
    session = "audit-disabled-#{System.unique_integer([:positive])}"
    si = audited_input(session)
    Application.put_env(:core, :agency_ledger_enabled?, false)

    result = LexicalRoleAccountability.record(si)

    assert %{decision: :skipped, reason: :agency_ledger_disabled} =
      Enum.find(result.trace, &match?(%{stage: :reflective_accountability}, &1))

    assert Enum.any?(result.trace, &match?(%{stage: :reflective_role_shadow}, &1))
    assert [] == LexicalRoleAccountability.recent(session)
    assert result.lifg_choices == si.lifg_choices
  end

  test "source-filtered agency recall keeps audit rows separate from response outcomes" do
    session = "audit-filter-#{System.unique_integer([:positive])}"
    si = audited_input(session)

    assert %{trace: trace} = LexicalRoleAccountability.record(si)
    assert Enum.any?(trace, &match?(%{stage: :reflective_accountability,
                                     decision: :persisted}, &1))

    assert {:ok, response} =
      AgencyEvents.create_event(%{session_id: session, source: "core_response",
        status: "observed", action: "answer"})

    assert [response_only] = Source.recent(session, limit: 1)
    assert response_only.id == response.id

    assert [audit_only] = LexicalRoleAccountability.recent(session, limit: 1)
    assert audit_only.source == "lexical_role_accountability"
    assert audit_only.id != response.id

    assert [first, second] = AgencyEvents.recent(session_id: session, limit: 2)
    assert Enum.sort([first.id, second.id]) ==
      Enum.sort([audit_only.id, response.id])
  end
end
