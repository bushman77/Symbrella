defmodule Core.Release.OperationalAwarenessV1Test do
  use ExUnit.Case, async: true

  alias Core.Release.OperationalAwarenessV1, as: Readiness

  defp receipt(criterion, index, opts \\ []) do
    source =
      Keyword.get(opts, :source, hd(criterion.accepted_sources))

    %{
      criterion: criterion.id,
      status: :passed,
      evidence: %{
        id: "fixture-#{index}",
        source: source,
        revision: String.duplicate("a", 40),
        artifact_sha256: String.duplicate("b", 64),
        observed_at_ms: Keyword.get(opts, :at, System.system_time(:millisecond)),
        synthetic?: Keyword.get(opts, :synthetic?, false)
      }
    }
  end

  test "the v1 contract is finite, explicitly named and does not expand silently" do
    ids = Enum.map(Readiness.criteria(), & &1.id)

    assert ids == [
      :self_state_grounding,
      :decision_accountability,
      :independent_verification,
      :unified_control_gate,
      :reversible_learning,
      :continuity_after_restart,
      :code_host_grounding,
      :longitudinal_demonstration
    ]

    assert length(ids) == length(Enum.uniq(ids))
    assert Enum.all?(Readiness.criteria(),
      &(is_binary(&1.title) and is_binary(&1.requires) and &1.accepted_sources != []))
  end

  test "an empty v1 evidence register is blocked, never approved" do
    summary = Readiness.assess()

    assert summary.status == :blocked
    assert summary.total == 8
    assert summary.declared == 0
    assert summary.missing == 8
    assert summary.invalid == 0
    refute summary.ready_for_external_audit?
    refute summary.externally_verified?
    refute summary.release_approved?
    refute summary.consciousness_proven?
    refute summary.changes_applied?
    assert Enum.all?(summary.criteria, &(&1.status == :missing))
  end

  test "omitted --strict is non-fatal, while explicit strict requires full receipts" do
    blocked = Readiness.assess([])

    refute Readiness.strict_failure?(nil, blocked)
    refute Readiness.strict_failure?(false, blocked)
    assert Readiness.strict_failure?(true, blocked)
    assert Readiness.strict_failure?(true, Readiness.assess(:invalid))

    receipts =
      Readiness.criteria()
      |> Enum.with_index(1)
      |> Enum.map(fn {criterion, index} -> receipt(criterion, index) end)

    ready_for_audit = Readiness.assess(receipts)
    assert ready_for_audit.status == :ready_for_external_audit
    refute Readiness.strict_failure?(true, ready_for_audit)
    refute ready_for_audit.release_approved?
  end

  test "partial evidence remains blocked with explicit missing criteria" do
    [first | _] = Readiness.criteria()
    summary = Readiness.assess([receipt(first, 1)])

    assert summary.status == :blocked
    assert summary.declared == 1
    assert summary.missing == 7
    assert %{id: :self_state_grounding, status: :declared} = hd(summary.criteria)
    assert Enum.any?(summary.criteria, &(&1.id == :reversible_learning and
      &1.status == :missing))
  end

  test "even eight shaped claims only unlock an external audit, never release approval" do
    receipts = Readiness.criteria()
      |> Enum.with_index(1)
      |> Enum.map(fn {criterion, index} -> receipt(criterion, index) end)

    untouched = receipts
    summary = Readiness.assess(receipts)

    assert summary.status == :ready_for_external_audit
    assert summary.ready_for_external_audit?
    assert summary.declared == 8
    assert summary.missing == 0
    assert summary.invalid == 0
    refute summary.evidence_issuer_authenticated?
    refute summary.externally_verified?
    refute summary.release_approved?
    refute summary.consciousness_proven?
    refute summary.changes_applied?
    assert Enum.all?(summary.criteria,
      &(&1.status == :declared and &1.reason == :not_authenticated))
    assert receipts == untouched
  end

  test "JSON-like string keys cannot bypass the strict source and fingerprint checks" do
    criterion = hd(Readiness.criteria())
    valid = receipt(criterion, 1)

    json = %{
      "criterion" => Atom.to_string(valid.criterion),
      "status" => "passed",
      "evidence" => %{
        "id" => valid.evidence.id,
        "source" => Atom.to_string(valid.evidence.source),
        "revision" => valid.evidence.revision,
        "artifact_sha256" => valid.evidence.artifact_sha256,
        "observed_at_ms" => valid.evidence.observed_at_ms,
        "synthetic?" => false
      }
    }

    assert Readiness.assess([json]).declared == 1

    forged = put_in(json, ["evidence", "artifact_sha256"], "fake")
    summary = Readiness.assess([forged])

    assert summary.invalid == 1
    assert %{status: :invalid, reason: :invalid_artifact_fingerprint} =
      hd(summary.criteria)
  end

  test "self-certified or synthetic semantic labels are not acceptable as independent review" do
    criterion = Enum.find(Readiness.criteria(),
      &(&1.id == :independent_verification))

    untrusted = Readiness.assess([receipt(criterion, 1, source: :ci)])
    assert untrusted.invalid == 1
    assert Enum.find(untrusted.criteria,
      &(&1.id == :independent_verification)).reason == :wrong_evidence_source

    synthetic = Readiness.assess([receipt(criterion, 1,
      source: :independent_review, synthetic?: true)])
    assert synthetic.invalid == 1
    assert Enum.find(synthetic.criteria,
      &(&1.id == :independent_verification)).reason ==
             :synthetic_or_unspecified_evidence
  end

  test "stale, future, or unstamped receipts are rejected" do
    criterion = hd(Readiness.criteria())
    now = System.system_time(:millisecond)
    old = Readiness.assess([receipt(criterion, 1, at: now - 91 * 86_400_000)])
    future = Readiness.assess([receipt(criterion, 1, at: now + 86_400_000)])
    unknown = Readiness.assess([receipt(criterion, 1, at: nil)])

    assert hd(old.criteria).reason == :stale_evidence
    assert hd(future.criteria).reason == :invalid_observation_time
    assert hd(unknown.criteria).reason == :invalid_observation_time
  end

  test "duplicate criterion and duplicate artifact receipts fail closed" do
    [first, second | _] = Readiness.criteria()
    duplicate = receipt(first, 1)

    assert Readiness.assess([duplicate, duplicate]).reason == :duplicate_criterion

    shared = receipt(second, 2)
    reused = put_in(shared, [:evidence, :id], duplicate.evidence.id)

    assert Readiness.assess([duplicate, reused]).reason == :duplicate_evidence_id
  end

  test "unknown gates, malformed receipts and oversized batches are rejected" do
    first = hd(Readiness.criteria())
    unknown = %{receipt(first, 1) | criterion: :consciousness_proven}

    assert Readiness.assess([unknown]).reason == :unknown_or_malformed_receipt
    assert Readiness.assess([%{}]).reason == :unknown_or_malformed_receipt
    assert Readiness.assess(:passed).reason == :invalid_receipt_batch

    excessive =
      Enum.map(1..33, fn index ->
        %{receipt(first, index) | criterion: :made_up}
      end)
    assert Readiness.assess(excessive).reason == :invalid_receipt_batch
  end

  test "a declared 'passed' without provenance never becomes a release criterion" do
    first = hd(Readiness.criteria())
    incomplete = %{
      criterion: first.id,
      status: :passed,
      evidence: %{id: "manual-claim"}
    }

    summary = Readiness.assess([incomplete])

    assert summary.status == :blocked
    assert summary.invalid == 1
    refute summary.ready_for_external_audit?
    refute summary.release_approved?
  end
end
