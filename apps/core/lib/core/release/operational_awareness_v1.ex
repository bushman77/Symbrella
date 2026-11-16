defmodule Core.Release.OperationalAwarenessV1 do
  @moduledoc """
  Fixed, non-cognitive release-readiness contract for operational self-awareness v1.

  This is an *evidence register*, NOT an oracle. It checks the shape, freshness,
  attribution and coverage of declared receipts, but cannot authenticate their
  issuers or independently reproduce their claims. A complete register means
  "ready for external audit", NEVER "self-awareness verified" or "release
  approved".

  No model, memory, Working Memory, LIFG, GoalStack, policy or autonomy state
  is read or modified by this module.
  """

  @version 1
  @max_receipts 32
  @max_evidence_age_ms 90 * 24 * 60 * 60 * 1_000
  @sha256 ~r/\A[0-9a-f]{64}\z/
  @revision ~r/\A[0-9a-f]{40}\z/
  @id ~r/\A[a-zA-Z0-9_.:-]{1,128}\z/

  @criteria [
    %{
      id: :self_state_grounding,
      title: "Live, evidence-grounded self-state",
      accepted_sources: [:ci, :runtime_probe],
      requires: "Versioned bounded introspection, uncertainty, and honest failure states"
    },
    %{
      id: :decision_accountability,
      title: "End-to-end decision accountability",
      accepted_sources: [:ci, :runtime_probe],
      requires: "Trace-to-durable-ledger provenance; failures are not promoted to success"
    },
    %{
      id: :independent_verification,
      title: "Independent outcome and semantic verification",
      accepted_sources: [:independent_review],
      requires: "Frozen reviewed holdout, separate oracle, coverage and regression results"
    },
    %{
      id: :unified_control_gate,
      title: "Canonical cognitive-control boundary",
      accepted_sources: [:ci, :controlled_experiment],
      requires: "All Working Memory ingress paths obey BasalGanglia hard evidence gates"
    },
    %{
      id: :reversible_learning,
      title: "Verified, bounded and reversible learning",
      accepted_sources: [:controlled_experiment],
      requires: "Predeclared change bounds, held-out benefit, regression budget and rollback"
    },
    %{
      id: :continuity_after_restart,
      title: "Evidence-preserving continuity",
      accepted_sources: [:controlled_experiment],
      requires: "Verified state/strategy retention across reboot; stale claims rejected"
    },
    %{
      id: :code_host_grounding,
      title: "Grounded software and host self-model",
      accepted_sources: [:ci, :runtime_probe],
      requires: "Read-only BEAM/module/host observations separated from claims and unknowns"
    },
    %{
      id: :longitudinal_demonstration,
      title: "Independent longitudinal self-regulation demonstration",
      accepted_sources: [:controlled_experiment, :independent_review],
      requires: "Mistake -> verification -> bounded adaptation -> better new decisions -> replay"
    }
  ]

  @doc "Fixed v1 requirements. New goals require a deliberate versioned contract change."
  @spec criteria() :: [map()]
  def criteria, do: @criteria

  @doc "Return a fail-closed readiness assessment from declared evidence receipts."
  @spec assess(term()) :: map()
  def assess(receipts \\ [])

  def assess(receipts) when is_list(receipts) and length(receipts) <= @max_receipts do
    now_ms = System.system_time(:millisecond)
    prepared = Enum.map(receipts, &prepare/1)
    known_ids = Enum.map(@criteria, & &1.id)

    invalid_unknown =
      Enum.any?(prepared, fn
        %{criterion: id} -> id not in known_ids
        _ -> true
      end)

    ids = Enum.map(prepared, &Map.get(&1, :criterion))
    evidence_ids = Enum.map(prepared, &get_in(&1, [:evidence, :id]))

    cond do
      invalid_unknown ->
        rejected(:unknown_or_malformed_receipt)

      length(ids) != length(Enum.uniq(ids)) ->
        rejected(:duplicate_criterion)

      Enum.any?(evidence_ids, &is_nil/1) ->
        assessment(prepared, now_ms)

      length(evidence_ids) != length(Enum.uniq(evidence_ids)) ->
        rejected(:duplicate_evidence_id)

      true ->
        assessment(prepared, now_ms)
    end
  end

  def assess(_), do: rejected(:invalid_receipt_batch)

  @doc """
  CLI strict-mode exit decision. An omitted flag is false, and incomplete or
  malformed evidence makes strict mode fail with a nonzero exit status.
  """
  @spec strict_failure?(term(), term()) :: boolean()
  def strict_failure?(true, %{ready_for_external_audit?: true}), do: false
  def strict_failure?(true, _assessment), do: true
  def strict_failure?(_flag, _assessment), do: false

  defp assessment(prepared, now_ms) do
    rows =
      Enum.map(@criteria, fn criterion ->
        case Enum.find(prepared, &(Map.get(&1, :criterion) == criterion.id)) do
          nil ->
            %{id: criterion.id, status: :missing, reason: :no_receipt}

          receipt ->
            case valid_receipt?(receipt, criterion, now_ms) do
              :ok -> %{id: criterion.id, status: :declared, reason: :not_authenticated}
              {:error, reason} -> %{id: criterion.id, status: :invalid, reason: reason}
            end
        end
      end)

    declared = Enum.count(rows, &(&1.status == :declared))
    missing = Enum.count(rows, &(&1.status == :missing))
    invalid = Enum.count(rows, &(&1.status == :invalid))

    %{
      v: @version,
      status: if(declared == length(@criteria), do: :ready_for_external_audit, else: :blocked),
      total: length(@criteria),
      declared: declared,
      missing: missing,
      invalid: invalid,
      criteria: rows,
      ready_for_external_audit?: declared == length(@criteria),
      evidence_issuer_authenticated?: false,
      externally_verified?: false,
      release_approved?: false,
      consciousness_proven?: false,
      changes_applied?: false
    }
  end

  defp valid_receipt?(%{status: :passed, evidence: evidence}, criterion, now_ms)
       when is_map(evidence) do
    source = Map.get(evidence, :source)
    observed = Map.get(evidence, :observed_at_ms)
    id = Map.get(evidence, :id)
    revision = Map.get(evidence, :revision)
    digest = Map.get(evidence, :artifact_sha256)

    cond do
      source not in criterion.accepted_sources ->
        {:error, :wrong_evidence_source}

      not (is_binary(id) and Regex.match?(@id, id)) ->
        {:error, :invalid_evidence_id}

      not (is_binary(revision) and Regex.match?(@revision, revision)) ->
        {:error, :invalid_revision}

      not (is_binary(digest) and Regex.match?(@sha256, digest)) ->
        {:error, :invalid_artifact_fingerprint}

      Map.get(evidence, :synthetic?) != false ->
        {:error, :synthetic_or_unspecified_evidence}

      not (is_integer(observed) and observed > 0 and observed <= now_ms) ->
        {:error, :invalid_observation_time}

      now_ms - observed > @max_evidence_age_ms ->
        {:error, :stale_evidence}

      true ->
        :ok
    end
  end

  defp valid_receipt?(_, _, _), do: {:error, :not_a_passing_evidence_receipt}

  defp prepare(%{} = value) do
    evidence = get(value, :evidence, %{})

    %{
      criterion: pick_atom(get(value, :criterion), Enum.map(@criteria, & &1.id)),
      status: pick_atom(get(value, :status), [:passed]),
      evidence: %{
        id: get(evidence, :id),
        source: pick_atom(get(evidence, :source),
          [:ci, :runtime_probe, :controlled_experiment, :independent_review]),
        revision: get(evidence, :revision),
        artifact_sha256: get(evidence, :artifact_sha256),
        observed_at_ms: get(evidence, :observed_at_ms),
        synthetic?: get(evidence, :synthetic?)
      }
    }
  end

  defp prepare(_), do: %{}

  defp pick_atom(value, choices) when is_atom(value) do
    if value in choices, do: value, else: nil
  end

  defp pick_atom(value, choices) when is_binary(value) do
    Enum.find(choices, &(Atom.to_string(&1) == value))
  end

  defp pick_atom(_, _), do: nil

  defp get(map, key, default \\ nil)

  defp get(%{} = map, key, default) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp get(_, _, default), do: default

  defp rejected(reason) do
    %{
      v: @version, status: :rejected, reason: reason,
      ready_for_external_audit?: false,
      evidence_issuer_authenticated?: false,
      externally_verified?: false,
      release_approved?: false,
      consciousness_proven?: false,
      changes_applied?: false
    }
  end
end
