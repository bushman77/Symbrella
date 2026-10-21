defmodule Core.Curiosity.Investigator do
  @moduledoc """
  Silent, supervised investigation of durable missing reasoning links.

  V1 handles only missing_reasoning_link / find_linking_premise issues.
  One eligible issue is revisited per permitted cycle. Hippocampus proposes
  opaque proof pointers or parent IDs; Core revalidates all relevant persisted
  proofs before Db.CognitiveIssues may mark the issue resolved. A new direct
  proof closes a stale missing link as a direct resolution, not as two-hop
  reasoning.

  The reasoning-only default does not invoke Qwen, network, chat/LiveView,
  PubSub, or generated prose. An independently opt-in acquisition mode may
  retrieve one dated public population comparison via fixed-source providers,
  then reauthorize and consolidate it before retrying reasoning. Failures
  leave uncertainty open and apply a bounded in-memory retry cooldown.
  """

  use GenServer

  alias Core.Cognition.{AssociativeReasoning, MemoryBoundary}
  alias Core.Curiosity.{EvidenceAcquisition, InvestigationHistory, SelectionPolicy, UnresolvedIssues}
  alias Core.Response.{AuthoritativeAnswerPlan, ResponseFrame}
  alias Db.{CognitiveIssue, CognitiveIssues, ConsolidatedConclusion, ConsolidatedConclusions}

  @default_interval_ms 300_000
  @default_retry_ms 3_600_000
  @max_candidates 20
  @max_attempts 32
  @telemetry [:core, :curiosity, :symbolic_investigation]

  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  @doc "Request one immediate, bounded investigation cycle. All feature gates still apply."
  @spec run_now(GenServer.server()) :: map()
  def run_now(server \\ __MODULE__), do: GenServer.call(server, :run_now, 30_000)

  @doc "Read-only runtime status; never includes subjects, evidence, or chat content."
  @spec snapshot(GenServer.server()) :: map()
  def snapshot(server \\ __MODULE__), do: GenServer.call(server, :snapshot)

  @impl true
  def init(opts) do
    state = %{opts: opts, last_result: %{status: :not_run},
      last_successful_result: nil, attempted: %{}, cycles: 0}
    schedule(interval_ms(opts))
    {:ok, state}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    {:reply,
     %{enabled?: enabled?(state.opts),
       acquisition_enabled?: enabled?(state.opts) and acquire_evidence?(state.opts),
       cycles: state.cycles,
       last_result: state.last_result,
       last_successful_result: state.last_successful_result,
       cooldown_count: map_size(state.attempted)},
     state}
  end

  @impl true
  def handle_call(:run_now, _from, state) do
    {result, next} = run_cycle(state)
    {:reply, result, next}
  end

  @impl true
  def handle_info(:tick, state) do
    {_result, next} = run_cycle(state)
    schedule(interval_ms(state.opts))
    {:noreply, next}
  end

  # Hippocampal recall telemetry may be echoed to the process that initiated
  # the recall. The investigator is a GenServer, so such messages must not
  # terminate its supervision cycle or mutate its investigation state.
  def handle_info({:telemetry, _event, _measurements, _metadata}, state),
    do: {:noreply, state}

  # Ignore unrecognized asynchronous messages safely. Never log arbitrary
  # message payloads: they may include internal cognitive or user data.
  def handle_info(_message, state), do: {:noreply, state}

  defp run_cycle(state) do
    now = System.monotonic_time(:millisecond)
    attempted =
      state.attempted
      |> Enum.reject(fn {_id, time} -> now - time >= retry_ms(state.opts) end)
      |> Map.new()

    result =
      cond do
        not enabled?(state.opts) ->
          %{status: :disabled}

        not features_enabled?() ->
          %{status: :disabled, reason: :reasoning_feature_disabled}

        true ->
          eligible =
            UnresolvedIssues.reasoning_candidates(@max_candidates,
              acquire_evidence?: acquire_evidence?(state.opts))
            |> Enum.filter(fn issue ->
              issue.kind == "missing_reasoning_link" and
                issue.next_action == "find_linking_premise" and
                issue.status in ["open", "investigating"] and
                is_integer(issue.issue_id) and
                not Map.has_key?(attempted, issue.issue_id)
            end)

          case eligible do
            [] -> %{status: :idle}
            _ -> select_and_investigate(eligible, state.opts)
          end
      end

    attempted =
      case Map.get(result, :issue_id) do
        id when is_integer(id) ->
          attempted
          |> Map.put(id, now)
          |> Enum.sort_by(fn {_id, time} -> time end, :desc)
          |> Enum.take(@max_attempts)
          |> Map.new()

        _ -> attempted
      end

    duration_ms = max(System.monotonic_time(:millisecond) - now, 0)

    :telemetry.execute(@telemetry, %{count: 1, duration_ms: duration_ms},
      %{status: result.status, reason: Map.get(result, :reason),
        issue_id: Map.get(result, :issue_id),
        history_id: Map.get(result, :history_id),
        parent_proof_ids: Map.get(result, :parent_proof_ids),
        acquired_proof_id: Map.get(result, :acquired_proof_id),
        acquisition: Map.get(result, :acquisition),
        acquisition_reason: Map.get(result, :acquisition_reason)})

    last_successful =
      if result.status == :resolved, do: result, else: state.last_successful_result

    next = %{state | last_result: result, last_successful_result: last_successful,
      attempted: attempted, cycles: state.cycles + 1}
    {result, next}
  end

  defp select_and_investigate(eligible, opts) do
    active? = self_model_policy_enabled?(opts)
    model = self_model_for_policy(opts)
    selection = SelectionPolicy.choose(eligible, model, active?: active?)

    legacy_id = selection.legacy_selected.issue_id
    shadow_id = selection.shadow_selected.issue_id

    policy_selection = %{
      mode: selection.mode,
      selected_strategy: selection.selected_strategy,
      legacy_strategy: selection.legacy_strategy,
      shadow_strategy: selection.shadow_strategy,
      selected_score: selection.selected_score,
      selected_learned_bias: selection.selected_learned_bias,
      legacy_issue_id: legacy_id,
      shadow_issue_id: shadow_id,
      selection_changed?: legacy_id != shadow_id
    }

    :telemetry.execute(
      [:core, :curiosity, :policy_comparison],
      %{count: 1, score: selection.score, learned_bias: selection.learned_bias,
        selected_score: selection.selected_score,
        selected_learned_bias: selection.selected_learned_bias},
      Map.merge(policy_selection, %{would_defer?: selection.would_defer?})
    )

    case selection do
      %{status: :deferred, reason: reason} ->
        # A deferral is not an issue attempt; do not write an audit row or
        # consume the issue's one-hour retry budget.
        %{status: :deferred, reason: reason, policy: :active,
          policy_selection: policy_selection}

      %{status: :ready, selected: %{issue_id: id}, mode: mode} ->
        investigate_issue_with_options(id, opts)
        |> Map.put(:policy, mode)
        |> Map.put(:policy_selection, policy_selection)
    end
  end

  defp self_model_policy_enabled?(opts) do
    cfg = Application.get_env(:core, __MODULE__, [])
    Keyword.get(opts, :self_model_policy?,
      Keyword.get(cfg, :self_model_policy?, false)) == true
  end

  defp self_model_for_policy(opts) do
    case Keyword.get(opts, :self_state) do
      %{} = state -> state
      _ -> Brain.Introspection.snapshot()
    end
  rescue
    _ -> %Brain.SelfModel{}
  catch
    :exit, _ -> %Brain.SelfModel{}
  end

  @doc """
  Revisit one persisted issue by ID. This never accepts caller-supplied facts,
  marks no issue resolved itself, and refuses to work unless enabled.
  Useful for testing the DB-backed cognitive boundary independently of timers.
  """
  @spec investigate_issue(integer()) :: map()
  def investigate_issue(id) when is_integer(id) and id > 0,
    do: investigate_issue_with_options(id, [])

  def investigate_issue(_), do: %{status: :skipped, reason: :invalid_issue_id}

  # A scheduled worker must honor its explicit per-process feature gate.
  # The public direct-call API still uses the global opt-in gate, but a
  # supervised named trial can remain enabled while the default worker is
  # disabled. This does not bypass the three independent reasoning gates.
  defp investigate_issue_with_options(id, opts) do
    if not enabled?(opts) or not features_enabled?() do
      %{status: :disabled, issue_id: id}
    else
      # Persist a start marker first. A crash leaves an inspectable
      # incomplete attempt rather than falsely reporting success.
      audit_id = InvestigationHistory.begin_attempt(id, opts)
      started = System.monotonic_time(:millisecond)

      result =
        try do
          case Db.get(CognitiveIssue, id) do
            %CognitiveIssue{} = issue -> investigate(issue, opts)
            _ -> %{status: :skipped, issue_id: id, reason: :issue_not_found}
          end
        rescue
          _ -> %{status: :failed, issue_id: id, reason: :investigation_unavailable}
        catch
          :exit, _ -> %{status: :failed, issue_id: id, reason: :investigation_unavailable}
        end

      duration_ms = max(System.monotonic_time(:millisecond) - started, 0)
      :ok = InvestigationHistory.complete_attempt(audit_id, result, duration_ms)
      emit_learning_receipt(audit_id, id)
      Map.put(result, :history_id, audit_id)
    end
  end

  # A receipt is credited only if the durable audit completion and resolved
  # CognitiveIssue agree. Publishing this telemetry cannot create a proof.
  defp emit_learning_receipt(audit_id, issue_id)
      when is_integer(audit_id) and audit_id > 0 do
    receipt =
      Db.CuriosityInvestigations.for_issue(issue_id, 1)
      |> Enum.find(&(Map.get(&1, :id) == audit_id and not is_nil(&1.completed_at)))

    if receipt do
      issue = Db.get(CognitiveIssue, issue_id)
      outcome = Brain.CuriosityLearning.classify(receipt, issue)
      projection = Brain.CuriosityLearning.snapshot()

      :telemetry.execute(
        [:core, :curiosity, :learning_outcome],
        %{count: 1, samples: projection.sample_count},
        %{history_id: audit_id, issue_id: issue_id, outcome: outcome,
          learning_version: projection.v}
      )
    end

    :ok
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end

  defp emit_learning_receipt(_, _), do: :ok

  defp investigate(issue, opts) do
    names = Map.get(issue.evidence || %{}, "subjects")

    with true <- issue.kind == "missing_reasoning_link" and
                   issue.next_action == "find_linking_premise" and
                   issue.status in ["open", "investigating"],
         true <- is_integer(issue.reference_year),
         true <- valid_names?(names),
         true <- issue.subject_key == subject_key(names) do
      # Curiosity supplies only an internal typed question. No LLM, network
      # retrieval or user-facing messages are involved.
      context = internal_context(issue.reference_year, names)

      result =
        case AssociativeReasoning.propose(names) do
          %{kind: :two_hop_candidate, parent_proof_ids: ids} when is_list(ids) ->
            reconcile_two_hop(issue, names, context, ids)

          %{kind: :direct_proof_pointer, fingerprint: fingerprint}
              when is_binary(fingerprint) ->
            reconcile_direct(issue, names, context, fingerprint)

          _ ->
            reconcile_without_cue(issue, names, context)
        end

      maybe_acquire_missing_bridge(issue, result, opts)
    else
      _ -> %{status: :skipped, issue_id: issue.id, reason: :invalid_or_stale_issue}
    end
  end

  # External evidence requires a separate explicit opt-in; it never runs
  # merely because the reasoning-only investigator has been enabled.
  defp maybe_acquire_missing_bridge(issue, %{status: :unresolved,
      reason: :no_two_hop_cue} = unresolved, opts) do
    if acquire_evidence?(opts) do
      case EvidenceAcquisition.acquire(issue) do
        %{status: :captured, proof_id: id} = acquired ->
          # The capture indexes a source-backed proof, but it cannot declare
          # the old A-C issue resolved. Re-enter the existing reasoning
          # boundary once, without recursive evidence acquisition.
          after_capture = investigate(issue, Keyword.put(opts, :acquire_evidence?, false))
          Map.merge(after_capture, %{
            acquisition: :captured,
            acquired_proof_id: id,
            provider_attempts: Map.get(acquired, :provider_attempts, [])
          })

        %{status: :unresolved, reason: reason} = acquired ->
          Map.merge(unresolved, %{
            acquisition: :not_captured,
            acquisition_reason: reason,
            provider_attempts: Map.get(acquired, :provider_attempts, [])
          })
      end
    else
      unresolved
    end
  end

  defp maybe_acquire_missing_bridge(_issue, result, _opts), do: result

  defp acquire_evidence?(opts) do
    config = Application.get_env(:core, __MODULE__, [])
    Keyword.get(opts, :acquire_evidence?,
      Keyword.get(config, :acquire_evidence?, false)) == true
  end

  defp internal_context(year, names) do
    frame = %{
      type: :location_question, domain: :geography,
      relation: :population, comparison: :greater,
      subjects: names, grounding_status: :unverified
    }

    %{
      session_id: "internal-curiosity",
      user_text: "Which place had the larger population in #{year}?",
      features: %{intent: :question, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}, mood: %{}
    }
  end

  defp reconcile_two_hop(issue, names, context, ids) do
    if length(ids) == 2 and Enum.uniq(ids) == ids and
         Enum.all?(ids, &is_integer/1) do
      recalled = MemoryBoundary.recall(context)
      composition = Map.get(recalled, :composition)
      support = get_in(recalled, [:web_evidence, :support])
      current = Db.get(CognitiveIssue, issue.id)

      # PopulationChain revalidates ALL persisted candidates, then resolves
      # the exact issue through Db.CognitiveIssues.resolve_reasoning_link/3.
      if match?(%{origin: :hippocampal_two_hop_cue,
                  rule: :transitive_population_order}, composition) and
           composition.parent_proof_ids == ids and
           composition.reference_year == issue.reference_year and
           match?(%{kind: :population_comparison, status: :source_supported}, support) and
           support.year == issue.reference_year and
           support.subjects == names and
           get_in(recalled, [:web_evidence, :evidence_origin]) == :composed_symbolic_premises and
           match?(%CognitiveIssue{status: "resolved"}, current) and
           is_map(current.resolution) and
           current.resolution["reasoning_rule"] == "transitive_population_order" and
           current.resolution["reference_year"] == issue.reference_year and
           current.resolution["parent_proof_ids"] == ids do
        %{status: :resolved, issue_id: issue.id, reference_year: issue.reference_year,
          parent_proof_ids: ids, origin: :hippocampal_two_hop_cue}
      else
        %{status: :unresolved, issue_id: issue.id, reason: :proof_not_authorized}
      end
    else
      %{status: :unresolved, issue_id: issue.id, reason: :invalid_two_hop_candidate}
    end
  end

  defp reconcile_without_cue(issue, names, context) do
    # Preserve a meaningful missing-cue diagnostic when there is no direct
    # stored proof. A durable direct comparison may still be authoritative
    # even after the hippocampal pointer has aged out of its window.
    case ConsolidatedConclusions.recent(issue.subject_key) do
      [] ->
        %{status: :unresolved, issue_id: issue.id, reason: :no_two_hop_cue}

      _ ->
        reconcile_direct(issue, names, context, nil)
    end
  end

  defp reconcile_direct(issue, names, context, cued_fingerprint) do
    # A hippocampal pointer is optional, but never sufficient. Core must
    # independently rehydrate the persisted claims, check for competing
    # direct proofs, and authorize the dated comparison.
    #
    # When the cue index has forgotten a proof, use the durable DB as the
    # recovery path. Report the fallback provenance explicitly rather than
    # claiming hippocampal guidance that did not occur.
    recalled = MemoryBoundary.recall(context)
    support = get_in(recalled, [:web_evidence, :support])
    stored = Map.get(recalled, :consolidation)

    with %{kind: :population_comparison, id: proof_id,
           origin: :stored_symbolic_premises, reference_year: year} <- stored,
         true <- year == issue.reference_year,
         %{kind: :population_comparison, status: :source_supported,
           year: ^year, subjects: ^names} <- support,
         true <- get_in(recalled, [:web_evidence, :evidence_origin]) ==
                   :consolidated_symbolic_memory,
         %ConsolidatedConclusion{
           fingerprint: fingerprint,
           pair_key: pair_key,
           reference_year: ^year,
           kind: "population_comparison",
           provenance: "source_supported_symbolic_derivation"
         } <- Db.get(ConsolidatedConclusion, proof_id),
         true <- pair_key == issue.subject_key,
         true <- valid_direct_pointer?(cued_fingerprint, fingerprint, recalled),
         framed <- Map.put(recalled, :response_frame, ResponseFrame.build(recalled)),
         %{kind: :verified_population_comparison} = plan <-
           AuthoritativeAnswerPlan.build(framed),
         true <- AuthoritativeAnswerPlan.authorized?(
           plan, AuthoritativeAnswerPlan.render(plan, :brief), framed
         ),
         :ok <- CognitiveIssues.resolve_direct_reasoning_link(
           issue.id, proof_id, fingerprint, year
         ),
         %CognitiveIssue{status: "resolved", resolution: resolution} <-
           Db.get(CognitiveIssue, issue.id),
         true <- is_map(resolution) and
                   resolution["reasoning_rule"] == "direct_population_comparison" and
                   resolution["origin"] == "stored_symbolic_premises" and
                   resolution["proof_id"] == proof_id and
                   resolution["proof_fingerprint"] == fingerprint and
                   resolution["reference_year"] == year do
      origin =
        if is_binary(cued_fingerprint),
          do: :verified_direct_proof,
          else: :verified_direct_db_fallback

      %{status: :resolved, issue_id: issue.id, reference_year: year,
        proof_id: proof_id, origin: origin}
    else
      _ ->
        %{status: :unresolved, issue_id: issue.id,
          reason: :direct_proof_not_authorized}
    end
  end

  defp valid_direct_pointer?(nil, _fingerprint, _recalled), do: true

  defp valid_direct_pointer?(fingerprint, fingerprint, recalled) do
    match?(%{kind: :direct_proof_pointer, fingerprint: ^fingerprint},
      Map.get(recalled, :hippocampal_association))
  end

  defp valid_direct_pointer?(_, _, _), do: false

  defp features_enabled? do
    Enum.all?(
      [:symbolic_consolidation_enabled?, :symbolic_composition_enabled?,
       :semantic_associative_recall_enabled?],
      &(Application.get_env(:core, &1, false) == true)
    )
  end

  defp enabled?(opts) do
    config = Application.get_env(:core, __MODULE__, [])
    Keyword.get(opts, :enabled?, Keyword.get(config, :enabled?, false)) == true
  end

  defp interval_ms(opts) do
    config = Application.get_env(:core, __MODULE__, [])
    value = Keyword.get(opts, :interval_ms, Keyword.get(config, :interval_ms, @default_interval_ms))
    if is_integer(value), do: max(60_000, value), else: @default_interval_ms
  end

  defp retry_ms(opts) do
    config = Application.get_env(:core, __MODULE__, [])
    value = Keyword.get(opts, :retry_ms, Keyword.get(config, :retry_ms, @default_retry_ms))
    if is_integer(value), do: max(60_000, value), else: @default_retry_ms
  end

  defp schedule(ms), do: Process.send_after(self(), :tick, ms)

  defp valid_names?([a, b]) when is_binary(a) and is_binary(b) do
    byte_size(a) in 3..100 and byte_size(b) in 3..100 and
      String.trim(a) != "" and String.trim(b) != "" and
      String.downcase(String.trim(a)) != String.downcase(String.trim(b))
  end

  defp valid_names?(_), do: false

  defp subject_key(names) do
    names
    |> Enum.map(&(String.downcase(String.trim(&1))))
    |> Enum.sort()
    |> Enum.join("|")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end
end
