defmodule Core.Curiosity.Investigator do
  @moduledoc """
  Silent, supervised investigation of durable missing reasoning links.

  V1 handles only missing_reasoning_link / find_linking_premise issues.
  One eligible issue is revisited per permitted cycle. Hippocampus proposes
  opaque parent IDs; Core revalidates all relevant persisted proofs before
  Db.CognitiveIssues may mark the issue resolved.

  No Qwen, network, chat/LiveView, PubSub, generated prose, or new fact
  writes are permitted here. An unsuccessful investigation leaves the issue
  open and applies a bounded in-memory retry cooldown.
  """

  use GenServer

  alias Core.Cognition.{AssociativeReasoning, MemoryBoundary}
  alias Core.Curiosity.UnresolvedIssues
  alias Core.Response.{AuthoritativeAnswerPlan, ResponseFrame}
  alias Db.{CognitiveIssue, CognitiveIssues}

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
    state = %{opts: opts, last_result: %{status: :not_run}, attempted: %{}, cycles: 0}
    schedule(interval_ms(opts))
    {:ok, state}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    {:reply,
     %{enabled?: enabled?(state.opts), cycles: state.cycles,
       last_result: state.last_result, cooldown_count: map_size(state.attempted)},
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
          UnresolvedIssues.reasoning_candidates(@max_candidates)
          |> Enum.find(fn issue ->
            issue.kind == "missing_reasoning_link" and
              issue.next_action == "find_linking_premise" and
              issue.status in ["open", "investigating"] and
              is_integer(issue.issue_id) and
              not Map.has_key?(attempted, issue.issue_id)
          end)
          |> case do
            nil -> %{status: :idle}
            %{issue_id: id} -> investigate_issue(id)
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

    :telemetry.execute(@telemetry, %{count: 1},
      %{status: result.status, reason: Map.get(result, :reason),
        issue_id: Map.get(result, :issue_id),
        parent_proof_ids: Map.get(result, :parent_proof_ids)})

    next = %{state | last_result: result, attempted: attempted, cycles: state.cycles + 1}
    {result, next}
  end

  @doc """
  Revisit one persisted issue by ID. This never accepts caller-supplied facts,
  marks no issue resolved itself, and refuses to work unless enabled.
  Useful for testing the DB-backed cognitive boundary independently of timers.
  """
  @spec investigate_issue(integer()) :: map()
  def investigate_issue(id) when is_integer(id) and id > 0 do
    cond do
      not enabled?([]) or not features_enabled?() ->
        %{status: :disabled, issue_id: id}

      true ->
        case Db.get(CognitiveIssue, id) do
          %CognitiveIssue{} = issue -> investigate(issue)
          _ -> %{status: :skipped, issue_id: id, reason: :issue_not_found}
        end
    end
  rescue
    _ -> %{status: :failed, issue_id: id, reason: :investigation_unavailable}
  catch
    :exit, _ -> %{status: :failed, issue_id: id, reason: :investigation_unavailable}
  end

  def investigate_issue(_), do: %{status: :skipped, reason: :invalid_issue_id}

  defp investigate(issue) do
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

      case AssociativeReasoning.propose(names) do
        %{kind: :two_hop_candidate, parent_proof_ids: ids} when is_list(ids) ->
          reconcile_two_hop(issue, names, context, ids)

        %{kind: :direct_proof_pointer, fingerprint: fingerprint}
            when is_binary(fingerprint) ->
          # A directly answered question is not a transitive inference.
          # This can happen when a source-supported comparison arrives
          # after a missing-link issue was first opened.
          reconcile_direct(issue, names, context, fingerprint)

        _ ->
          %{status: :unresolved, issue_id: issue.id, reason: :no_two_hop_cue}
      end
    else
      _ -> %{status: :skipped, issue_id: issue.id, reason: :invalid_or_stale_issue}
    end
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

  defp reconcile_direct(issue, names, context, fingerprint) do
    # Consolidation.recall independently rehydrates the ORIGINAL two source
    # claims, checks for competing same-year proofs, re-derives the result
    # and requires an authoritative population comparison plan.
    recalled = MemoryBoundary.recall(context)
    support = get_in(recalled, [:web_evidence, :support])
    stored = Map.get(recalled, :consolidation)

    with %{kind: :direct_proof_pointer, fingerprint: ^fingerprint} <-
           Map.get(recalled, :hippocampal_association),
         %{kind: :population_comparison, id: proof_id,
           origin: :stored_symbolic_premises, reference_year: year} <- stored,
         true <- year == issue.reference_year,
         %{kind: :population_comparison, status: :source_supported,
           year: ^year, subjects: ^names} <- support,
         true <- get_in(recalled, [:web_evidence, :evidence_origin]) ==
                   :consolidated_symbolic_memory,
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
      %{status: :resolved, issue_id: issue.id, reference_year: year,
        proof_id: proof_id, origin: :verified_direct_proof}
    else
      _ ->
        %{status: :unresolved, issue_id: issue.id,
          reason: :direct_proof_not_authorized}
    end
  end

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
