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
  alias Db.CognitiveIssue

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
         true <- issue.subject_key == subject_key(names),
         %{kind: :two_hop_candidate, parent_proof_ids: ids} <-
           AssociativeReasoning.propose(names),
         true <- length(ids) == 2 and Enum.uniq(ids) == ids do
      frame = %{
        type: :location_question, domain: :geography,
        relation: :population, comparison: :greater,
        subjects: names, grounding_status: :unverified
      }

      # Synthetic internal request, not a user message. No retrieval or
      # generation is executed: MemoryBoundary only calls the semantic store.
      context = %{
        session_id: "internal-curiosity",
        user_text: "Which place had the larger population in #{issue.reference_year}?",
        features: %{intent: :question, symbolic_frame: frame},
        symbolic_frame: %{event: frame},
        decision: %{action: :answer}, mood: %{}
      }

      recalled = MemoryBoundary.recall(context)
      composition = Map.get(recalled, :composition)
      support = get_in(recalled, [:web_evidence, :support])
      current = Db.get(CognitiveIssue, issue.id)

      # The existing PopulationChain has already revalidated the full graph,
      # derived and authorized the comparison, and resolved the exact
      # persisted issue through Db.CognitiveIssues.resolve_reasoning_link/3.
      if match?(%{origin: :hippocampal_two_hop_cue,
                  rule: :transitive_population_order}, composition) and
           composition.parent_proof_ids == ids and
           composition.reference_year == issue.reference_year and
           match?(%{kind: :population_comparison, status: :source_supported}, support) and
           support.year == issue.reference_year and
           support.subjects == names and
           get_in(recalled, [:web_evidence, :evidence_origin]) == :composed_symbolic_premises and
           match?(%CognitiveIssue{status: "resolved"}, current) do
        %{status: :resolved, issue_id: issue.id, reference_year: issue.reference_year,
          parent_proof_ids: ids, origin: :hippocampal_two_hop_cue}
      else
        %{status: :unresolved, issue_id: issue.id, reason: :proof_not_authorized}
      end
    else
      %{kind: :direct_proof_pointer} ->
        %{status: :unresolved, issue_id: issue.id, reason: :no_two_hop_cue}

      nil ->
        %{status: :unresolved, issue_id: issue.id, reason: :no_two_hop_cue}

      false ->
        %{status: :skipped, issue_id: issue.id, reason: :invalid_or_stale_issue}

      _ ->
        %{status: :unresolved, issue_id: issue.id, reason: :no_two_hop_cue}
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
