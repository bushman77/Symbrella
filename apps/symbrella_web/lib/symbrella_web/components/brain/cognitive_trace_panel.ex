defmodule SymbrellaWeb.Components.Brain.CognitiveTracePanel do
  @moduledoc """
  Inspect correlated execution spans without exposing conversation text.

  Completed stage invocations prove code ran and returned. They do not prove
  that the stage produced a correct interpretation or that signals traversed
  a conceptual neural edge.
  """

  use SymbrellaWeb, :html

  @labels %{
    turn: "Chat turn",
    chat_route: "Chat routing",
    phase_1_input: "1 · Input preparation",
    phase_2_context: "2 · Context and WM recall",
    phase_3_lexical: "3 · Lexical candidates",
    phase_4_grammar: "4 · Syntax and lexical competition",
    lifg_selection: "LIFG · Lexical selection",
    atl_integration: "ATL · Semantic integration",
    phase_5_semantics: "5 · Semantic composition",
    phase_6_comprehension: "6 · Comprehension and speech acts",
    comprehension_summary: "Comprehension evidence summary",
    speech_act_binding: "Social speech-act binding",
    phase_7_planning: "7 · WM and response planning",
    prefrontal_control: "Prefrontal · Planner computation",
    action_selection: "Executive · Live and shadow selection",
    response_planning: "Executive · Final planned decision",
    phase_8_memory: "8 · Experience and memory",
    hippocampus_persist: "Hippocampus · Persistence decision"
  }

  @doc "Return a human-readable stage label without executing user data."
  def label(stage), do: Map.get(@labels, stage, "Unrecognized stage")

  @doc "Display only predetermined, bounded evidence keys."
  def evidence_label(%{evidence: evidence}) when is_map(evidence) do
    evidence
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(fn
      {:degraded, value} -> "degraded summary: #{value}"
      {:understood, value} -> "understood: #{value}"
      {:uncertain, value} -> "uncertain: #{value}"
      {:unresolved, value} -> "unresolved: #{value}"
      {:choices, value} -> "LIFG choices: #{value}"
      {:slate_present, value} -> "ATL slate present: #{value}"
      {:speech_act, value} -> "speech act: #{value}"
      {:social_components, [:greeting, :social_update_query]} ->
        "social components: greeting + social_update_query"
      {:social_components, [:social_update_query]} ->
        "social components: social_update_query"
      {:available, value} -> "prefrontal available: #{value}"
      {:planners, value} when is_list(value) -> "planner receipts: #{length(value)}"
      {:controls, value} when is_map(value) -> "merged controls: #{map_size(value)}"
      {:feedback_quality, value} -> "feedback: #{value}"
      {:shadow_status, value} -> "shadow: #{value}"
      {:live_action, value} -> "live: #{value}"
      {:shadow_action, value} -> "shadow proposal: #{value}"
      {:changed, value} -> "changed proposal: #{value}"
      {:applied_keys, value} when is_list(value) ->
        "shadow-applied controls: #{length(value)}"
      _ -> ""
    end)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" · ")
  end

  def evidence_label(_), do: ""

  @diagnostic_reasons [
    :no_lifg_choices, :weak_decision_rate_high, :fallback_rate_high,
    :unresolved_content_tokens
  ]
  @diagnostic_fields [
    :reasons, :weak_rate, :fallback_rate, :structural_status,
    :sentence_meaning_status, :integration_status, :feedback_mode,
    :feedback_applied, :repair_hint, :curiosity_gap_gate
  ]

  @doc "Only a complete, successful diagnosis from the selected trace is displayed."
  def diagnosis(%{events: events}) when is_list(events) do
    events
    |> Enum.reverse()
    |> Enum.find_value(fn
      %{stage: :comprehension_summary, status: :ok, evidence: %{} = evidence} ->
        reasons = Map.get(evidence, :reasons)

        if is_list(reasons) and length(reasons) <= 4 and
             Enum.all?(reasons, &(&1 in @diagnostic_reasons)) and
             Enum.all?(@diagnostic_fields, &Map.has_key?(evidence, &1)),
          do: evidence, else: nil

      _ -> nil
    end)
  end

  def diagnosis(_), do: nil

  defp reason_label(:no_lifg_choices), do: "No LIFG selections"
  defp reason_label(:weak_decision_rate_high), do: "High weak-selection rate"
  defp reason_label(:fallback_rate_high), do: "High fallback-selection rate"
  defp reason_label(:unresolved_content_tokens), do: "Unresolved content tokens"
  defp reason_label(_), do: "Other (unrecognized) reason"

  defp review_label(:review_missing_choices), do: "Review missing lexical candidates"
  defp review_label(:review_unresolved_content), do: "Review unresolved content evidence"
  defp review_label(:review_lexical_evidence), do: "Review weak or fallback lexical evidence"
  defp review_label(:not_indicated), do: "No degradation-driven review indicated"
  defp review_label(_), do: "Review cause not established"

  defp gap_label(:eligible_for_review), do: "Current-turn review prerequisites present"
  defp gap_label(:needs_unresolved_content), do: "Requires unresolved content"
  defp gap_label(:needs_uncertainty), do: "Requires uncertain content"
  defp gap_label(:needs_session), do: "Requires session context"
  defp gap_label(:needs_user_origin), do: "Requires user-origin evidence"
  defp gap_label(:needs_input), do: "Requires nonempty input"
  defp gap_label(:not_degraded), do: "No degraded-comprehension trigger"
  defp gap_label(_), do: "Prerequisites not observed"

  defp rate_label(n) when is_number(n) and n >= 0 and n <= 1,
    do: "#{Float.round(n * 100.0, 1)}%"
  defp rate_label(_), do: "not observed"

  defp count_label(n) when is_integer(n), do: Integer.to_string(n)
  defp count_label(_), do: "not observed"

  attr :trace, :any, default: nil
  attr :traces, :list, default: []

  def panel(assigns) do
    steps =
      case assigns.trace do
        %{events: events} when is_list(events) ->
          Enum.reject(events, &(&1.status == :started))

        _ -> []
      end

    assigns =
      assigns
      |> assign(:steps, steps)
      |> assign(:diagnosis, diagnosis(assigns.trace))

    ~H"""
    <section id="atlas-cognitive-trace" class="mt-5 rounded-xl border border-emerald-800/60 bg-zinc-900/75 p-4 sm:p-5"
      aria-label="Instrumented cognitive turn execution">
      <h3 class="text-lg font-semibold text-emerald-200">Cognitive Trace v1</h3>
      <p class="mt-1 text-sm leading-relaxed text-zinc-300">
        Timed, instrumented function calls in actual chat worker turns.
        Durations include nested operations. An executed stage does not establish
        successful comprehension; degraded summaries remain explicitly visible.
      </p>

      <div :if={@traces != []} class="mt-4 flex flex-wrap items-center gap-2">
        <span class="text-sm text-zinc-300">Observed turns:</span>
        <button :for={item <- @traces} type="button"
          id={"atlas-trace-#{item.trace_id}"} phx-click="atlas-trace"
          phx-value-id={item.trace_id}
          aria-pressed={to_string(@trace != nil and @trace.trace_id == item.trace_id)}
          class={[
            "min-h-11 rounded-lg border px-3 py-2 font-mono text-xs focus:outline-none focus:ring-2 focus:ring-emerald-300",
            @trace != nil and @trace.trace_id == item.trace_id && "border-emerald-300 bg-emerald-950 text-emerald-100",
            (@trace == nil or @trace.trace_id != item.trace_id) && "border-zinc-700 text-zinc-300"
          ]}>
          {String.slice(item.trace_id, -8, 8)} · {item.status}
        </button>
      </div>

      <p :if={@trace == nil} id="atlas-trace-empty" class="mt-4 text-sm text-zinc-400">
        No instrumented chat turn observed in the recent bounded window.
        Send a message in Symbrella Chat, then inspect this panel.
      </p>

      <div :if={@trace != nil} class="mt-4">
        <div class="flex flex-wrap items-center justify-between gap-3">
          <span class="font-mono text-xs text-cyan-200">Trace {@trace.trace_id}</span>
          <span class="rounded-full border border-emerald-800 px-3 py-1 text-xs text-emerald-200">
            Turn status: {@trace.status}
          </span>
        </div>
        <ol id="atlas-trace-steps" class="mt-4 space-y-2">
          <li :for={event <- @steps} class="rounded-lg border border-zinc-700 bg-zinc-950/70 p-3"
            data-stage={event.stage} data-status={event.status}>
            <div class="flex flex-wrap items-center justify-between gap-2">
              <strong class="text-sm text-zinc-100">{label(event.stage)}</strong>
              <span class={[
                "font-mono text-xs",
                event.status == :error && "text-rose-300",
                event.status != :error && "text-emerald-300"
              ]}>
                {event.status} · {Float.round(event.duration_us / 1000, 2)} ms
              </span>
            </div>
            <p :if={event.region != nil} class="mt-1 text-xs text-cyan-300">
              Instrumented region: {event.region}
            </p>
            <p :if={evidence_label(event) != ""} class="mt-1 text-xs text-amber-200">
              {evidence_label(event)}
            </p>
          </li>
        </ol>
      </div>

      <section :if={is_map(@diagnosis)} id="atlas-comprehension-diagnosis"
        class="mt-5 rounded-xl border border-cyan-800/70 bg-zinc-950/80 p-4"
        aria-label="Comprehension diagnosis for selected turn">
        <h4 class="text-base font-semibold text-cyan-200">
          Comprehension diagnosis · selected turn
        </h4>
        <p class="mt-2 text-xs leading-relaxed text-zinc-300">
          This report separates weak lexical evidence from structural interpretation
          and independently verified lexical constructions. No diagnosis alone
          certifies full sentence understanding or authorizes another interpretation.
        </p>
        <div class="mt-3">
          <strong class="text-xs text-zinc-300">Recorded degradation reasons</strong>
          <ul id="atlas-comprehension-reasons" class="mt-1 flex flex-wrap gap-2 text-xs">
            <li :for={reason <- @diagnosis.reasons} data-degradation-reason={reason}
              class="rounded-lg border border-amber-800/70 px-2 py-1 text-amber-200">
              {reason_label(reason)}
            </li>
            <li :if={@diagnosis.reasons == []} class="text-zinc-400">
              No recorded degradation reason
            </li>
          </ul>
        </div>
        <dl class="mt-4 grid gap-3 text-sm sm:grid-cols-2">
          <div>
            <dt class="text-xs text-zinc-400">LIFG selections</dt>
            <dd data-diagnosis-field="choice-count">
              {count_label(Map.get(@diagnosis, :choice_count))}
            </dd>
          </div>
          <div>
            <dt class="text-xs text-zinc-400">Independently verified lexical constructions</dt>
            <dd data-diagnosis-field="verified-lexical-count">
              {count_label(Map.get(@diagnosis, :verified_lexical_count))}
            </dd>
          </div>
          <div>
            <dt class="text-xs text-zinc-400">Weak selection rate</dt>
            <dd data-diagnosis-field="weak-rate">{rate_label(@diagnosis.weak_rate)}</dd>
          </div>
          <div>
            <dt class="text-xs text-zinc-400">Fallback selection rate</dt>
            <dd data-diagnosis-field="fallback-rate">{rate_label(@diagnosis.fallback_rate)}</dd>
          </div>
          <div>
            <dt class="text-xs text-zinc-400">Unresolved content token count</dt>
            <dd>{count_label(Map.get(@diagnosis, :unresolved_content_count))}</dd>
          </div>
          <div>
            <dt class="text-xs text-zinc-400">Structural competition</dt>
            <dd data-diagnosis-field="structural-status">{@diagnosis.structural_status}</dd>
          </div>
          <div>
            <dt class="text-xs text-zinc-400">Sentence meaning</dt>
            <dd data-diagnosis-field="sentence-meaning-status">
              {@diagnosis.sentence_meaning_status}
            </dd>
          </div>
          <div>
            <dt class="text-xs text-zinc-400">Cross-stage integration audit</dt>
            <dd data-diagnosis-field="integration-status">{@diagnosis.integration_status}</dd>
          </div>
        </dl>
        <div class="mt-4 space-y-2 border-t border-zinc-800 pt-3 text-xs">
          <p data-diagnosis-field="feedback">
            ATL-to-LIFG feedback: {@diagnosis.feedback_mode} ·
            revision applied: {to_string(@diagnosis.feedback_applied)}
          </p>
          <p data-diagnosis-field="repair-hint" class="text-cyan-200">
            Diagnostic review: {review_label(@diagnosis.repair_hint)}
          </p>
          <p data-diagnosis-field="curiosity-gap-gate" class="text-amber-200">
            Current-turn curiosity-gap gate: {gap_label(@diagnosis.curiosity_gap_gate)}
          </p>
          <p class="text-zinc-400">
            Review eligibility is not a repair command. Existing bounded feedback
            may already have run earlier in the pipeline; this panel does not retry
            LIFG, change lexical winners, ask the user a question, or write memory.
          </p>
        </div>
      </section>
      <p :if={@trace != nil and is_nil(@diagnosis)} id="atlas-comprehension-diagnosis-empty"
        class="mt-4 text-xs text-zinc-400">
        No detailed comprehension diagnosis observed for the selected turn.
        Earlier traces may contain counts without cause codes.
      </p>

      <p class="mt-4 text-xs text-zinc-400">
        Recent trace metadata expires after ten minutes and is never stored in the database.
        No raw message text, selected senses or memory payloads are retained.
      </p>
    </section>
    """
  end
end
