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
    phase_8_memory: "8 · Experience and memory",
    hippocampus_persist: "Hippocampus · Qualified persistence"
  }

  @doc "Return a human-readable stage label without executing user data."
  def label(stage), do: Map.get(@labels, stage, "Unrecognized stage")

  @doc "Display only predetermined, bounded evidence keys."
  def evidence_label(%{evidence: evidence}) when is_map(evidence) do
    evidence
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map_join(" · ", fn
      {:degraded, value} -> "degraded summary: #{value}"
      {:understood, value} -> "understood: #{value}"
      {:uncertain, value} -> "uncertain: #{value}"
      {:unresolved, value} -> "unresolved: #{value}"
      {:choices, value} -> "LIFG choices: #{value}"
      {:slate_present, value} -> "ATL slate present: #{value}"
      {:speech_act, value} -> "speech act: #{value}"
      _ -> ""
    end)
    |> String.trim(" ·")
  end

  def evidence_label(_), do: ""

  attr :trace, :any, default: nil
  attr :traces, :list, default: []

  def panel(assigns) do
    steps =
      case assigns.trace do
        %{events: events} when is_list(events) ->
          Enum.reject(events, &(&1.status == :started))

        _ -> []
      end

    assigns = assign(assigns, :steps, steps)

    ~H"""
    <section id="atlas-cognitive-trace" class="mt-5 rounded-xl border border-emerald-800/60 bg-zinc-900/75 p-4 sm:p-5"
      aria-label="Instrumented cognitive turn execution">
      <h3 class="text-lg font-semibold text-emerald-200">Cognitive Trace v1</h3>
      <p class="mt-1 text-sm leading-relaxed text-zinc-300">
        Verified function entry/return boundaries for chat worker turns.
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

      <p class="mt-4 text-xs text-zinc-400">
        Recent trace metadata expires after ten minutes and is never stored in the database.
        No raw message text, selected senses or memory payloads are retained.
      </p>
    </section>
    """
  end
end
