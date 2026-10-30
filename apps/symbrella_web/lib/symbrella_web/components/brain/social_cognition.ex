defmodule SymbrellaWeb.Components.Brain.SocialCognition do
  @moduledoc """
  Source-attributed Colony cognition results in the Brain dashboard.

  Extracted from the bounded Brain.Blackboard event window. A thread-level
  result includes lexical analysis and requested transient neuron stimulation.
  It is not proof of neuron firing, learning, or working-memory admission.
  """

  use SymbrellaWeb, :html

  @event [:core, :social, :cognitive_shadow]

  @doc "Newest read-only social cognition event, or nil."
  def latest(events) when is_list(events) do
    Enum.find_value(events, fn event ->
      payload = Map.get(event, :env, event)

      if is_map(payload) and Map.get(payload, :event) == @event and
           Map.get(payload, :kind) == :telemetry do
        measurements = Map.get(payload, :measurements, %{})
        meta = Map.get(payload, :meta, %{})

        if is_map(measurements) and is_map(meta) and
             Map.get(meta, :source) == :colony_external do
          %{
            at_ms: Map.get(payload, :at_ms),
            post_id: Map.get(meta, :post_id),
            relation: Map.get(meta, :reported_relation),
            analysis_scope: Map.get(meta, :analysis_scope, :unspecified),
            sentence_status: Map.get(meta, :sentence_status),
            provenance: Map.get(meta, :provenance),
            segments: bounded_count(measurements, :segments),
            neuron_stimuli_issued: bounded_count(measurements, :neuron_stimuli_issued),
            lifg_choices: bounded_count(measurements, :lifg_choices),
            atl_winners: bounded_count(measurements, :atl_winners),
            syntax_frames: bounded_count(measurements, :syntax_frames),
            dictionary_rows: bounded_count(measurements, :dictionary_rows),
            pos_covered_tokens: bounded_count(measurements, :pos_covered_tokens),
            processed_tokens: bounded_count(measurements, :processed_tokens),
            proposition_status: Map.get(meta, :proposition_status, :not_interpreted),
            proposition_reason: Map.get(meta, :proposition_reason),
            disputed_roles: bounded_count(measurements, :disputed_roles),
            grammar_outcome: Map.get(meta, :grammar_outcome, :not_recorded),
            ltm_status: Map.get(meta, :ltm_status, :not_recorded),
            ltm_reason: Map.get(meta, :ltm_reason)
          }
        end
      end
    end)
  end

  def latest(_), do: nil

  defp bounded_count(map, key) do
    case Map.get(map, key) do
      n when is_integer(n) and n >= 0 -> min(n, 10_000)
      _ -> 0
    end
  end

  attr :events, :list, default: []

  def panel(assigns) do
    assigns = assign(assigns, :social, latest(assigns.events))

    ~H"""
    <section data-brain-report-section="Read-only Colony Cognition" class="rounded-xl border border-zinc-200 p-4 shadow-sm dark:border-zinc-700">
      <div class="flex flex-wrap items-center justify-between gap-2">
        <h3 class="text-sm font-semibold">Colony cognition · read-only</h3>
        <span class="rounded bg-zinc-100 px-2 py-1 text-xs text-zinc-600 dark:bg-zinc-800 dark:text-zinc-300">
          External language · isolated memory · transient neuron requests
        </span>
      </div>
      <%= if @social do %>
        <dl class="mt-3 grid grid-cols-2 gap-x-4 gap-y-2 text-sm sm:grid-cols-3">
          <div><dt class="text-xs text-zinc-500">Lexical analysis scope</dt><dd class="font-mono break-all">{@social.analysis_scope}</dd></div>
          <div :if={@social.analysis_scope == :thread_aggregate}><dt class="text-xs text-zinc-500">Sentences inspected</dt><dd class="font-mono">{@social.segments}</dd></div>
          <div :if={@social.analysis_scope == :thread_aggregate}><dt class="text-xs text-zinc-500">Neuron stimuli requested</dt><dd class="font-mono">{@social.neuron_stimuli_issued}</dd></div>
          <div><dt class="text-xs text-zinc-500">LIFG selections</dt><dd class="font-mono">{@social.lifg_choices}</dd></div>
          <div><dt class="text-xs text-zinc-500">ATL winners</dt><dd class="font-mono">{@social.atl_winners}</dd></div>
          <div><dt class="text-xs text-zinc-500">Grammar frames</dt><dd class="font-mono">{@social.syntax_frames}</dd></div>
          <div><dt class="text-xs text-zinc-500">Dictionary rows</dt><dd class="font-mono">{@social.dictionary_rows}</dd></div>
          <div><dt class="text-xs text-zinc-500">Tokens with POS</dt><dd class="font-mono">{@social.pos_covered_tokens} / {@social.processed_tokens}</dd></div>
          <div :if={@social.analysis_scope != :thread_aggregate}><dt class="text-xs text-zinc-500">Grammar audit</dt><dd class="font-mono break-all">{@social.grammar_outcome}</dd></div>
          <div :if={@social.analysis_scope != :thread_aggregate}><dt class="text-xs text-zinc-500">Dictionary lookup</dt><dd class="font-mono break-all">{@social.ltm_status}</dd></div>
          <div :if={@social.ltm_reason}><dt class="text-xs text-zinc-500">Lookup error</dt><dd class="font-mono break-all">{@social.ltm_reason}</dd></div>
          <div><dt class="text-xs text-zinc-500">Sentence meaning</dt><dd class="font-mono break-all">{@social.sentence_status}</dd></div>
          <div :if={@social.analysis_scope != :thread_aggregate}><dt class="text-xs text-zinc-500">External proposition</dt><dd class="font-mono break-all">{@social.proposition_status}</dd></div>
          <div :if={@social.proposition_reason}><dt class="text-xs text-zinc-500">Proposition abstention</dt><dd class="font-mono break-all">{@social.proposition_reason}</dd></div>
          <div><dt class="text-xs text-zinc-500">Disputed grammatical roles</dt><dd class="font-mono">{@social.disputed_roles}</dd></div>
          <div :if={@social.analysis_scope != :thread_aggregate}><dt class="text-xs text-zinc-500">Reported relation</dt><dd class="font-mono break-all">{@social.relation}</dd></div>
          <div><dt class="text-xs text-zinc-500">Evidence status</dt><dd class="font-mono break-all">{@social.provenance}</dd></div>
        </dl>
        <p class="mt-3 text-xs text-zinc-500">
          Colony text is untrusted external input. Neuron-stimulus counts represent
          activation requests, not verified firing reports. No durable memory,
          working-memory admission, synaptic learning, or factual authority is implied.
        </p>
      <% else %>
        <p class="mt-3 text-sm text-zinc-500">
          No recent Colony analysis in the blackboard window.
          Select Analyze cognition under /social, then return to Brain.
        </p>
      <% end %>
    </section>
    """
  end
end
