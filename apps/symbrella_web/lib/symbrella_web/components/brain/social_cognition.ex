defmodule SymbrellaWeb.Components.Brain.SocialCognition do
  @moduledoc """
  Read-only Colony shadow results in the Brain dashboard.

  Extracted from the already bounded Brain.Blackboard event window. These
  counts report pure LIFG/ATL analysis, NOT active BrainCells, neural pulses,
  live GenServer state changes, fact verification, or working-memory writes.
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
            sentence_status: Map.get(meta, :sentence_status),
            provenance: Map.get(meta, :provenance),
            lifg_choices: bounded_count(measurements, :lifg_choices),
            atl_winners: bounded_count(measurements, :atl_winners),
            syntax_frames: bounded_count(measurements, :syntax_frames)
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
          Shadow analysis, not neuron activation
        </span>
      </div>
      <%= if @social do %>
        <dl class="mt-3 grid grid-cols-2 gap-x-4 gap-y-2 text-sm sm:grid-cols-3">
          <div><dt class="text-xs text-zinc-500">LIFG selections</dt><dd class="font-mono">{@social.lifg_choices}</dd></div>
          <div><dt class="text-xs text-zinc-500">ATL winners</dt><dd class="font-mono">{@social.atl_winners}</dd></div>
          <div><dt class="text-xs text-zinc-500">Grammar frames</dt><dd class="font-mono">{@social.syntax_frames}</dd></div>
          <div><dt class="text-xs text-zinc-500">Sentence meaning</dt><dd class="font-mono break-all">{@social.sentence_status}</dd></div>
          <div><dt class="text-xs text-zinc-500">Reported relation</dt><dd class="font-mono break-all">{@social.relation}</dd></div>
          <div><dt class="text-xs text-zinc-500">Evidence status</dt><dd class="font-mono break-all">{@social.provenance}</dd></div>
        </dl>
        <p class="mt-3 text-xs text-zinc-500">
          These are temporary lexical and structural computations on an untrusted external report.
          No firing neurons, working-memory admission, or verified claim is implied.
        </p>
      <% else %>
        <p class="mt-3 text-sm text-zinc-500">
          No Colony cognitive shadow event in the recent blackboard window.
          Run a new read-only draft and reload or refresh the Brain dashboard.
        </p>
      <% end %>
    </section>
    """
  end
end
